package chipyard.socgen.pool

import chisel3._
import chisel3.util._
import chipyard.socgen.link.AutoEndpointAsyncLink
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.SystemBusKey
import freechips.rocketchip.tile.{BuildRoCC, LazyRoCC, LazyRoCCModuleImp, OpcodeSet}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{AsyncQueueParams, FromAsyncBundle, ToAsyncBundle}
import org.chipsalliance.cde.config.{Config, Parameters}
import org.chipsalliance.diplomacy.lazymodule.LazyModule

class PoolAccelerator(opcodes: OpcodeSet, params: PoolParams)(implicit p: Parameters)
    extends LazyRoCC(opcodes) {
  val engine = LazyModule(new PoolEngine(params))
  private val dmaNode = TLIdentityNode()
  dmaNode := TLWidthWidget(p(SystemBusKey).beatBytes) := engine.node
  val linkParams = p(PoolLinkKey).map(_.adapter)
  val autoNode = linkParams.map(_ => BundleBridgeSink[AutoEndpointAsyncLink]())
  override val tlNode: TLNode = dmaNode
  override lazy val module = new PoolAcceleratorImp(this, params)
}

class PoolAcceleratorImp(outer: PoolAccelerator, params: PoolParams)(implicit p: Parameters)
    extends LazyRoCCModuleImp(outer) {
  val engine = outer.engine.module
  val configuredJob = RegInit(0.U.asTypeOf(new PoolJob(params)))
  val manualStart = Wire(Decoupled(new PoolJob(params)))
  val manualDone = Wire(Decoupled(new PoolEvent))
  val adapter = Module(new PoolLinkAdapter(params, outer.linkParams))

  adapter.io.configuredJob := configuredJob
  adapter.io.manualStart <> manualStart
  manualDone <> adapter.io.manualDone
  engine.io.job <> adapter.io.job
  adapter.io.inputDone <> engine.io.inputDone
  adapter.io.jobDone <> engine.io.done

  outer.linkParams.zip(outer.autoNode).foreach { case (_, node) =>
    val auto = node.in.head._1
    adapter.io.autoLink.get.watchOutput <> FromAsyncBundle(auto.watchOutput)
    auto.reportOutput <> ToAsyncBundle(adapter.io.autoLink.get.reportOutput, AsyncQueueParams.singleton())
    adapter.io.autoLink.get.requestCopy <> FromAsyncBundle(auto.requestCopy)
    auto.reportCopy <> ToAsyncBundle(adapter.io.autoLink.get.reportCopy, AsyncQueueParams.singleton())
    adapter.io.autoLink.get.requestCompute <> FromAsyncBundle(auto.requestCompute)
    auto.reportCompute <> ToAsyncBundle(adapter.io.autoLink.get.reportCompute, AsyncQueueParams.singleton())
  }

  val cmd = io.cmd
  val funct = cmd.bits.inst.funct
  val waiting = funct === PoolCommand.Wait.U
  val starting = funct === PoolCommand.Start.U
  val responseValid = RegInit(false.B)
  val responseData = RegInit(0.U(64.W))
  val responseRd = Reg(UInt(5.W))
  val lastStatus = RegInit(PoolStatus.Success)

  manualStart.valid := cmd.valid && starting && !adapter.io.active
  manualStart.bits := configuredJob
  manualDone.ready := true.B
  when(manualDone.fire) {
    lastStatus := manualDone.bits.status
  }

  val configReady = !adapter.io.active && !responseValid
  val waitReady = !adapter.io.active && !responseValid
  cmd.ready := Mux(starting, manualStart.ready, Mux(waiting, waitReady, configReady))

  when(cmd.fire) {
    switch(funct) {
      is(PoolCommand.Source.U) {
        configuredJob.source := cmd.bits.rs1
      }
      is(PoolCommand.Destination.U) {
        configuredJob.destination := cmd.bits.rs1
      }
      is(PoolCommand.Shape.U) {
        configuredJob.inputHeight := cmd.bits.rs1
        configuredJob.inputWidth := cmd.bits.rs2
      }
      is(PoolCommand.Channels.U) {
        configuredJob.channels := cmd.bits.rs1
      }
      is(PoolCommand.Kernel.U) {
        configuredJob.kernelHeight := cmd.bits.rs1
        configuredJob.kernelWidth := cmd.bits.rs2
      }
      is(PoolCommand.Stride.U) {
        configuredJob.strideHeight := cmd.bits.rs1
        configuredJob.strideWidth := cmd.bits.rs2
      }
      is(PoolCommand.Padding.U) {
        configuredJob.padHeight := cmd.bits.rs1
        configuredJob.padWidth := cmd.bits.rs2
      }
      is(PoolCommand.Mode.U) {
        configuredJob.mode := cmd.bits.rs1
      }
      is(PoolCommand.Start.U) {
        lastStatus := PoolStatus.Success
      }
      is(PoolCommand.Wait.U) {
        responseValid := true.B
        responseData := lastStatus
        responseRd := cmd.bits.inst.rd
      }
    }
  }

  io.resp.valid := responseValid
  io.resp.bits.rd := responseRd
  io.resp.bits.data := responseData
  when(io.resp.fire) {
    responseValid := false.B
  }

  io.busy := adapter.io.active || responseValid
  io.interrupt := false.B
  io.mem.req.valid := false.B
  io.mem.req.bits := 0.U.asTypeOf(io.mem.req.bits)
  io.mem.s1_kill := false.B
  io.mem.s1_data := 0.U.asTypeOf(io.mem.s1_data)
}

class WithPoolAccelerator(params: PoolParams = PoolParams())
    extends Config((_, _, up) => {
      case BuildRoCC => up(BuildRoCC) :+ { p: Parameters =>
        implicit val q: Parameters = p
        LazyModule(new PoolAccelerator(OpcodeSet.custom2, params))
      }
    })
