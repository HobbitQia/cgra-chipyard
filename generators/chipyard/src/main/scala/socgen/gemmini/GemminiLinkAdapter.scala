package chipyard.socgen.gemmini

import chisel3._
import chisel3.util._
import chipyard.socgen.link._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.subsystem.{BaseSubsystem, InstantiatesHierarchicalElements, SBUS}
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.tile.RoCCCommand
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams, FromAsyncBundle, ToAsyncBundle}
import chipyard.socgen.generated.CgraLinkControlGenerated
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import org.chipsalliance.diplomacy.lazymodule.LazyModule

object GemminiLinkStatus {
  val BadAddress = 1
  val BadBeat = 2
  val BadOrder = 3
  val Denied = 4
  val Corrupt = 5
}

case class GemminiLinkParams(auto: AutoLinkParams, beatBytes: Int) {
  require(isPow2(beatBytes))
}

case class GemminiLinkAttachParams(adapter: GemminiLinkParams, portName: String) {
  require(adapter.auto.endpoints.exists(_.name == portName))
}

case object GemminiLinkKey extends Field[Option[GemminiLinkAttachParams]](None)

class WithGemminiLink(params: GemminiLinkAttachParams) extends Config((_, _, _) => { case GemminiLinkKey => Some(params) })

class GemminiLinkWrite(params: GemminiLinkParams) extends Bundle {
  val address = UInt(64.W)
  val source = UInt(16.W)
  val size = UInt(8.W)
  val opcode = UInt(3.W)
  val mask = UInt(params.beatBytes.W)
}

class GemminiLinkAck extends Bundle {
  val source = UInt(16.W)
  val size = UInt(8.W)
  val denied = Bool()
  val corrupt = Bool()
}

/** Converts Gemmini external-SPM writes into the standard AutoLink interface. */
class GemminiLinkAdapter(params: GemminiLinkParams) extends Module {
  val io = IO(new Bundle {
    val write = Flipped(Valid(new GemminiLinkWrite(params)))
    val ack = Flipped(Valid(new GemminiLinkAck))
    val autoLink = new AutoEndpointIO(params.auto)
    val completion = Valid(new AutoEvent(params.auto))
  })

  val watch = Reg(new AutoWatch(params.auto))
  val armed = RegInit(false.B)
  val active = RegInit(false.B)
  val issuedBytes = RegInit(0.U(params.auto.lengthWidth.W))
  val acknowledgedBytes = RegInit(0.U(params.auto.lengthWidth.W))
  val outstanding = RegInit(false.B)
  val source = Reg(UInt(16.W))
  val beatValid = RegInit(false.B)
  val beatError = RegInit(0.U(params.auto.detailWidth.W))
  val producedValid = RegInit(false.B)
  val producedDetail = RegInit(0.U(params.auto.detailWidth.W))

  val expectedSize = log2Ceil(params.beatBytes).U
  val fullMask = ((BigInt(1) << params.beatBytes) - 1).U

  io.autoLink.watchOutput.ready := !armed && !producedValid
  io.autoLink.reportOutput.valid := producedValid
  io.autoLink.reportOutput.bits.status := Mux(
    producedDetail === 0.U,
    AutoLinkStatus.Success,
    AutoLinkStatus.SourceFailure)
  io.autoLink.reportOutput.bits.detail := producedDetail
  io.autoLink.reportOutput.bits.data := 0.U
  io.autoLink.requestCopy.ready := false.B
  io.autoLink.reportCopy.valid := false.B
  io.autoLink.reportCopy.bits := 0.U.asTypeOf(new AutoCopyResult(params.auto))
  io.autoLink.requestCompute.ready := false.B
  io.autoLink.reportCompute.valid := false.B
  io.autoLink.reportCompute.bits := 0.U.asTypeOf(new AutoEvent(params.auto))
  io.completion.valid := false.B
  io.completion.bits := 0.U.asTypeOf(new AutoEvent(params.auto))

  def finish(detail: UInt): Unit = {
    producedValid := true.B
    producedDetail := detail
    io.completion.valid := true.B
    io.completion.bits.status := Mux(
      detail === 0.U,
      AutoLinkStatus.Success,
      AutoLinkStatus.SourceFailure)
    io.completion.bits.detail := detail
    io.completion.bits.data := 0.U
  }

  when(io.autoLink.watchOutput.fire) {
    watch := io.autoLink.watchOutput.bits
    armed := true.B
    active := false.B
    issuedBytes := 0.U
    acknowledgedBytes := 0.U
    outstanding := false.B
  }

  when(armed && !producedValid && io.write.valid) {
    val expectedAddress = watch.address + issuedBytes
    val addressValid = io.write.bits.address === expectedAddress
    val shapeValid = io.write.bits.opcode === TLMessages.PutFullData &&
      io.write.bits.size === expectedSize && io.write.bits.mask === fullMask
    val withinPublication = issuedBytes < watch.bytes

    active := true.B
    when(outstanding) {
      beatValid := false.B
      beatError := GemminiLinkStatus.BadOrder.U
    }.otherwise {
      outstanding := true.B
      source := io.write.bits.source
      beatValid := addressValid && shapeValid && withinPublication
      beatError := Mux(
        !addressValid,
        GemminiLinkStatus.BadAddress.U,
        Mux(!shapeValid, GemminiLinkStatus.BadBeat.U, GemminiLinkStatus.BadOrder.U))
      when(addressValid && shapeValid && withinPublication) {
        issuedBytes := issuedBytes + params.beatBytes.U
      }
    }
  }

  when(active && !producedValid && io.ack.valid) {
    when(!outstanding) {
      finish(GemminiLinkStatus.BadOrder.U)
    }.otherwise {
      val responseShapeValid = io.ack.bits.source === source &&
        io.ack.bits.size === expectedSize
      val responseValid = beatValid && responseShapeValid &&
        !io.ack.bits.denied && !io.ack.bits.corrupt
      val detail = Mux(
        io.ack.bits.denied,
        GemminiLinkStatus.Denied.U,
        Mux(
          io.ack.bits.corrupt,
          GemminiLinkStatus.Corrupt.U,
          Mux(!responseShapeValid, GemminiLinkStatus.BadBeat.U, beatError)))
      val nextBytes = Mux(
        responseValid,
        acknowledgedBytes + params.beatBytes.U,
        acknowledgedBytes)

      acknowledgedBytes := nextBytes
      outstanding := false.B
      when(!responseValid) {
        finish(detail)
      }.elsewhen(nextBytes === watch.bytes) {
        finish(0.U)
      }
    }
  }

  when(io.autoLink.reportOutput.fire) {
    armed := false.B
    active := false.B
    outstanding := false.B
    producedValid := false.B
  }
}

class GemminiLinkMonitor(params: GemminiLinkParams)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  val node = TLAdapterNode()

  override lazy val module = new MonitorImpl
  class MonitorImpl extends Impl {
    val io = IO(new Bundle {
      val write = Valid(new GemminiLinkWrite(params))
      val ack = Valid(new GemminiLinkAck)
    })

    withClockAndReset(clock, reset) {
      val (in, _) = node.in.head
      val (out, _) = node.out.head
      out.a <> in.a
      in.b <> out.b
      out.c <> in.c
      in.d <> out.d
      out.e <> in.e

      io.write.valid := in.a.fire
      io.write.bits.address := in.a.bits.address
      io.write.bits.source := in.a.bits.source
      io.write.bits.size := in.a.bits.size
      io.write.bits.opcode := in.a.bits.opcode
      io.write.bits.mask := in.a.bits.mask
      io.ack.valid := in.d.fire
      io.ack.bits.source := in.d.bits.source
      io.ack.bits.size := in.d.bits.size
      io.ack.bits.denied := in.d.bits.denied
      io.ack.bits.corrupt := in.d.bits.corrupt
    }
  }
}

class GemminiLinkEndpoint(gemminiAccelerator: gemmini.Gemmini[chisel3.SInt, gemmini.Float, gemmini.Float], params: GemminiLinkParams)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  val node = BundleBridgeSink[AutoEndpointAsyncLink]()
  val cmdNode = BundleBridgeSource(() => new AsyncBundle(new RoCCCommand, AsyncQueueParams.singleton()))
  val writerNode = TLIdentityNode()
  val monitor = LazyModule(new GemminiLinkMonitor(params))
  private val controlAddress = CgraLinkControlGenerated.baseAddress + CgraLinkControlGenerated.pageSizeBytes
  private val device = new SimpleDevice("gemmini-job", Seq("coredac,gemmini-job"))
  val controlNode = TLRegisterNode(
    address = Seq(AddressSet(controlAddress, CgraLinkControlGenerated.pageSizeBytes - 1)),
    device = device,
    beatBytes = 8,
    concurrency = 1)
  private val dmaBeatBytes = gemminiAccelerator.config.dma_buswidth / 8

  writerNode := monitor.node := TLWidthWidget(dmaBeatBytes) := TLBuffer() :=
    gemminiAccelerator.spad.spad_writer.get.node

  override lazy val module = new EndpointImpl
  class EndpointImpl extends Impl {
    withClockAndReset(clock, reset) {
      val adapter = Module(new GemminiLinkAdapter(params))
      val job = Module(new GemminiJobAdapter(gemminiAccelerator.config, params.auto))
      val link = node.in.head._1
      val command = cmdNode.out.head._1

      adapter.io.write <> monitor.module.io.write
      adapter.io.ack <> monitor.module.io.ack
      adapter.io.autoLink.watchOutput <> FromAsyncBundle(link.watchOutput)
      link.reportOutput <> ToAsyncBundle(
        adapter.io.autoLink.reportOutput,
        AsyncQueueParams.singleton())
      job.io.requestCopy <> FromAsyncBundle(link.requestCopy)
      link.reportCopy <> ToAsyncBundle(
        job.io.reportCopy,
        AsyncQueueParams.singleton())
      job.io.requestCompute <> FromAsyncBundle(link.requestCompute)
      link.reportCompute <> ToAsyncBundle(
        job.io.reportCompute,
        AsyncQueueParams.singleton())
      adapter.io.autoLink.requestCopy.valid := false.B
      adapter.io.autoLink.requestCopy.bits := 0.U.asTypeOf(new AutoCopyRequest(params.auto))
      adapter.io.autoLink.reportCopy.ready := false.B
      adapter.io.autoLink.requestCompute.valid := false.B
      adapter.io.autoLink.requestCompute.bits := 0.U.asTypeOf(new AutoComputeRequest)
      adapter.io.autoLink.reportCompute.ready := false.B

      job.io.publication <> adapter.io.completion
      command <> ToAsyncBundle(job.io.command, AsyncQueueParams.singleton())

      val aRow = RegInit(0.U(32.W))
      val bRow = RegInit(0.U(32.W))
      val accAddress = RegInit(0.U(32.W))
      val outputRow = RegInit(0.U(32.W))
      val outputRows = RegInit(0.U(32.W))
      val submit = Wire(Decoupled(UInt(1.W)))
      job.io.configIn.valid := submit.valid && submit.bits.asBool
      job.io.configIn.bits.aRow := aRow
      job.io.configIn.bits.bRow := bRow
      job.io.configIn.bits.accAddress := accAddress
      job.io.configIn.bits.outputRow := outputRow
      job.io.configIn.bits.outputRows := outputRows
      submit.ready := Mux(submit.bits.asBool, job.io.configIn.ready, true.B)

      import GemminiJobControl._
      controlNode.regmap(
        ARow -> Seq(RegField(32, aRow)),
        BRow -> Seq(RegField(32, bRow)),
        AccAddress -> Seq(RegField(32, accAddress)),
        OutputRow -> Seq(RegField(32, outputRow)),
        OutputRows -> Seq(RegField(32, outputRows)),
        Submit -> Seq(RegField.w(1, submit)))
    }
  }
}

trait CanHaveGemminiLink {
  this: BaseSubsystem with InstantiatesHierarchicalElements with CanHaveAutoLink with CanHaveGemminiExternalSpm =>
  private val sbus = locateTLBusWrapper(SBUS)

  val gemminiLink = p(GemminiLinkKey).map { attach =>
    val params = attach.adapter
    val externalSpm = gemminiExternalSpm.get
    require(externalSpm.readBeatBytes == params.auto.beatBytes)
    require(externalSpm.writeBeatBytes == params.beatBytes)
    val endpoint = LazyModule(new GemminiLinkEndpoint(externalSpm.gemminiAccelerator, params))

    require(externalSpm.gemminiAccelerator.cmdNode.nonEmpty)

    externalSpm.writerNode := endpoint.writerNode
    endpoint.node := autoLink.get.endpoint(attach.portName)
    externalSpm.gemminiAccelerator.cmdNode.get := endpoint.cmdNode
    endpoint.clockNode := sbus.fixedClockNode
    endpoint.monitor.clockNode := sbus.fixedClockNode
    sbus.coupleTo("gemmini-job") {
      endpoint.controlNode := TLBuffer() := TLFragmenter(
        endpoint.controlNode.beatBytes,
        sbus.blockBytes) := TLWidthWidget(sbus) := _
    }
    endpoint
  }
}
