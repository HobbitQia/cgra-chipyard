package chipyard.socgen.gemmini

import chisel3._
import chisel3.util._
import chipyard.socgen.generated.CgraLinkControlGenerated
import chipyard.socgen.link._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.subsystem.{BaseSubsystem, InstantiatesHierarchicalElements, SBUS}
import freechips.rocketchip.tile.{OpcodeSet, RoCCCommand}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams, FromAsyncBundle, ToAsyncBundle}
import gemmini.GemminiISA._
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

class GemminiJobDesc extends Bundle {
  val aRow = UInt(32.W)
  val bRow = UInt(32.W)
  val accAddress = UInt(32.W)
  val outputRow = UInt(32.W)
  val outputRows = UInt(32.W)
}

/** Translates the AutoLink lifecycle into Gemmini commands and publication checks. */
class GemminiLinkAdapter(
  config: gemmini.GemminiArrayConfig[SInt, gemmini.Float, gemmini.Float],
  params: GemminiLinkParams)(implicit p: Parameters)
    extends Module {
  val io = IO(new Bundle {
    val configIn = Flipped(Decoupled(new GemminiJobDesc))
    val write = Flipped(Valid(new GemminiLinkWrite(params)))
    val ack = Flipped(Valid(new GemminiLinkAck))
    val autoLink = new AutoEndpointIO(params.auto)
    val command = Decoupled(new GemminiAutoCommand)
  })

  object State {
    val idle :: reportCopy :: waitCompute :: issue :: waitOutput :: reportCompute :: Nil = Enum(6)
  }

  val state = RegInit(State.idle)
  val desc = Reg(new GemminiJobDesc)
  val descValid = RegInit(false.B)
  val copyTask = Reg(UInt(params.auto.taskWidth.W))
  val commandIndex = RegInit(0.U(3.W))
  val computeResult = Reg(new AutoEvent(params.auto))
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
  val publication = Wire(Valid(new AutoEvent(params.auto)))

  val dim = config.meshRows * config.tileRows
  val outputStride = config.accType.getWidth / config.inputType.getWidth
  val storeStride = dim * config.accType.getWidth / 8
  require(config.dataflow == gemmini.Dataflow.WS)
  require(config.acc_scale_args.isEmpty)

  val commands = Wire(Vec(5, new RoCCCommand))
  commands.foreach { command =>
    command := 0.U.asTypeOf(new RoCCCommand)
    command.inst.opcode := OpcodeSet.custom3.opcodes.head
    command.inst.xs1 := true.B
    command.inst.xs2 := true.B
  }

  val configExRs1 = Wire(new ConfigExRs1(config.acc_scale_t_bits))
  configExRs1 := 0.U.asTypeOf(configExRs1)
  configExRs1.a_stride := 1.U
  configExRs1.dataflow := gemmini.Dataflow.WS.id.U
  configExRs1.cmd_type := CONFIG_EX
  val configExRs2 = Wire(new ConfigExRs2)
  configExRs2 := 0.U.asTypeOf(configExRs2)
  configExRs2.c_stride := 1.U
  commands(0).inst.funct := CONFIG_CMD
  commands(0).rs1 := configExRs1.asUInt
  commands(0).rs2 := configExRs2.asUInt

  val configStRs1 = Wire(new ConfigMvoutRs1)
  configStRs1 := 0.U.asTypeOf(configStRs1)
  configStRs1.cmd_type := CONFIG_STORE
  val configStRs2 = Wire(new ConfigMvoutRs2(config.acc_scale_t_bits, 32))
  configStRs2 := 0.U.asTypeOf(configStRs2)
  configStRs2.stride := storeStride.U
  commands(1).inst.funct := CONFIG_CMD
  commands(1).rs1 := configStRs1.asUInt
  commands(1).rs2 := configStRs2.asUInt

  val preloadRs1 = Wire(new PreloadRs(config.mvin_rows_bits, config.mvin_cols_bits, config.local_addr_t))
  preloadRs1 := 0.U.asTypeOf(preloadRs1)
  preloadRs1.num_rows := dim.U
  preloadRs1.num_cols := dim.U
  preloadRs1.local_addr := gemmini.LocalAddr.cast_to_sp_addr(preloadRs1.local_addr, desc.bRow)
  val preloadRs2 = Wire(new PreloadRs(config.mvout_rows_bits, config.mvout_cols_bits, config.local_addr_t))
  preloadRs2 := 0.U.asTypeOf(preloadRs2)
  preloadRs2.num_rows := dim.U
  preloadRs2.num_cols := dim.U
  preloadRs2.local_addr := gemmini.LocalAddr.cast_to_acc_addr(
    preloadRs2.local_addr,
    desc.accAddress,
    accumulate = false.B,
    read_full = false.B)
  commands(2).inst.funct := PRELOAD_CMD
  commands(2).rs1 := preloadRs1.asUInt
  commands(2).rs2 := preloadRs2.asUInt

  val computeRs1 = Wire(new ComputeRs(config.mvin_rows_bits, config.mvin_cols_bits, config.local_addr_t))
  computeRs1 := 0.U.asTypeOf(computeRs1)
  computeRs1.num_rows := dim.U
  computeRs1.num_cols := dim.U
  computeRs1.local_addr := gemmini.LocalAddr.cast_to_sp_addr(computeRs1.local_addr, desc.aRow)
  val computeRs2 = Wire(new ComputeRs(config.mvin_rows_bits, config.mvin_cols_bits, config.local_addr_t))
  computeRs2 := 0.U.asTypeOf(computeRs2)
  computeRs2.num_rows := dim.U
  computeRs2.num_cols := dim.U
  computeRs2.local_addr := gemmini.LocalAddr.garbage_addr(computeRs2.local_addr)
  commands(3).inst.funct := COMPUTE_AND_FLIP_CMD
  commands(3).rs1 := computeRs1.asUInt
  commands(3).rs2 := computeRs2.asUInt

  val mvoutRs1 = Wire(new MvoutSpadRs1(32, config.local_addr_t))
  mvoutRs1 := 0.U.asTypeOf(mvoutRs1)
  mvoutRs1.stride := outputStride.U
  mvoutRs1.local_addr := gemmini.LocalAddr.cast_to_sp_addr(mvoutRs1.local_addr, desc.outputRow)
  val mvoutRs2 = Wire(new MvoutRs2(config.mvout_rows_bits, config.mvout_cols_bits, config.local_addr_t))
  mvoutRs2 := 0.U.asTypeOf(mvoutRs2)
  mvoutRs2.num_rows := desc.outputRows
  mvoutRs2.num_cols := dim.U
  mvoutRs2.local_addr := gemmini.LocalAddr.cast_to_acc_addr(
    mvoutRs2.local_addr,
    desc.accAddress,
    accumulate = false.B,
    read_full = true.B)
  commands(4).inst.funct := STORE_SPAD_CMD
  commands(4).rs1 := mvoutRs1.asUInt
  commands(4).rs2 := mvoutRs2.asUInt

  val expectedSize = log2Ceil(params.beatBytes).U
  val fullMask = ((BigInt(1) << params.beatBytes) - 1).U

  io.configIn.ready := state === State.idle && !descValid
  io.autoLink.watchOutput.ready := !armed && !producedValid
  io.autoLink.reportOutput.valid := producedValid
  io.autoLink.reportOutput.bits.status := Mux(
    producedDetail === 0.U,
    AutoLinkStatus.Success,
    AutoLinkStatus.SourceFailure)
  io.autoLink.reportOutput.bits.detail := producedDetail
  io.autoLink.reportOutput.bits.data := 0.U
  io.autoLink.requestCopy.ready := state === State.idle && descValid
  io.autoLink.reportCopy.valid := state === State.reportCopy
  io.autoLink.reportCopy.bits.task := copyTask
  io.autoLink.reportCopy.bits.status := AutoLinkStatus.Success
  io.autoLink.reportCopy.bits.detail := 0.U
  io.autoLink.requestCompute.ready := Mux(
    io.autoLink.requestCompute.bits.start,
    state === State.waitCompute,
    state === State.idle || state === State.waitCompute)
  io.autoLink.reportCompute.valid := state === State.reportCompute
  io.autoLink.reportCompute.bits := computeResult
  io.command.valid := state === State.issue
  io.command.bits.command := commands(commandIndex)
  io.command.bits.last := commandIndex === (commands.length - 1).U
  publication.valid := false.B
  publication.bits := 0.U.asTypeOf(new AutoEvent(params.auto))

  def finish(detail: UInt): Unit = {
    producedValid := true.B
    producedDetail := detail
    publication.valid := true.B
    publication.bits.status := Mux(
      detail === 0.U,
      AutoLinkStatus.Success,
      AutoLinkStatus.SourceFailure)
    publication.bits.detail := detail
    publication.bits.data := 0.U
  }

  when(io.configIn.fire) {
    desc := io.configIn.bits
    descValid := true.B
  }
  when(io.autoLink.requestCopy.fire) {
    copyTask := io.autoLink.requestCopy.bits.task
    state := State.reportCopy
  }
  when(io.autoLink.reportCopy.fire) {
    state := State.waitCompute
  }
  when(io.autoLink.requestCompute.fire) {
    when(io.autoLink.requestCompute.bits.start) {
      commandIndex := 0.U
      state := State.issue
    }.otherwise {
      descValid := false.B
      state := State.idle
    }
  }
  when(io.command.fire) {
    when(commandIndex === (commands.length - 1).U) {
      state := State.waitOutput
    }.otherwise {
      commandIndex := commandIndex + 1.U
    }
  }
  when(state === State.waitOutput && publication.valid) {
    computeResult := publication.bits
    state := State.reportCompute
  }
  when(io.autoLink.reportCompute.fire) {
    descValid := false.B
    state := State.idle
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

class GemminiLinkEndpoint(gemminiRoCC: GemminiRoCC, params: GemminiLinkParams)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  private val gemminiAccelerator = gemminiRoCC.accelerator
  val autoNode = BundleBridgeSink[AutoEndpointAsyncLink]()
  val cmdNode = BundleBridgeSource(() =>
    new AsyncBundle(
      new GemminiAutoCommand()(gemminiAccelerator.p),
      AsyncQueueParams.singleton()))
  val writerNode = TLAdapterNode()
  private val device = new SimpleDevice("gemmini-job", Seq("coredac,gemmini-job"))
  val controlNode = TLRegisterNode(
    address = Seq(AddressSet(
      CgraLinkControlGenerated.gemminiJobAddress,
      CgraLinkControlGenerated.pageSizeBytes - 1)),
    device = device,
    beatBytes = 8,
    concurrency = 1)
  private val dmaBeatBytes = gemminiAccelerator.config.dma_buswidth / 8

  writerNode := TLWidthWidget(dmaBeatBytes) := TLBuffer() :=
    gemminiAccelerator.spad.spad_writer.get.node

  override lazy val module = new EndpointImpl
  class EndpointImpl extends Impl {
    withClockAndReset(clock, reset) {
      val adapter = Module(new GemminiLinkAdapter(
        gemminiAccelerator.config,
        params)(gemminiAccelerator.p))
      val link = autoNode.in.head._1
      val command = cmdNode.out.head._1
      val (writerIn, _) = writerNode.in.head
      val (writerOut, _) = writerNode.out.head

      writerOut.a <> writerIn.a
      writerIn.b <> writerOut.b
      writerOut.c <> writerIn.c
      writerIn.d <> writerOut.d
      writerOut.e <> writerIn.e
      adapter.io.write.valid := writerIn.a.fire
      adapter.io.write.bits.address := writerIn.a.bits.address
      adapter.io.write.bits.source := writerIn.a.bits.source
      adapter.io.write.bits.size := writerIn.a.bits.size
      adapter.io.write.bits.opcode := writerIn.a.bits.opcode
      adapter.io.write.bits.mask := writerIn.a.bits.mask
      adapter.io.ack.valid := writerIn.d.fire
      adapter.io.ack.bits.source := writerIn.d.bits.source
      adapter.io.ack.bits.size := writerIn.d.bits.size
      adapter.io.ack.bits.denied := writerIn.d.bits.denied
      adapter.io.ack.bits.corrupt := writerIn.d.bits.corrupt
      adapter.io.autoLink.watchOutput <> FromAsyncBundle(link.watchOutput)
      link.reportOutput <> ToAsyncBundle(
        adapter.io.autoLink.reportOutput,
        AsyncQueueParams.singleton())
      adapter.io.autoLink.requestCopy <> FromAsyncBundle(link.requestCopy)
      link.reportCopy <> ToAsyncBundle(
        adapter.io.autoLink.reportCopy,
        AsyncQueueParams.singleton())
      adapter.io.autoLink.requestCompute <> FromAsyncBundle(link.requestCompute)
      link.reportCompute <> ToAsyncBundle(
        adapter.io.autoLink.reportCompute,
        AsyncQueueParams.singleton())
      command <> ToAsyncBundle(adapter.io.command, AsyncQueueParams.singleton())

      val aRow = RegInit(0.U(32.W))
      val bRow = RegInit(0.U(32.W))
      val accAddress = RegInit(0.U(32.W))
      val outputRow = RegInit(0.U(32.W))
      val outputRows = RegInit(0.U(32.W))
      val submit = Wire(Decoupled(UInt(1.W)))
      adapter.io.configIn.valid := submit.valid && submit.bits.asBool
      adapter.io.configIn.bits.aRow := aRow
      adapter.io.configIn.bits.bRow := bRow
      adapter.io.configIn.bits.accAddress := accAddress
      adapter.io.configIn.bits.outputRow := outputRow
      adapter.io.configIn.bits.outputRows := outputRows
      submit.ready := Mux(submit.bits.asBool, adapter.io.configIn.ready, true.B)

      import CgraLinkControlGenerated._
      controlNode.regmap(
        GEMMINI_A_ROW -> Seq(RegField(32, aRow)),
        GEMMINI_B_ROW -> Seq(RegField(32, bRow)),
        GEMMINI_ACC_ADDRESS -> Seq(RegField(32, accAddress)),
        GEMMINI_OUTPUT_ROW -> Seq(RegField(32, outputRow)),
        GEMMINI_OUTPUT_ROWS -> Seq(RegField(32, outputRows)),
        GEMMINI_SUBMIT -> Seq(RegField.w(1, submit)))
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
    val endpoint = LazyModule(new GemminiLinkEndpoint(externalSpm.gemminiRoCC, params))

    require(externalSpm.gemminiRoCC.cmdNode.nonEmpty)

    externalSpm.writerNode := endpoint.writerNode
    endpoint.autoNode := autoLink.get.endpoint(attach.portName)
    externalSpm.gemminiRoCC.cmdNode.get := endpoint.cmdNode
    endpoint.clockNode := sbus.fixedClockNode
    sbus.coupleTo("gemmini-job") {
      endpoint.controlNode := TLBuffer() := TLFragmenter(
        endpoint.controlNode.beatBytes,
        sbus.blockBytes) := TLWidthWidget(sbus) := _
    }
    endpoint
  }
}
