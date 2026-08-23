package chipyard.example

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.subsystem.{BaseSubsystem, InstantiatesHierarchicalElements, PBUS}
import freechips.rocketchip.tile.RocketTile
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{AsyncQueueParams, FromAsyncBundle, ToAsyncBundle}
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import org.chipsalliance.diplomacy.lazymodule.LazyModule

case class GemminiCgraSpmDmaParams(spm: SharedSpmParams, controlAddress: BigInt, pageSizeBytes: Int = 4096) {
  val protocol: SpmDmaParams = SpmDmaParams(
    slotCount = spm.slotCount,
    slotSizeBytes = spm.slotSizeBytes,
    beatBytes = CGRAGenerated.params.dma.dramDataWidth / 8,
    resultWidth = CGRAGenerated.params.dataPayloadWidth)
  val cgra: CgraSpmParams = CgraSpmParams(protocol, CGRAGenerated.params, spm.slotBases)
}

case object GemminiCgraSpmDmaKey extends Field[Option[GemminiCgraSpmDmaParams]](None)
case object CgraSpmKey extends Field[Option[CgraSpmParams]](None)

class WithGemminiCgraSpmDma(params: GemminiCgraSpmDmaParams)
    extends Config((_, _, _) => {
      case GemminiCgraSpmDmaKey => Some(params)
      case CgraSpmKey => Some(params.cgra)
    })

/** Current Gemmini-to-CGRA instance of the reusable SPM DMA protocol. */
class GemminiCgraSpmDma(
  gemminiAccelerator: gemmini.Gemmini[chisel3.SInt, gemmini.Float, gemmini.Float],
  params: GemminiCgraSpmDmaParams)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  private val gemminiConfig = gemminiAccelerator.config
  private val readBeatBytes = gemminiConfig.sp_width / 8
  private val writeBeatBytes = gemminiConfig.meshColumns * gemminiConfig.tileColumns * gemminiConfig.accType.getWidth / 8
  private val producerParams = GemminiSpmParams(params.spm.slotBases, params.spm.slotSizeBytes, writeBeatBytes)
  private val controlDevice = new SimpleDevice("cgra-spm-control", Seq("coredac,cgra-spm-control"))

  require(gemminiConfig.use_shared_ext_mem && gemminiConfig.use_tl_ext_mem)
  require(gemminiConfig.tl_ext_mem_base == params.spm.baseAddress)
  require(readBeatBytes == params.protocol.beatBytes)
  require(gemminiAccelerator.spad.spad_writer.isDefined)

  val spm = LazyModule(new SharedSpm(params.spm, readBeatBytes, writeBeatBytes))
  val monitor = LazyModule(new GemminiSpmMonitor(producerParams))
  val cgraNode = BundleBridgeSource(() => new CgraSpmAsyncLink(params.cgra))
  val controlNode = TLRegisterNode(
    address = Seq(AddressSet(params.controlAddress, params.pageSizeBytes - 1)),
    device = controlDevice,
    beatBytes = 8,
    concurrency = 1)

  spm.readers :=* gemminiAccelerator.spad_read_nodes
  spm.writers :=* TLWidthWidget(readBeatBytes) :=* TLBuffer() :=* gemminiAccelerator.spad_write_nodes
  spm.writers := monitor.node := TLWidthWidget(readBeatBytes) := TLBuffer() :=
    gemminiAccelerator.spad.spad_writer.get.node

  override lazy val module = new DmaImpl
  class DmaImpl extends Impl {
    withClockAndReset(clock, reset) {
      val producer = Module(new GemminiSpmProducer(params.protocol, producerParams))
      val pipeline = Module(new SpmDmaController(params.protocol))
      val control = Module(new CgraSpmControl(params.cgra))
      val link = cgraNode.out.head._1

      producer.io.write <> monitor.module.io.write
      producer.io.ack <> monitor.module.io.ack
      producer.io.start <> pipeline.io.producerStart
      pipeline.io.producerDone <> producer.io.done
      control.io.jobOut <> pipeline.io.jobIn
      control.io.stepOut <> pipeline.io.stepIn
      control.io.resultIn <> pipeline.io.resultOut

      link.config <> ToAsyncBundle(control.io.configOut, AsyncQueueParams.singleton())
      control.io.configAck <> FromAsyncBundle(link.configAck)
      link.transferStart <> ToAsyncBundle(pipeline.io.transferStart, AsyncQueueParams.singleton())
      pipeline.io.transferDone <> FromAsyncBundle(link.transferDone)
      link.consumerStart <> ToAsyncBundle(pipeline.io.consumerStart, AsyncQueueParams.singleton())
      pipeline.io.consumerDone <> FromAsyncBundle(link.consumerDone)

      val jobId = RegInit(0.U(32.W))
      val slot = RegInit(0.U(32.W))
      val bytes = RegInit(0.U(32.W))
      val mode = RegInit(SpmDmaMode.Auto)
      val spmWordAddress = RegInit(0.U(32.W))
      val dmaTag = RegInit(0.U(32.W))
      val packetCount = RegInit(0.U(32.W))
      val packetLo = RegInit(0.U(64.W))
      val packetMid = RegInit(0.U(64.W))
      val packetHi = RegInit(0.U(64.W))
      val packetTop = RegInit(0.U(64.W))

      val headerSubmit = Wire(Decoupled(UInt(1.W)))
      control.io.headerSubmit.valid := headerSubmit.valid && headerSubmit.bits.asBool
      control.io.headerSubmit.bits.jobId := jobId
      control.io.headerSubmit.bits.slot := slot(params.protocol.slotWidth - 1, 0)
      control.io.headerSubmit.bits.bytes := bytes
      control.io.headerSubmit.bits.spmWordAddress := spmWordAddress(params.cgra.cgra.dma.spmAddrWidth - 1, 0)
      control.io.headerSubmit.bits.dmaTag := dmaTag(params.cgra.cgra.dma.tagWidth - 1, 0)
      control.io.headerSubmit.bits.packetCount := packetCount(params.cgra.packetCountWidth - 1, 0)
      headerSubmit.ready := control.io.headerSubmit.ready

      val packetSubmit = Wire(Decoupled(UInt(1.W)))
      val packet = Cat(packetTop, packetHi, packetMid, packetLo)
      control.io.packetSubmit.valid := packetSubmit.valid && packetSubmit.bits.asBool
      control.io.packetSubmit.bits := packet(params.cgra.cgra.intraPktWidth - 1, 0)
      packetSubmit.ready := control.io.packetSubmit.ready

      val jobSubmit = Wire(Decoupled(UInt(1.W)))
      control.io.jobSubmit.valid := jobSubmit.valid && jobSubmit.bits.asBool
      control.io.jobSubmit.bits.jobId := jobId
      control.io.jobSubmit.bits.slot := slot(params.protocol.slotWidth - 1, 0)
      control.io.jobSubmit.bits.bytes := bytes
      control.io.jobSubmit.bits.mode := mode
      jobSubmit.ready := control.io.jobSubmit.ready

      val stepSubmit = Wire(Decoupled(UInt(1.W)))
      control.io.stepSubmit.valid := stepSubmit.valid && stepSubmit.bits.asBool
      control.io.stepSubmit.bits.jobId := jobId
      control.io.stepSubmit.bits.slot := slot(params.protocol.slotWidth - 1, 0)
      control.io.stepSubmit.bits.bytes := bytes
      stepSubmit.ready := control.io.stepSubmit.ready

      val resultPop = Wire(Decoupled(UInt(1.W)))
      val result = RegInit(0.U.asTypeOf(new SpmDmaResult(params.protocol)))
      resultPop.ready := Mux(resultPop.bits.asBool, control.io.resultOut.valid, true.B)
      control.io.resultOut.ready := resultPop.valid && resultPop.bits.asBool
      when(control.io.resultOut.fire) {
        result := control.io.resultOut.bits
      }

      import CgraSpmControlGenerated._
      controlNode.regmap(
        JOB_ID -> Seq(RegField(32, jobId)),
        SLOT -> Seq(RegField(32, slot)),
        BYTES -> Seq(RegField(32, bytes)),
        MODE -> Seq(RegField(1, mode)),
        SPM_WORD_ADDRESS -> Seq(RegField(32, spmWordAddress)),
        DMA_TAG -> Seq(RegField(32, dmaTag)),
        PACKET_COUNT -> Seq(RegField(32, packetCount)),
        HEADER_SUBMIT -> Seq(RegField.w(1, headerSubmit)),
        PACKET_LO -> Seq(RegField(64, packetLo)),
        PACKET_MID -> Seq(RegField(64, packetMid)),
        PACKET_HI -> Seq(RegField(64, packetHi)),
        PACKET_TOP -> Seq(RegField(64, packetTop)),
        PACKET_SUBMIT -> Seq(RegField.w(1, packetSubmit)),
        JOB_SUBMIT -> Seq(RegField.w(1, jobSubmit)),
        STEP_SUBMIT -> Seq(RegField.w(1, stepSubmit)),
        RESULT_VALID -> Seq(RegField.r(1, control.io.resultOut.valid)),
        RESULT_POP -> Seq(RegField.w(1, resultPop)),
        RESULT_JOB_ID -> Seq(RegField.r(32, result.jobId)),
        RESULT_SLOT -> Seq(RegField.r(32, result.slot)),
        RESULT_BYTES -> Seq(RegField.r(32, result.bytes)),
        RESULT_STAGE -> Seq(RegField.r(32, result.stage)),
        RESULT_STATUS -> Seq(RegField.r(32, result.status)),
        RESULT_DETAIL -> Seq(RegField.r(32, result.detail)),
        RESULT_DATA -> Seq(RegField.r(32, result.data)))
    }
  }
}

trait CanHaveGemminiCgraSpmDma {
  this: BaseSubsystem with InstantiatesHierarchicalElements =>
  private val pbus = locateTLBusWrapper(PBUS)

  val gemminiCgraSpmDma = p(GemminiCgraSpmDmaKey).map { params =>
    val gemminis = totalTiles.values.toSeq.flatMap {
      case tile: RocketTile => tile.roccs.collect { case accelerator: gemmini.Gemmini[_, _, _] => accelerator }
      case _ => Nil
    }
    val cgras = totalTiles.values.toSeq.flatMap {
      case tile: RocketTile => tile.roccs.collect { case accelerator: CGRAAccelerator => accelerator }
      case _ => Nil
    }
    require(gemminis.size == 1 && cgras.size == 1)

    val gemminiAccelerator = gemminis.head.asInstanceOf[
      gemmini.Gemmini[chisel3.SInt, gemmini.Float, gemmini.Float]]
    val dma = LazyModule(new GemminiCgraSpmDma(gemminiAccelerator, params))
    cgras.head.spmNode.get := dma.cgraNode
    dma.clockNode := pbus.fixedClockNode
    dma.spm.clockNode := pbus.fixedClockNode
    dma.monitor.clockNode := pbus.fixedClockNode

    pbus.coupleTo("shared-spm") {
      dma.spm.readers := TLFragmenter(params.protocol.beatBytes, pbus.blockBytes) := TLWidthWidget(pbus) := _
    }
    pbus.coupleTo("cgra-spm-control") {
      dma.controlNode := TLBuffer() := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _
    }
    dma
  }
}
