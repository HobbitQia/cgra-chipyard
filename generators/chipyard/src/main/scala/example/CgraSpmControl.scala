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
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams, FromAsyncBundle, ToAsyncBundle}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.LazyModule

/** CPU configuration and result registers for the CGRA AutoLink adapter. */
class CgraSpmControl(
  params: CgraSpmParams,
  address: BigInt,
  pageSizeBytes: Int)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  private val device = new SimpleDevice("cgra-spm-control", Seq("coredac,cgra-spm-control"))
  val node = TLRegisterNode(
    address = Seq(AddressSet(address, pageSizeBytes - 1)),
    device = device,
    beatBytes = 8,
    concurrency = 1)
  val configNode = BundleBridgeSource(() => new CgraSpmConfigAsyncLink(params))
  val resultNode = BundleBridgeSink[AsyncBundle[SpmLinkEvent]]()

  override lazy val module = new ControlImpl
  class ControlImpl extends Impl {
    withClockAndReset(clock, reset) {
      val configLink = configNode.out.head._1
      val configOut = Wire(Decoupled(new CgraSpmConfig(params)))
      val configAck = FromAsyncBundle(configLink.configAck)
      val resultIn = FromAsyncBundle(resultNode.in.head._1)
      configLink.config <> ToAsyncBundle(configOut, AsyncQueueParams.singleton())

      val spmWordAddress = RegInit(0.U(32.W))
      val dmaTag = RegInit(0.U(32.W))
      val packetCount = RegInit(0.U(32.W))
      val packetLo = RegInit(0.U(64.W))
      val packetMid = RegInit(0.U(64.W))
      val packetHi = RegInit(0.U(64.W))
      val packetTop = RegInit(0.U(64.W))

      val headerSubmit = Wire(Decoupled(UInt(1.W)))
      val packetSubmit = Wire(Decoupled(UInt(1.W)))
      val config = Module(new Arbiter(new CgraSpmConfig(params), 2))
      config.io.in(0).valid := headerSubmit.valid && headerSubmit.bits.asBool
      config.io.in(0).bits.kind := CgraSpmConfigKind.Header
      config.io.in(0).bits.header.spmWordAddress :=
        spmWordAddress(params.cgra.dma.spmAddrWidth - 1, 0)
      config.io.in(0).bits.header.dmaTag := dmaTag(params.cgra.dma.tagWidth - 1, 0)
      config.io.in(0).bits.header.packetCount :=
        packetCount(params.packetCountWidth - 1, 0)
      config.io.in(0).bits.packet := 0.U
      headerSubmit.ready := config.io.in(0).ready

      val packet = Cat(packetTop, packetHi, packetMid, packetLo)
      config.io.in(1).valid := packetSubmit.valid && packetSubmit.bits.asBool
      config.io.in(1).bits.kind := CgraSpmConfigKind.Packet
      config.io.in(1).bits.header := 0.U.asTypeOf(new CgraSpmHeader(params))
      config.io.in(1).bits.packet := packet(params.cgra.intraPktWidth - 1, 0)
      packetSubmit.ready := config.io.in(1).ready
      configOut <> config.io.out

      val results = Module(new Queue(new SpmLinkEvent(params.link), 2))
      val resultArbiter = Module(new Arbiter(new SpmLinkEvent(params.link), 2))
      resultArbiter.io.in(0) <> resultIn
      resultArbiter.io.in(1).valid := configAck.valid &&
        configAck.bits.status =/= SpmLinkStatus.Success
      resultArbiter.io.in(1).bits.link := params.delivery.link.U
      resultArbiter.io.in(1).bits.slot := params.delivery.slot.U
      resultArbiter.io.in(1).bits.bytes := params.delivery.bytes.U
      resultArbiter.io.in(1).bits.status := configAck.bits.status
      resultArbiter.io.in(1).bits.detail := configAck.bits.detail
      resultArbiter.io.in(1).bits.data := 0.U
      results.io.enq <> resultArbiter.io.out
      configAck.ready := Mux(
        configAck.bits.status === SpmLinkStatus.Success,
        true.B,
        resultArbiter.io.in(1).ready)

      val resultPop = Wire(Decoupled(UInt(1.W)))
      val result = RegInit(0.U.asTypeOf(new SpmLinkEvent(params.link)))
      resultPop.ready := Mux(resultPop.bits.asBool, results.io.deq.valid, true.B)
      results.io.deq.ready := resultPop.valid && resultPop.bits.asBool
      when(results.io.deq.fire) {
        result := results.io.deq.bits
      }

      import CgraSpmControlGenerated._
      node.regmap(
        SPM_WORD_ADDRESS -> Seq(RegField(32, spmWordAddress)),
        DMA_TAG -> Seq(RegField(32, dmaTag)),
        PACKET_COUNT -> Seq(RegField(32, packetCount)),
        HEADER_SUBMIT -> Seq(RegField.w(1, headerSubmit)),
        PACKET_LO -> Seq(RegField(64, packetLo)),
        PACKET_MID -> Seq(RegField(64, packetMid)),
        PACKET_HI -> Seq(RegField(64, packetHi)),
        PACKET_TOP -> Seq(RegField(64, packetTop)),
        PACKET_SUBMIT -> Seq(RegField.w(1, packetSubmit)),
        RESULT_VALID -> Seq(RegField.r(1, results.io.deq.valid)),
        RESULT_POP -> Seq(RegField.w(1, resultPop)),
        RESULT_STATUS -> Seq(RegField.r(32, result.status)),
        RESULT_DETAIL -> Seq(RegField.r(32, result.detail)),
        RESULT_DATA -> Seq(RegField.r(32, result.data)))
    }
  }
}

trait CanHaveCgraSpm {
  this: BaseSubsystem with InstantiatesHierarchicalElements with CanHaveSpmAutoLink =>
  private val pbus = locateTLBusWrapper(PBUS)

  val cgraSpm = p(CgraSpmKey).map { attach =>
    val system = spmAutoLink.get
    val cgras = totalTiles.values.toSeq.flatMap {
      case tile: RocketTile =>
        tile.roccs.collect { case accelerator: CGRAAccelerator => accelerator }
      case _ => Nil
    }
    require(cgras.size == 1)
    val params = attach.adapter
    val cgra = cgras.head
    val control = LazyModule(new CgraSpmControl(
      params,
      attach.controlAddress,
      attach.controlBytes))

    cgra.spmNode.get := system.fabric.endpoint(params.endpoint.name)
    cgra.spmConfigNode.get := control.configNode
    control.resultNode := system.fabric.result(params.endpoint.name)
    control.clockNode := pbus.fixedClockNode
    pbus.coupleTo("cgra-spm-control") {
      control.node := TLBuffer() := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _
    }
    control
  }
}
