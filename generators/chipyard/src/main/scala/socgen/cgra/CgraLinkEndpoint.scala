package chipyard.socgen.cgra

import chisel3._
import chisel3.util._
import chipyard.example.CGRAAccelerator
import chipyard.socgen.generated.CgraLinkControlGenerated
import chipyard.socgen.link.{AutoEvent, AutoLinkStatus, CanHaveAutoLink}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.subsystem.{BaseSubsystem, InstantiatesHierarchicalElements, PBUS}
import freechips.rocketchip.tile.RocketTile
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{AsyncQueueParams, FromAsyncBundle, ToAsyncBundle}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.LazyModule

/** SoC attachment and CPU-visible registers for the CGRA AutoLink adapter. */
class CgraLinkEndpoint(params: CgraLinkParams, resultNames: Seq[String], address: BigInt, pageSizeBytes: Int)(implicit p: Parameters) extends ClockSinkDomain(ClockSinkParameters())(p) {
  private val device = new SimpleDevice("cgra-link-control", Seq("coredac,cgra-link-control"))
  val controlNode = TLRegisterNode(
    address = Seq(AddressSet(address, pageSizeBytes - 1)),
    device = device,
    beatBytes = 8,
    concurrency = 1)
  val configNode = BundleBridgeSource(() => new CgraLinkConfigAsync(params))
  private val resultNodes = resultNames.map(name =>
    name -> BundleBridgeSink[DecoupledIO[AutoEvent]]()).toMap

  def resultNode(name: String): BundleBridgeSink[DecoupledIO[AutoEvent]] = resultNodes(name)

  override lazy val module = new EndpointImpl
  class EndpointImpl extends Impl {
    withClockAndReset(clock, reset) {
      val configLink = configNode.out.head._1
      val configOut = Wire(Decoupled(new CgraLinkConfig(params)))
      val patchOut = Wire(Decoupled(new CgraPatchConfig))
      val repeatOut = Wire(Decoupled(UInt(32.W)))
      val configAck = FromAsyncBundle(configLink.ack)
      val resultIn = resultNames.map(name => resultNodes(name).in.head._1)
      configLink.config <> ToAsyncBundle(configOut, AsyncQueueParams.singleton())
      configLink.patch <> ToAsyncBundle(patchOut, AsyncQueueParams.singleton())
      configLink.repeat <> ToAsyncBundle(repeatOut, AsyncQueueParams.singleton())

      val job = RegInit(0.U(32.W))
      val packetCount = RegInit(0.U(32.W))
      val expectedCompletions = RegInit(0.U(32.W))
      val patchCount = RegInit(0.U(32.W))
      val repeatCount = RegInit(0.U(32.W))
      val repeatPacket = RegInit(0.U(32.W))
      val patch = RegInit(0.U.asTypeOf(new CgraPatchConfig))
      val writeback = RegInit(0.U.asTypeOf(new CgraWritebackConfig))
      val patchPush = Wire(Decoupled(UInt(1.W)))
      val repeatPush = Wire(Decoupled(UInt(1.W)))
      val configSubmit = Wire(Decoupled(UInt(1.W)))
      val configPending = RegInit(false.B)
      val configReady = RegInit(false.B)
      val configDone = RegInit(false.B)
      configOut.valid := configSubmit.valid && configSubmit.bits.asBool && !configPending
      configOut.bits.job := job
      configOut.bits.packetCount := packetCount
      configOut.bits.expectedCompletions := expectedCompletions
      configOut.bits.patchCount := patchCount
      configOut.bits.repeatCount := repeatCount
      configOut.bits.writeback := writeback
      configSubmit.ready := Mux(configSubmit.bits.asBool, configOut.ready && !configPending, true.B)
      configAck.ready := true.B
      patchOut.valid := patchPush.valid && patchPush.bits.asBool
      patchOut.bits := patch
      patchPush.ready := Mux(patchPush.bits.asBool, patchOut.ready, true.B)
      repeatOut.valid := repeatPush.valid && repeatPush.bits.asBool
      repeatOut.bits := repeatPacket
      repeatPush.ready := Mux(repeatPush.bits.asBool, repeatOut.ready, true.B)

      when(configOut.fire) {
        writeback.enabled := false.B
        repeatCount := 0.U
        configPending := true.B
        configReady := false.B
        configDone := false.B
      }
      when(configAck.fire) {
        configPending := !configAck.bits.done
        configReady := true.B
        configDone := configAck.bits.done
      }

      val results = Module(new Queue(new AutoEvent(params.auto), math.max(2, resultNames.size)))
      val resultArbiter = Module(new Arbiter(new AutoEvent(params.auto), resultIn.size))
      resultIn.zipWithIndex.foreach { case (result, index) =>
        resultArbiter.io.in(index) <> result
      }
      results.io.enq <> resultArbiter.io.out

      val resultPop = Wire(Decoupled(UInt(1.W)))
      val result = RegInit(0.U.asTypeOf(new AutoEvent(params.auto)))
      resultPop.ready := Mux(resultPop.bits.asBool, results.io.deq.valid, true.B)
      results.io.deq.ready := resultPop.valid && resultPop.bits.asBool
      when(results.io.deq.fire) {
        result := results.io.deq.bits
      }

      import CgraLinkControlGenerated._
      controlNode.regmap(
        PACKET_COUNT -> Seq(RegField(32, packetCount)),
        CONFIG_SUBMIT -> Seq(RegField.w(1, configSubmit)),
        RESULT_VALID -> Seq(RegField.r(1, results.io.deq.valid)),
        RESULT_POP -> Seq(RegField.w(1, resultPop)),
        RESULT_STATUS -> Seq(RegField.r(32, result.status)),
        RESULT_DETAIL -> Seq(RegField.r(32, result.detail)),
        RESULT_DATA -> Seq(RegField.r(32, result.data)),
        RESULT_STAGE -> Seq(RegField.r(32, result.stage)),
        RESULT_JOB -> Seq(RegField.r(32, result.job)),
        JOB -> Seq(RegField(32, job)),
        EXPECTED_COMPLETES -> Seq(RegField(32, expectedCompletions)),
        CONFIG_READY -> Seq(RegField.r(1, configReady)),
        CONFIG_DONE -> Seq(RegField.r(1, configDone)),
        CONFIG_STATUS -> Seq(RegField.r(32, AutoLinkStatus.Success)),
        CONFIG_DETAIL -> Seq(RegField.r(32, 0.U)),
        PATCH_COUNT -> Seq(RegField(32, patchCount)),
        PATCH_PACKET -> Seq(RegField(32, patch.packetIndex)),
        PATCH_SOURCE -> Seq(RegField(CgraSymbolSource.Width, patch.source)),
        PATCH_COEFFICIENT -> Seq(RegField(32, patch.coefficient)),
        PATCH_BIAS -> Seq(RegField(32, patch.bias)),
        PATCH_PUSH -> Seq(RegField.w(1, patchPush)),
        REPEAT_COUNT -> Seq(RegField(32, repeatCount)),
        REPEAT_PACKET -> Seq(RegField(32, repeatPacket)),
        REPEAT_PUSH -> Seq(RegField.w(1, repeatPush)),
        OUT_ENABLE -> Seq(RegField(1, writeback.enabled)),
        OUT_ADDRESS -> Seq(RegField(64, writeback.address)),
        OUT_WORD -> Seq(RegField(32, writeback.word)),
        OUT_SLOT_STRIDE -> Seq(RegField(32, writeback.slotStride)),
        OUT_CHANNELS -> Seq(RegField(32, writeback.channels)),
        OUT_ROW_STRIDE -> Seq(RegField(32, writeback.rowStride)))
    }
  }
}

trait CanHaveCgraLink {
  this: BaseSubsystem with InstantiatesHierarchicalElements with CanHaveAutoLink =>
  private val pbus = locateTLBusWrapper(PBUS)

  val cgraLink = p(CgraLinkKey).map { attach =>
    val cgras = totalTiles.values.toSeq.flatMap {
      case tile: RocketTile =>
        tile.roccs.collect { case accelerator: CGRAAccelerator => accelerator }
      case _ => Nil
    }
    require(cgras.size == 1)
    val params = attach.adapter
    val cgra = cgras.head
    val endpoint = LazyModule(new CgraLinkEndpoint(params, attach.resultNames, attach.controlAddress, attach.controlBytes))

    cgra.autoNode.get := autoLink.get.endpoint(attach.portName)
    cgra.linkConfigNode.get := endpoint.configNode
    attach.resultNames.foreach { name =>
      endpoint.resultNode(name) := autoLink.get.result(name)
    }
    endpoint.clockNode := pbus.fixedClockNode
    pbus.coupleTo("cgra-link-control") {
      endpoint.controlNode := TLBuffer() := TLFragmenter(
        pbus.beatBytes,
        pbus.blockBytes) := _
    }
    endpoint
  }
}
