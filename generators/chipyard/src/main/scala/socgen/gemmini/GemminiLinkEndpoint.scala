package chipyard.socgen.gemmini

import chisel3._
import chisel3.util.Decoupled
import chipyard.socgen.generated.CgraLinkControlGenerated
import chipyard.socgen.link.{AutoLinkStatus, CanHaveAutoLink}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.subsystem.{BaseSubsystem, InstantiatesHierarchicalElements, SBUS}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{AsyncQueueParams, FromAsyncBundle, ToAsyncBundle}
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import org.chipsalliance.diplomacy.lazymodule.LazyModule

case class GemminiLinkAttachParams(adapter: GemminiLinkParams, portName: String) {
  require(adapter.auto.endpoints.exists(_.name == portName))
}

case object GemminiLinkKey extends Field[Option[GemminiLinkAttachParams]](None)

class WithGemminiLink(params: GemminiLinkAttachParams) extends Config((_, _, _) => { case GemminiLinkKey => Some(params) })

/** SoC attachment and CPU-visible registers for the Gemmini AutoLink adapter. */
class GemminiLinkEndpoint(gemminiRoCC: GemminiRoCC, params: GemminiLinkParams)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  private val gemminiAccelerator = gemminiRoCC.accelerator
  val configNode = BundleBridgeSource(() => new GemminiLinkConfigAsync(params))
  val observeNode = BundleBridgeSource(() => new GemminiLinkObserveAsync(params))
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
      val configLink = configNode.out.head._1
      val observe = observeNode.out.head._1
      val configOut = Wire(Decoupled(new GemminiLinkConfig(params)))
      val configAck = FromAsyncBundle(configLink.ack)
      val event = Wire(Decoupled(new GemminiLinkEvent(params)))
      val (writerIn, _) = writerNode.in.head
      val (writerOut, _) = writerNode.out.head

      val selectAck = writerOut.d.valid
      writerOut.a.valid := writerIn.a.valid && event.ready && !selectAck
      writerOut.a.bits := writerIn.a.bits
      writerIn.a.ready := writerOut.a.ready && event.ready && !selectAck

      writerIn.b <> writerOut.b
      writerOut.c <> writerIn.c
      writerIn.d.valid := writerOut.d.valid && event.ready
      writerIn.d.bits := writerOut.d.bits
      writerOut.d.ready := writerIn.d.ready && event.ready
      writerOut.e <> writerIn.e

      event.valid := Mux(
        selectAck,
        writerIn.d.ready,
        writerIn.a.valid && writerOut.a.ready)
      event.bits := 0.U.asTypeOf(new GemminiLinkEvent(params))
      event.bits.isAck := selectAck
      event.bits.write.address := writerIn.a.bits.address
      event.bits.write.source := writerIn.a.bits.source
      event.bits.write.size := writerIn.a.bits.size
      event.bits.write.opcode := writerIn.a.bits.opcode
      event.bits.write.mask := writerIn.a.bits.mask
      event.bits.ack.source := writerOut.d.bits.source
      event.bits.ack.size := writerOut.d.bits.size
      event.bits.ack.denied := writerOut.d.bits.denied
      event.bits.ack.corrupt := writerOut.d.bits.corrupt

      configLink.config <> ToAsyncBundle(configOut, AsyncQueueParams.singleton())
      observe.event <> ToAsyncBundle(event, AsyncQueueParams.singleton())

      val job = RegInit(0.U(32.W))
      val commandCount = RegInit(0.U(32.W))
      val configSubmit = Wire(Decoupled(UInt(1.W)))
      val ackValid = RegInit(false.B)
      val ackStatus = RegInit(AutoLinkStatus.Success)
      val ackDetail = RegInit(0.U(params.auto.detailWidth.W))
      configOut.valid := configSubmit.valid && configSubmit.bits.asBool
      configOut.bits.job := job
      configOut.bits.commandCount := commandCount
      configSubmit.ready := Mux(configSubmit.bits.asBool, configOut.ready, true.B)
      configAck.ready := true.B

      when(configSubmit.fire && configSubmit.bits.asBool) {
        ackValid := false.B
      }
      when(configAck.fire) {
        ackValid := true.B
        ackStatus := configAck.bits.status
        ackDetail := configAck.bits.detail
      }

      import CgraLinkControlGenerated._
      controlNode.regmap(
        GEMMINI_COMMAND_COUNT -> Seq(RegField(32, commandCount)),
        GEMMINI_SUBMIT -> Seq(RegField.w(1, configSubmit)),
        GEMMINI_CAPTURE_READY -> Seq(RegField.r(1, ackValid)),
        GEMMINI_CONFIG_STATUS -> Seq(RegField.r(32, ackStatus)),
        GEMMINI_CONFIG_DETAIL -> Seq(RegField.r(32, ackDetail)),
        GEMMINI_SELECT -> Seq(RegField(32, job)))
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
    val gemminiRoCC = externalSpm.gemminiRoCC
    val endpoint = LazyModule(new GemminiLinkEndpoint(gemminiRoCC, params))

    require(gemminiRoCC.autoNode.nonEmpty)
    require(gemminiRoCC.configNode.nonEmpty)
    require(gemminiRoCC.observeNode.nonEmpty)

    externalSpm.writerNode := endpoint.writerNode
    gemminiRoCC.autoNode.get := autoLink.get.endpoint(attach.portName)
    gemminiRoCC.configNode.get := endpoint.configNode
    gemminiRoCC.observeNode.get := endpoint.observeNode
    endpoint.clockNode := sbus.fixedClockNode
    sbus.coupleTo("gemmini-job") {
      endpoint.controlNode := TLBuffer() := TLFragmenter(
        endpoint.controlNode.beatBytes,
        sbus.blockBytes) := TLWidthWidget(sbus) := _
    }
    endpoint
  }
}
