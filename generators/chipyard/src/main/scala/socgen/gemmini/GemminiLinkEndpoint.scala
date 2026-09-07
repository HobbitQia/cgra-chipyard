package chipyard.socgen.gemmini

import chisel3._
import chisel3.util.{Decoupled, Queue, RRArbiter, log2Ceil}
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
  val localNode = TLAdapterNode()
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
      val ports = Seq(writerNode -> false, localNode -> true)
      val events = Module(new RRArbiter(new GemminiLinkEvent(params), ports.size))
      ports.zipWithIndex.foreach { case ((node, local), index) =>
        val (in, edge) = node.in.head
        val (out, _) = node.out.head
        val queue = Module(new Queue(new GemminiLinkEvent(params), 2))
        val event = queue.io.enq
        events.io.in(index) <> queue.io.deq
        require(params.beatBytes % edge.manager.beatBytes == 0)
        require(edge.bundle.sourceBits <= event.bits.write.source.getWidth)

        val isWrite = in.a.bits.opcode === TLMessages.PutFullData ||
          in.a.bits.opcode === TLMessages.PutPartialData
        val isWriteAck = out.d.bits.opcode === TLMessages.AccessAck
        val selectAck = out.d.valid && isWriteAck
        val writeReady = event.ready && !selectAck
        in.b <> out.b
        out.c <> in.c
        out.e <> in.e
        out.a.bits := in.a.bits
        out.a.valid := in.a.valid && (!isWrite || writeReady)
        in.a.ready := out.a.ready && (!isWrite || writeReady)
        in.d.bits := out.d.bits
        in.d.valid := out.d.valid && (!isWriteAck || event.ready)
        out.d.ready := in.d.ready && (!isWriteAck || event.ready)

        event.valid := Mux(selectAck, in.d.ready, in.a.valid && isWrite && out.a.ready)
        event.bits := 0.U.asTypeOf(new GemminiLinkEvent(params))
        event.bits.local := local.B
        event.bits.isAck := selectAck
        event.bits.write.address := in.a.bits.address
        event.bits.write.source := in.a.bits.source
        event.bits.write.size := in.a.bits.size
        event.bits.write.opcode := in.a.bits.opcode
        val lane = in.a.bits.address(log2Ceil(params.beatBytes) - 1, 0) &
          (params.beatBytes - edge.manager.beatBytes).U
        event.bits.write.mask := in.a.bits.mask << lane
        event.bits.ack.source := out.d.bits.source
        event.bits.ack.size := out.d.bits.size
        event.bits.ack.denied := out.d.bits.denied
        event.bits.ack.corrupt := out.d.bits.corrupt
      }

      configLink.config <> ToAsyncBundle(configOut, AsyncQueueParams.singleton())
      observe.event <> ToAsyncBundle(events.io.out, AsyncQueueParams.singleton())

      val job = RegInit(0.U(32.W))
      val commandCount = RegInit(0.U(32.W))
      val configSubmit = Wire(Decoupled(UInt(1.W)))
      val configPending = RegInit(false.B)
      val configReady = RegInit(false.B)
      val configDone = RegInit(false.B)
      val configStatus = RegInit(AutoLinkStatus.Success)
      val configDetail = RegInit(0.U(params.auto.detailWidth.W))
      configOut.valid := configSubmit.valid && configSubmit.bits.asBool && !configPending
      configOut.bits.job := job
      configOut.bits.commandCount := commandCount
      configSubmit.ready := Mux(configSubmit.bits.asBool, configOut.ready && !configPending, true.B)
      configAck.ready := true.B

      when(configOut.fire) {
        configPending := true.B
        configReady := false.B
        configDone := false.B
        configStatus := AutoLinkStatus.Success
        configDetail := 0.U
      }
      when(configAck.fire) {
        configPending := !configAck.bits.done
        configReady := true.B
        configDone := configAck.bits.done
        configStatus := configAck.bits.status
        configDetail := configAck.bits.detail
      }

      import CgraLinkControlGenerated._
      controlNode.regmap(
        GEMMINI_COMMAND_COUNT -> Seq(RegField(32, commandCount)),
        GEMMINI_SUBMIT -> Seq(RegField.w(1, configSubmit)),
        GEMMINI_CONFIG_READY -> Seq(RegField.r(1, configReady)),
        GEMMINI_CONFIG_STATUS -> Seq(RegField.r(32, configStatus)),
        GEMMINI_CONFIG_DETAIL -> Seq(RegField.r(32, configDetail)),
        GEMMINI_SELECT -> Seq(RegField(32, job)),
        GEMMINI_CONFIG_DONE -> Seq(RegField.r(1, configDone)))
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
    externalSpm.localNode := endpoint.localNode := gemminiRoCC.localNode
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
