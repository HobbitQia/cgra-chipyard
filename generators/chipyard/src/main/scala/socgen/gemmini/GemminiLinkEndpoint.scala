package chipyard.socgen.gemmini

import chisel3._
import chisel3.util._
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

case class GemminiLinkAttachParams(adapter: GemminiLinkParams, portName: String,
    controlAddress: BigInt = CgraLinkControlGenerated.gemminiJobAddress) {
  require(adapter.auto.endpoints.exists(_.name == portName))
  require(adapter.endpoint == portName)
}

case object GemminiLinkKey extends Field[Option[GemminiLinkAttachParams]](None)

class WithGemminiLink(params: GemminiLinkAttachParams) extends Config((_, _, _) => { case GemminiLinkKey => Some(params) })

/** Collects write errors in the endpoint's bus clock domain. */
object GemminiPublication {
  def apply(params: GemminiLinkParams, control: DecoupledIO[GemminiPublicationControl],
      reply: DecoupledIO[GemminiPublicationReply], acks: Seq[ValidIO[GemminiLinkAck]]): Unit = {
    val active = RegInit(false.B)
    val detail = RegInit(0.U(params.auto.detailWidth.W))
    val pending = RegInit(false.B)
    val result = Reg(new GemminiPublicationReply(params))
    val error = acks.foldLeft(detail) { (previous, ack) =>
      Mux(active && ack.valid && previous === 0.U,
        Mux(ack.bits.denied, GemminiLinkStatus.Denied.U,
          Mux(ack.bits.corrupt, GemminiLinkStatus.Corrupt.U, 0.U)), previous)
    }

    control.ready := !pending
    reply.valid := pending
    reply.bits := result
    detail := error

    when(reply.fire) {
      pending := false.B
    }
    when(control.fire) {
      active := control.bits.enable
      detail := Mux(control.bits.enable, 0.U, error)
      pending := true.B
      result.detail := Mux(control.bits.enable, 0.U, error)
    }
  }
}

/** SoC attachment and CPU-visible registers for the Gemmini AutoLink adapter. */
class GemminiLinkEndpoint(gemminiRoCC: GemminiRoCC, params: GemminiLinkParams, address: BigInt)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  private val gemminiAccelerator = gemminiRoCC.accelerator
  val configNode = BundleBridgeSource(() => new GemminiLinkConfigAsync(params))
  val observeNode = BundleBridgeSource(() => new GemminiLinkObserveAsync(params))
  val writerNode = TLAdapterNode()
  val dmaNode = TLAdapterNode()
  private val device = new SimpleDevice("gemmini-job", Seq("coredac,gemmini-job"))
  val controlNode = TLRegisterNode(
    address = Seq(AddressSet(
      address,
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
      val ports = Seq(writerNode, dmaNode)
      val acks = ports.map(_ => Wire(Valid(new GemminiLinkAck)))
      ports.zipWithIndex.foreach { case (node, index) =>
        val (in, _) = node.in.head
        val (out, _) = node.out.head
        out <> in

        val ack = acks(index)
        ack.valid := out.d.fire && out.d.bits.opcode === TLMessages.AccessAck
        ack.bits.denied := out.d.bits.denied
        ack.bits.corrupt := out.d.bits.corrupt
      }

      configLink.config <> ToAsyncBundle(configOut, AsyncQueueParams.singleton())
      val publicationControl = FromAsyncBundle(observe.control)
      val publicationReply = Wire(Decoupled(new GemminiPublicationReply(params)))
      GemminiPublication(params, publicationControl, publicationReply, acks)
      observe.reply <> ToAsyncBundle(publicationReply, AsyncQueueParams.singleton())

      val job = RegInit(0.U(32.W))
      val commandCount = RegInit(0.U(32.W))
      val patchCount = RegInit(0.U(32.W))
      val window = RegInit(0.U.asTypeOf(new GemminiWindow))
      val patch = RegInit(0.U.asTypeOf(new GemminiPatchEntry))
      val patchPush = Wire(Decoupled(UInt(1.W)))
      val patchOut = Wire(Decoupled(new GemminiPatchEntry))
      patchOut.valid := patchPush.valid && patchPush.bits.asBool
      patchOut.bits := patch
      patchPush.ready := !patchPush.bits.asBool || patchOut.ready
      configLink.patch <> ToAsyncBundle(patchOut, AsyncQueueParams.singleton())
      val configSubmit = Wire(Decoupled(UInt(1.W)))
      val configPending = RegInit(false.B)
      val configReady = RegInit(false.B)
      val configDone = RegInit(false.B)
      val configStatus = RegInit(AutoLinkStatus.Success)
      val configDetail = RegInit(0.U(params.auto.detailWidth.W))
      configOut.valid := configSubmit.valid && configSubmit.bits.asBool && !configPending
      configOut.bits.job := job
      configOut.bits.commandCount := commandCount
      configOut.bits.patchCount := patchCount
      configOut.bits.window := window
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
        GEMMINI_PATCH_COUNT -> Seq(RegField(32, patchCount)),
        GEMMINI_WINDOW_ROWS -> Seq(RegField(32, window.region.rows)),
        GEMMINI_WINDOW_COLUMNS -> Seq(RegField(32, window.region.columns)),
        GEMMINI_WINDOW_ROW_STEP -> Seq(RegField(32, window.region.rowStep)),
        GEMMINI_WINDOW_COLUMN_STEP -> Seq(RegField(32, window.region.columnStep)),
        GEMMINI_WINDOW_TOP -> Seq(RegField(32, window.region.top)),
        GEMMINI_WINDOW_BOTTOM -> Seq(RegField(32, window.region.bottom)),
        GEMMINI_WINDOW_LEFT -> Seq(RegField(32, window.region.left)),
        GEMMINI_WINDOW_RIGHT -> Seq(RegField(32, window.region.right)),
        GEMMINI_WINDOW_ADDRESS -> Seq(RegField(64, window.address)),
        GEMMINI_WINDOW_PIXEL_BYTES -> Seq(RegField(32, window.pixelBytes)),
        GEMMINI_WINDOW_ROW_BYTES -> Seq(RegField(32, window.rowBytes)),
        GEMMINI_PATCH_COMMAND -> Seq(RegField(32, patch.command)),
        GEMMINI_PATCH_OPERAND -> Seq(RegField(1, patch.operand)),
        GEMMINI_PATCH_LSB -> Seq(RegField(6, patch.lsb)),
        GEMMINI_PATCH_WIDTH -> Seq(RegField(7, patch.bitCount)),
        GEMMINI_PATCH_SOURCE -> Seq(RegField(4, patch.source)),
        GEMMINI_PATCH_SCALE -> Seq(RegField(32, patch.scale)),
        GEMMINI_PATCH_OFFSET -> Seq(RegField(32, patch.offset)),
        GEMMINI_PATCH_PUSH -> Seq(RegField.w(1, patchPush)),
        GEMMINI_CONFIG_DONE -> Seq(RegField.r(1, configDone)))
    }
  }
}

trait CanHaveGemminiLink {
  this: BaseSubsystem with InstantiatesHierarchicalElements with CanHaveAutoLink with CanHaveGemminiExternalSpm =>
  private val sbus = locateTLBusWrapper(SBUS)

  val gemminiLink = gemminiExternalSpm.flatMap { spm =>
    spm.gemminiRoCC.linkAttach.map(attach => (spm, attach))
  }.map { case (externalSpm, attach) =>
    val params = attach.adapter
    require(externalSpm.readBeatBytes == params.auto.beatBytes)
    val gemminiRoCC = externalSpm.gemminiRoCC
    val endpoint = LazyModule(new GemminiLinkEndpoint(gemminiRoCC, params, attach.controlAddress))

    require(gemminiRoCC.autoNode.nonEmpty)
    require(gemminiRoCC.configNode.nonEmpty)
    require(gemminiRoCC.observeNode.nonEmpty)

    externalSpm.writerNode := endpoint.writerNode
    gemminiRoCC.dmaNode := endpoint.dmaNode := gemminiRoCC.dmaSourceNode
    gemminiRoCC.autoNode.get := autoLink.get.endpoint(attach.portName)
    gemminiRoCC.configNode.get := endpoint.configNode
    gemminiRoCC.observeNode.get := endpoint.observeNode
    endpoint.clockNode := sbus.fixedClockNode
    sbus.coupleTo(s"${attach.portName}-job") {
      endpoint.controlNode := TLBuffer() := TLFragmenter(
        endpoint.controlNode.beatBytes,
        sbus.blockBytes) := TLWidthWidget(sbus) := _
    }
    endpoint
  }
}
