package chipyard.socgen.gemmini

import chisel3._
import chisel3.util._
import chipyard.socgen.generated.CgraLinkControlGenerated
import chipyard.socgen.link.{AutoLinkStatus, AutoWatch, CanHaveAutoLink}
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

class GemminiPublicationEntry(params: GemminiLinkParams, paths: Int) extends Bundle {
  val valid = Bool()
  val path = UInt(math.max(1, log2Ceil(paths)).W)
  val source = UInt(16.W)
  val bytes = UInt(log2Ceil(params.beatBytes + 1).W)
  val size = UInt(8.W)
  val error = UInt(params.auto.detailWidth.W)
}

/** Elaborates publication accounting in the endpoint's bus clock domain. */
object GemminiPublication {
  def apply(params: GemminiLinkParams, control: DecoupledIO[GemminiPublicationControl],
      reply: DecoupledIO[GemminiPublicationReply], writes: Seq[ValidIO[GemminiLinkWrite]],
      acks: Seq[ValidIO[GemminiLinkAck]]): Unit = {
    require(writes.size == acks.size)
    val watch = Reg(new AutoWatch(params.auto))
    val active = RegInit(false.B)
    val cancel = RegInit(false.B)
    val ackPending = RegInit(false.B)
    val resultValid = RegInit(false.B)
    val resultDetail = Reg(UInt(params.auto.detailWidth.W))
    val pending = RegInit(VecInit(Seq.fill(params.maxInflight)(
      0.U.asTypeOf(new GemminiPublicationEntry(params, writes.size)))))
    val coverage = RegInit(0.U(params.publicationBytes.W))
    val acknowledged = RegInit(0.U((params.auto.lengthWidth + 1).W))

    control.ready := !ackPending && !resultValid && !cancel &&
      (!control.bits.enable || (!active && !VecInit(pending.map(_.valid)).asUInt.orR))
    reply.valid := ackPending || resultValid
    reply.bits.result := !ackPending
    reply.bits.detail := Mux(ackPending, 0.U, resultDetail)

    // Retire acknowledgements before allocating writes so both paths can reuse sources each cycle.
    var table = pending
    var covered = coverage
    var count = acknowledged
    var error = 0.U(params.auto.detailWidth.W)
    def retire(index: Int, valid: Bool): Unit = {
      val ack = acks(index).bits
      val matches = VecInit(table.map(entry => entry.valid &&
        entry.path === index.U && entry.source === ack.source))
      val slot = PriorityEncoder(matches.asUInt)
      val entry = table(slot)
      val next = WireDefault(table)
      val detail = Mux(!matches.asUInt.orR, GemminiLinkStatus.BadOrder.U,
        Mux(ack.denied, GemminiLinkStatus.Denied.U,
          Mux(ack.corrupt, GemminiLinkStatus.Corrupt.U,
            Mux(ack.size =/= entry.size, GemminiLinkStatus.BadBeat.U, entry.error))))
      when(valid && matches.asUInt.orR) {
        next(slot).valid := false.B
      }
      error = Mux(valid && active && !resultValid && error === 0.U, detail, error)
      count = count + Mux(valid && matches.asUInt.orR && detail === 0.U, entry.bytes, 0.U)
      table = next
    }
    acks.indices.foreach(index => retire(index, acks(index).valid))

    writes.zipWithIndex.foreach { case (request, index) =>
      val write = request.bits
      val firstLane = PriorityEncoder(write.mask)
      val byteCount = PopCount(write.mask)
      val beatAddress = write.address & (~(params.beatBytes - 1).U(64.W))
      val firstAddress = beatAddress + firstLane
      val offset = firstAddress - watch.address
      val addressValid = firstAddress >= watch.address && offset < watch.bytes
      val requestBytes = MuxLookup(write.size, 0.U(log2Ceil(params.beatBytes + 1).W))(
        (0 to log2Ceil(params.beatBytes)).map(size => size.U -> (1 << size).U))
      val laneOffset = write.address & (params.beatBytes - 1).U
      val requestMask = VecInit((0 until params.beatBytes).map(lane =>
        lane.U >= laneOffset && lane.U < laneOffset + requestBytes)).asUInt
      val shiftedMask = write.mask >> firstLane
      val contiguous = write.mask.orR && (shiftedMask & (shiftedMask +& 1.U)) === 0.U
      val requestValid = requestBytes =/= 0.U && (write.address & (requestBytes - 1.U)) === 0.U
      val opcodeValid = (write.opcode === TLMessages.PutFullData && write.mask === requestMask) ||
        write.opcode === TLMessages.PutPartialData
      val shapeValid = requestValid && opcodeValid && contiguous && (write.mask & ~requestMask) === 0.U
      val endOffset = offset +& byteCount
      val written = VecInit((0 until params.publicationBytes).map(byte =>
        byte.U >= offset && byte.U < endOffset)).asUInt
      val overlap = (written & covered).orR
      val matches = VecInit(table.map(entry => entry.valid &&
        entry.path === index.U && entry.source === write.source))
      val free = VecInit(table.map(entry => !entry.valid))
      val slot = PriorityEncoder(free.asUInt)
      val detail = Mux(!addressValid, GemminiLinkStatus.BadAddress.U,
        Mux(!shapeValid, GemminiLinkStatus.BadBeat.U,
          Mux(endOffset > watch.bytes || overlap, GemminiLinkStatus.BadOrder.U, 0.U)))
      val accept = request.valid && active && !resultValid
      val next = WireDefault(table)
      when(accept) {
        when(matches.asUInt.orR) {
          next(PriorityEncoder(matches.asUInt)).error := GemminiLinkStatus.BadOrder.U
        }.elsewhen(free.asUInt.orR) {
          next(slot).valid := true.B
          next(slot).path := index.U
          next(slot).source := write.source
          next(slot).bytes := byteCount
          next(slot).size := write.size
          next(slot).error := detail
        }
      }
      covered = Mux(accept && !matches.asUInt.orR && free.asUInt.orR && detail === 0.U,
        covered | written, covered)
      error = Mux(accept && !matches.asUInt.orR && !free.asUInt.orR && error === 0.U,
        GemminiLinkStatus.BadOrder.U, error)
      table = next
    }
    pending := table
    coverage := covered
    acknowledged := count

    val cancelFire = control.fire && !control.bits.enable
    when(active && !resultValid && !cancelFire &&
        (error =/= 0.U || (count === watch.bytes && !VecInit(table.map(_.valid)).asUInt.orR))) {
      resultValid := true.B
      resultDetail := error
    }
    when(reply.fire) {
      when(ackPending) {
        ackPending := false.B
      }.otherwise {
        resultValid := false.B
        active := false.B
      }
    }
    when(control.fire) {
      when(control.bits.enable) {
        watch := control.bits.watch
        active := true.B
        coverage := 0.U
        acknowledged := 0.U
        ackPending := true.B
      }.otherwise {
        active := false.B
        cancel := true.B
      }
    }
    when(cancel && !VecInit(table.map(_.valid)).asUInt.orR) {
      cancel := false.B
      ackPending := true.B
    }
  }
}

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
      val ports = Seq(writerNode, localNode)
      val writes = ports.map(_ => Wire(Valid(new GemminiLinkWrite(params))))
      val acks = ports.map(_ => Wire(Valid(new GemminiLinkAck)))
      ports.zipWithIndex.foreach { case (node, index) =>
        val (in, edge) = node.in.head
        val (out, _) = node.out.head
        require(params.beatBytes % edge.manager.beatBytes == 0)
        require(edge.manager.minLatency > 0)
        require(edge.bundle.sourceBits <= writes(index).bits.source.getWidth)
        out <> in

        val write = writes(index)
        write.valid := in.a.fire && (in.a.bits.opcode === TLMessages.PutFullData ||
          in.a.bits.opcode === TLMessages.PutPartialData)
        write.bits.address := in.a.bits.address
        write.bits.source := in.a.bits.source
        write.bits.size := in.a.bits.size
        write.bits.opcode := in.a.bits.opcode
        val lane = in.a.bits.address(log2Ceil(params.beatBytes) - 1, 0) &
          (params.beatBytes - edge.manager.beatBytes).U
        write.bits.mask := in.a.bits.mask << lane

        val ack = acks(index)
        ack.valid := out.d.fire && out.d.bits.opcode === TLMessages.AccessAck
        ack.bits.source := out.d.bits.source
        ack.bits.size := out.d.bits.size
        ack.bits.denied := out.d.bits.denied
        ack.bits.corrupt := out.d.bits.corrupt
      }

      configLink.config <> ToAsyncBundle(configOut, AsyncQueueParams.singleton())
      val publicationControl = FromAsyncBundle(observe.control)
      val publicationReply = Wire(Decoupled(new GemminiPublicationReply(params)))
      GemminiPublication(params, publicationControl, publicationReply, writes, acks)
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
