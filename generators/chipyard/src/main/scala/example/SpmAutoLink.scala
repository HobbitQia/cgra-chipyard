package chipyard.example

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams}

object SpmLinkStatus {
  val Width = 4
  val Success = 0.U(Width.W)
  val BadLink = 1.U(Width.W)
  val BadSlot = 2.U(Width.W)
  val BadLength = 3.U(Width.W)
  val SourceFailure = 4.U(Width.W)
  val SinkFailure = 5.U(Width.W)
  val IdentityMismatch = 6.U(Width.W)
}

case class SpmLinkSpec(source: String, destination: String)

case class SpmLinkParams(
  linkCount: Int,
  slotCount: Int,
  slotSizeBytes: Int,
  beatBytes: Int,
  lengthWidth: Int = 32,
  detailWidth: Int = 8,
  resultWidth: Int = 32) {
  require(linkCount > 0)
  require(slotCount > 0)
  require(slotSizeBytes > 0)
  require(isPow2(beatBytes))
  require(slotSizeBytes % beatBytes == 0)

  val linkWidth: Int = math.max(1, log2Ceil(linkCount))
  val slotWidth: Int = math.max(1, log2Ceil(slotCount))
}

case class SpmCommunicationRule(link: Int, slot: Int, bytes: Int)

case class SpmCommunicationTable(
  waitFor: Seq[SpmCommunicationRule],
  publishTo: Seq[SpmCommunicationRule]) {
  val depth: Int = waitFor.size + publishTo.size

  def validate(params: SpmLinkParams): Unit = {
    (waitFor ++ publishTo).foreach { rule =>
      require(rule.link >= 0 && rule.link < params.linkCount)
      require(rule.slot >= 0 && rule.slot < params.slotCount)
      require(rule.bytes > 0 && rule.bytes <= params.slotSizeBytes)
      require(rule.bytes % params.beatBytes == 0)
    }
  }
}

case class SpmEndpointSpec(name: String, table: SpmCommunicationTable)

class SpmLinkEvent(params: SpmLinkParams) extends Bundle {
  val link = UInt(params.linkWidth.W)
  val slot = UInt(params.slotWidth.W)
  val bytes = UInt(params.lengthWidth.W)
  val status = UInt(SpmLinkStatus.Width.W)
  val detail = UInt(params.detailWidth.W)
  val data = UInt(params.resultWidth.W)
}

/** Common AutoLink interface exposed by IP adapters. */
class SpmEndpointIO(params: SpmLinkParams) extends Bundle {
  val produced = Decoupled(new SpmLinkEvent(params))
  val deliver = Flipped(Decoupled(new SpmLinkEvent(params)))
  val done = Decoupled(new SpmLinkEvent(params))
}

class SpmEndpointAsyncLink(params: SpmLinkParams) extends Bundle {
  private val crossing = AsyncQueueParams.singleton()
  val produced = Flipped(new AsyncBundle(new SpmLinkEvent(params), crossing))
  val deliver = new AsyncBundle(new SpmLinkEvent(params), crossing)
  val done = Flipped(new AsyncBundle(new SpmLinkEvent(params), crossing))
}

/** Connects one elaborated producer-to-consumer communication link. */
class SpmAutoLink(params: SpmLinkParams, link: Int) extends Module {
  require(link >= 0 && link < params.linkCount)

  val io = IO(new Bundle {
    val produced = Flipped(Decoupled(new SpmLinkEvent(params)))
    val deliver = Decoupled(new SpmLinkEvent(params))
    val done = Flipped(Decoupled(new SpmLinkEvent(params)))
    val result = Decoupled(new SpmLinkEvent(params))
  })

  val Seq(idle, send, waitDone, result) = Enum(4)
  val state = RegInit(idle)
  val event = Reg(new SpmLinkEvent(params))

  val validSource = io.produced.bits.link === link.U &&
    io.produced.bits.slot < params.slotCount.U &&
    io.produced.bits.bytes =/= 0.U &&
    io.produced.bits.bytes <= params.slotSizeBytes.U &&
    (io.produced.bits.bytes & (params.beatBytes - 1).U) === 0.U

  io.produced.ready := state === idle
  io.deliver.valid := state === send
  io.deliver.bits := event
  io.done.ready := state === waitDone
  io.result.valid := state === result
  io.result.bits := event

  when(io.produced.fire) {
    event := io.produced.bits
    when(!validSource) {
      event.status := Mux(
        io.produced.bits.link =/= link.U,
        SpmLinkStatus.BadLink,
        Mux(io.produced.bits.slot >= params.slotCount.U, SpmLinkStatus.BadSlot, SpmLinkStatus.BadLength))
      event.detail := 0.U
      event.data := 0.U
      state := result
    }.elsewhen(io.produced.bits.status =/= SpmLinkStatus.Success) {
      event.status := SpmLinkStatus.SourceFailure
      state := result
    }.otherwise {
      state := send
    }
  }

  when(io.deliver.fire) {
    state := waitDone
  }

  when(io.done.fire) {
    event := io.done.bits
    when(io.done.bits.link =/= event.link ||
      io.done.bits.slot =/= event.slot ||
      io.done.bits.bytes =/= event.bytes) {
      event.status := SpmLinkStatus.IdentityMismatch
      event.detail := 0.U
      event.data := 0.U
    }.elsewhen(io.done.bits.status =/= SpmLinkStatus.Success) {
      event.status := SpmLinkStatus.SinkFailure
    }
    state := result
  }

  when(io.result.fire) {
    state := idle
  }
}
