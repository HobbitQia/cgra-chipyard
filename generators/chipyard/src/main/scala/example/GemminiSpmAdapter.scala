package chipyard.example

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters

object GemminiSpmStatus {
  val BadAddress = 1
  val BadBeat = 2
  val BadOrder = 3
  val Denied = 4
  val Corrupt = 5
}

case class GemminiSpmParams(
  link: SpmLinkParams,
  table: SpmCommunicationTable,
  slotBases: Seq[BigInt],
  beatBytes: Int) {
  table.validate(link)
  require(table.waitFor.isEmpty)
  require(table.publishTo.size == 1)
  require(slotBases.size == link.slotCount)
  require(isPow2(beatBytes))

  val publication: SpmCommunicationRule = table.publishTo.head
}

class GemminiSpmWrite(params: GemminiSpmParams) extends Bundle {
  val address = UInt(64.W)
  val source = UInt(16.W)
  val size = UInt(8.W)
  val opcode = UInt(3.W)
  val mask = UInt(params.beatBytes.W)
}

class GemminiSpmAck extends Bundle {
  val source = UInt(16.W)
  val size = UInt(8.W)
  val denied = Bool()
  val corrupt = Bool()
}

/** Converts Gemmini's dedicated Shared SPM writer into an AutoLink publication. */
class GemminiSpmAdapter(params: GemminiSpmParams) extends Module {
  val io = IO(new Bundle {
    val write = Flipped(Valid(new GemminiSpmWrite(params)))
    val ack = Flipped(Valid(new GemminiSpmAck))
    val endpoint = new SpmEndpointIO(params.link)
  })

  val active = RegInit(false.B)
  val issuedBytes = RegInit(0.U(params.link.lengthWidth.W))
  val acknowledgedBytes = RegInit(0.U(params.link.lengthWidth.W))
  val outstanding = RegInit(false.B)
  val source = Reg(UInt(16.W))
  val beatValid = RegInit(false.B)
  val beatError = RegInit(0.U(params.link.detailWidth.W))
  val producedValid = RegInit(false.B)
  val producedDetail = RegInit(0.U(params.link.detailWidth.W))

  val publication = params.publication
  val slotBases = VecInit(params.slotBases.map(_.U(64.W)))
  val expectedSize = log2Ceil(params.beatBytes).U
  val fullMask = ((BigInt(1) << params.beatBytes) - 1).U

  io.endpoint.produced.valid := producedValid
  io.endpoint.produced.bits.link := publication.link.U
  io.endpoint.produced.bits.slot := publication.slot.U
  io.endpoint.produced.bits.bytes := publication.bytes.U
  io.endpoint.produced.bits.status := Mux(
    producedDetail === 0.U,
    SpmLinkStatus.Success,
    SpmLinkStatus.SourceFailure)
  io.endpoint.produced.bits.detail := producedDetail
  io.endpoint.produced.bits.data := 0.U
  io.endpoint.deliver.ready := false.B
  io.endpoint.done.valid := false.B
  io.endpoint.done.bits := 0.U.asTypeOf(new SpmLinkEvent(params.link))

  def finish(detail: UInt): Unit = {
    producedValid := true.B
    producedDetail := detail
  }

  when(!producedValid && io.write.valid) {
    val expectedAddress = slotBases(publication.slot) + issuedBytes
    val addressValid = io.write.bits.address === expectedAddress
    val shapeValid = io.write.bits.opcode === TLMessages.PutFullData &&
      io.write.bits.size === expectedSize && io.write.bits.mask === fullMask
    val withinPublication = issuedBytes < publication.bytes.U

    active := true.B
    when(outstanding) {
      beatValid := false.B
      beatError := GemminiSpmStatus.BadOrder.U
    }.otherwise {
      outstanding := true.B
      source := io.write.bits.source
      beatValid := addressValid && shapeValid && withinPublication
      beatError := Mux(
        !addressValid,
        GemminiSpmStatus.BadAddress.U,
        Mux(!shapeValid, GemminiSpmStatus.BadBeat.U, GemminiSpmStatus.BadOrder.U))
      when(addressValid && shapeValid && withinPublication) {
        issuedBytes := issuedBytes + params.beatBytes.U
      }
    }
  }

  when(active && !producedValid && io.ack.valid) {
    when(!outstanding) {
      finish(GemminiSpmStatus.BadOrder.U)
    }.otherwise {
      val responseShapeValid = io.ack.bits.source === source && io.ack.bits.size === expectedSize
      val responseValid = beatValid && responseShapeValid && !io.ack.bits.denied && !io.ack.bits.corrupt
      val detail = Mux(
        io.ack.bits.denied,
        GemminiSpmStatus.Denied.U,
        Mux(
          io.ack.bits.corrupt,
          GemminiSpmStatus.Corrupt.U,
          Mux(!responseShapeValid, GemminiSpmStatus.BadBeat.U, beatError)))
      val nextBytes = Mux(responseValid, acknowledgedBytes + params.beatBytes.U, acknowledgedBytes)

      acknowledgedBytes := nextBytes
      outstanding := false.B
      when(!responseValid) {
        finish(detail)
      }.elsewhen(nextBytes === publication.bytes.U) {
        finish(0.U)
      }
    }
  }

  when(io.endpoint.produced.fire) {
    active := false.B
    issuedBytes := 0.U
    acknowledgedBytes := 0.U
    outstanding := false.B
    producedValid := false.B
  }
}

/** Observes only Gemmini's dedicated Shared SPM writer. */
class GemminiSpmMonitor(params: GemminiSpmParams)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  val node = TLAdapterNode()

  override lazy val module = new MonitorImpl
  class MonitorImpl extends Impl {
    val io = IO(new Bundle {
      val write = Valid(new GemminiSpmWrite(params))
      val ack = Valid(new GemminiSpmAck)
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
