package chipyard.example

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters

object GemminiSpmStatus {
  val BadRequest = 1
  val BadAddress = 2
  val BadBeat = 3
  val BadOrder = 4
  val Denied = 5
  val Corrupt = 6
}

case class GemminiSpmParams(slotBases: Seq[BigInt], slotSizeBytes: Int, beatBytes: Int) {
  require(slotBases.nonEmpty)
  require(slotSizeBytes > 0)
  require(isPow2(beatBytes))
  require(slotSizeBytes % beatBytes == 0)
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

/** Adapts one serialized Gemmini STORE_SPAD publication to the common
  * producer stage.
  */
class GemminiSpmProducer(protocol: SpmDmaParams, params: GemminiSpmParams) extends Module {
  val io = IO(new Bundle {
    val start = Flipped(Decoupled(new SpmDmaCommand(protocol)))
    val write = Flipped(Valid(new GemminiSpmWrite(params)))
    val ack = Flipped(Valid(new GemminiSpmAck))
    val done = Decoupled(new SpmDmaResult(protocol))
  })

  val active = RegInit(false.B)
  val command = Reg(new SpmDmaCommand(protocol))
  val issuedBytes = RegInit(0.U(protocol.lengthWidth.W))
  val acknowledgedBytes = RegInit(0.U(protocol.lengthWidth.W))
  val outstanding = RegInit(false.B)
  val source = Reg(UInt(16.W))
  val beatValid = RegInit(false.B)
  val beatError = RegInit(0.U(protocol.detailWidth.W))
  val doneValid = RegInit(false.B)
  val doneDetail = RegInit(0.U(protocol.detailWidth.W))

  val slotBases = VecInit(params.slotBases.map(_.U(64.W)))
  val expectedSize = log2Ceil(params.beatBytes).U
  val fullMask = ((BigInt(1) << params.beatBytes) - 1).U
  val startValid = io.start.bits.slot < params.slotBases.size.U &&
    io.start.bits.bytes =/= 0.U && io.start.bits.bytes <= params.slotSizeBytes.U &&
    (io.start.bits.bytes & (params.beatBytes - 1).U) === 0.U

  io.start.ready := !active && !doneValid
  io.done.valid := doneValid
  io.done.bits.jobId := command.jobId
  io.done.bits.slot := command.slot
  io.done.bits.bytes := command.bytes
  io.done.bits.stage := SpmDmaStage.Producer
  io.done.bits.status := Mux(doneDetail === 0.U, SpmDmaStatus.Success, SpmDmaStatus.StageFailure)
  io.done.bits.detail := doneDetail
  io.done.bits.data := 0.U

  def finish(detail: UInt): Unit = {
    doneValid := true.B
    doneDetail := detail
  }

  when(io.start.fire) {
    active := true.B
    command := io.start.bits
    issuedBytes := 0.U
    acknowledgedBytes := 0.U
    outstanding := false.B
    when(!startValid) {
      finish(GemminiSpmStatus.BadRequest.U)
    }
  }

  when(active && !doneValid && io.write.valid) {
    when(outstanding) {
      beatValid := false.B
      beatError := GemminiSpmStatus.BadOrder.U
    }.otherwise {
      val expectedAddress = slotBases(command.slot) + issuedBytes
      val addressValid = io.write.bits.address === expectedAddress
      val shapeValid = io.write.bits.opcode === TLMessages.PutFullData &&
        io.write.bits.size === expectedSize && io.write.bits.mask === fullMask
      val withinRequest = issuedBytes < command.bytes
      outstanding := true.B
      source := io.write.bits.source
      beatValid := addressValid && shapeValid && withinRequest
      beatError := Mux(
        !addressValid,
        GemminiSpmStatus.BadAddress.U,
        Mux(!shapeValid, GemminiSpmStatus.BadBeat.U, GemminiSpmStatus.BadOrder.U))
      when(addressValid && shapeValid && withinRequest) {
        issuedBytes := issuedBytes + params.beatBytes.U
      }
    }
  }

  when(active && !doneValid && io.ack.valid) {
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
      }.elsewhen(nextBytes === command.bytes) {
        finish(0.U)
      }
    }
  }

  when(io.done.fire) {
    active := false.B
    doneValid := false.B
  }
}

/** Observes only Gemmini's dedicated spad_writer branch. */
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
