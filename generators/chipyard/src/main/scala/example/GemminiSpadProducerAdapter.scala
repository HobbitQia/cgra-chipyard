package chipyard.example

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters

/** Producer-local status values carried by SpmTransferReady.status. */
object GemminiSpadProducerStatus {
  val Success = 0
  val UnsupportedRequest = 1
  val UnexpectedAddress = 2
  val UnexpectedSize = 3
  val UnexpectedOrder = 4
  val Denied = 5
  val Corrupt = 6
  val SourceMismatch = 7
}

case class GemminiSpadProducerAdapterParams(
  slotBases: Seq[BigInt],
  slotSizeBytes: Int,
  fullWidthRowBytes: Int) {
  require(slotBases.nonEmpty)
  require(slotBases.distinct.size == slotBases.size)
  require(slotSizeBytes > 0 && isPow2(slotSizeBytes))
  require(fullWidthRowBytes > 0 && isPow2(fullWidthRowBytes))
  require(slotSizeBytes % fullWidthRowBytes == 0)
  require(slotBases.forall(_ % slotSizeBytes == 0))
}

object GemminiSpadProducerAdapterParams {
  val production: GemminiSpadProducerAdapterParams =
    GemminiSpadProducerAdapterParams(
      slotBases = GemminiExternalSpadGenerated.outputSlotBases,
      slotSizeBytes = GemminiExternalSpadGenerated.outputSlotSizeBytes,
      fullWidthRowBytes = GemminiExternalSpadGenerated.fullWidthRowBytes)
}

/** One accepted, manager-width A beat from Gemmini's dedicated spad_writer. */
class GemminiSpadWriterAEvent(params: GemminiSpadProducerAdapterParams)
    extends Bundle {
  val address = UInt(64.W)
  val source = UInt(16.W)
  val size = UInt(8.W)
  val opcode = UInt(3.W)
  val mask = UInt(params.fullWidthRowBytes.W)
}

/** One accepted D acknowledgement returned to Gemmini's spad_writer. */
class GemminiSpadWriterDEvent extends Bundle {
  val source = UInt(16.W)
  val size = UInt(8.W)
  val denied = Bool()
  val corrupt = Bool()
}

/** Correlates one reserved endpoint request with one serialized Gemmini publish.
  *
  * The adapter does not launch Gemmini. The CPU or a later producer-local
  * command adapter submits the existing MVOUT_SPAD command after this adapter
  * accepts requestIn. The dedicated TileLink branch monitor supplies only real
  * spad_writer A handshakes and D handshakes; ordinary external-SPAD writes are
  * structurally outside this observation path.
  *
  * T4 supports an exact-length contract: maxBytes is the requested publication
  * length and must be a whole number of full-width accumulator rows. This makes
  * the final acknowledged row explicit without a timer, busy bit, or fence.
  */
class GemminiSpadProducerAdapter(params: GemminiSpadProducerAdapterParams)
    extends Module {
  val io = IO(new Bundle {
    val requestIn = Flipped(Decoupled(new SpmTransferRequest))
    val writerA = Flipped(Valid(new GemminiSpadWriterAEvent(params)))
    val writerD = Flipped(Valid(new GemminiSpadWriterDEvent))
    val readyOut = Decoupled(new SpmTransferReady)

    val active = Output(Bool())
    val acknowledgedBytes = Output(UInt(SpmTransferProtocol.LengthWidth.W))
    val issuedBytes = Output(UInt(SpmTransferProtocol.LengthWidth.W))
    val rowOutstanding = Output(Bool())
    val finalRowOutstanding = Output(Bool())
  })

  import GemminiSpadProducerStatus._
  import SpmTransferProtocol._

  private val slotIndexWidth = math.max(1, log2Ceil(params.slotBases.size))
  private val expectedSize = log2Ceil(params.fullWidthRowBytes).U(8.W)
  private val fullMask = ((BigInt(1) << params.fullWidthRowBytes) - 1).U
  private val slotBases = VecInit(params.slotBases.map(_.U(64.W)))

  private val active = RegInit(false.B)
  private val jobId = Reg(UInt(JobIdWidth.W))
  private val slot = Reg(UInt(SlotIdWidth.W))
  private val expectedBytes = Reg(UInt(LengthWidth.W))
  private val issuedBytes = RegInit(0.U(LengthWidth.W))
  private val acknowledgedBytes = RegInit(0.U(LengthWidth.W))
  private val rowOutstanding = RegInit(false.B)
  private val rowSource = Reg(UInt(16.W))
  private val rowWellFormed = RegInit(false.B)
  private val rowFailureStatus = RegInit(Success.U(StatusWidth.W))

  private val readyValid = RegInit(false.B)
  private val readyBits = Reg(new SpmTransferReady)
  private val successfulFinalAckSeen = RegInit(false.B)

  io.requestIn.ready := !active && !readyValid
  io.readyOut.valid := readyValid
  io.readyOut.bits := readyBits
  io.active := active
  io.acknowledgedBytes := acknowledgedBytes
  io.issuedBytes := issuedBytes
  io.rowOutstanding := rowOutstanding
  io.finalRowOutstanding :=
    active && !readyValid && rowOutstanding && issuedBytes === expectedBytes

  private val requestSlotValid = io.requestIn.bits.slot < params.slotBases.size.U
  private val requestSlot = io.requestIn.bits.slot(slotIndexWidth - 1, 0)
  private val requestLengthValid =
    io.requestIn.bits.maxBytes =/= 0.U &&
      io.requestIn.bits.maxBytes <= params.slotSizeBytes.U &&
      (io.requestIn.bits.maxBytes & (params.fullWidthRowBytes - 1).U) === 0.U
  private val requestValid =
    requestSlotValid && io.requestIn.bits.jobId =/= 0.U && requestLengthValid

  private def generateReady(actualBytes: UInt, status: UInt): Unit = {
    assert(active)
    assert(!readyValid)
    readyValid := true.B
    readyBits.jobId := jobId
    readyBits.slot := slot
    readyBits.actualBytes := actualBytes
    readyBits.status := status
  }

  when(io.requestIn.fire) {
    active := true.B
    jobId := io.requestIn.bits.jobId
    slot := io.requestIn.bits.slot
    expectedBytes := io.requestIn.bits.maxBytes
    issuedBytes := 0.U
    acknowledgedBytes := 0.U
    rowOutstanding := false.B
    rowWellFormed := false.B
    rowFailureStatus := Success.U
    successfulFinalAckSeen := false.B

    when(!requestValid) {
      readyValid := true.B
      readyBits.jobId := io.requestIn.bits.jobId
      readyBits.slot := io.requestIn.bits.slot
      readyBits.actualBytes := 0.U
      readyBits.status := UnsupportedRequest.U
    }
  }

  when(active && !readyValid && io.writerA.valid) {
    when(rowOutstanding) {
      // The T2 manager cannot accept this sequence, but retain a sound
      // failure until the already outstanding row receives its real D ack.
      rowWellFormed := false.B
      rowFailureStatus := UnexpectedOrder.U
    }.otherwise {
      val expectedAddress = slotBases(slot(slotIndexWidth - 1, 0)) + issuedBytes
      val addressMatches = io.writerA.bits.address === expectedAddress
      val shapeMatches =
        io.writerA.bits.opcode === TLMessages.PutFullData &&
          io.writerA.bits.size === expectedSize &&
          io.writerA.bits.mask === fullMask
      val withinRequest = issuedBytes < expectedBytes

      rowOutstanding := true.B
      rowSource := io.writerA.bits.source
      rowWellFormed := addressMatches && shapeMatches && withinRequest
      rowFailureStatus := Mux(
        !addressMatches,
        UnexpectedAddress.U,
        Mux(!shapeMatches, UnexpectedSize.U,
          Mux(!withinRequest, UnexpectedOrder.U, Success.U)))
      when(addressMatches && shapeMatches && withinRequest) {
        issuedBytes := issuedBytes + params.fullWidthRowBytes.U
      }
    }
  }

  when(active && !readyValid && io.writerD.valid) {
    when(!rowOutstanding) {
      generateReady(acknowledgedBytes, UnexpectedOrder.U)
    }.otherwise {
      val sourceMatches = io.writerD.bits.source === rowSource
      val sizeMatches = io.writerD.bits.size === expectedSize
      val responseStatus = Mux(
        !sourceMatches,
        SourceMismatch.U,
        Mux(!sizeMatches, UnexpectedSize.U,
          Mux(io.writerD.bits.denied, Denied.U,
            Mux(io.writerD.bits.corrupt, Corrupt.U, rowFailureStatus))))
      val responseSuccessful =
        rowWellFormed && sourceMatches && sizeMatches &&
          !io.writerD.bits.denied && !io.writerD.bits.corrupt
      val nextAcknowledgedBytes = Mux(
        responseSuccessful,
        acknowledgedBytes + params.fullWidthRowBytes.U,
        acknowledgedBytes)

      rowOutstanding := false.B
      rowWellFormed := false.B
      rowFailureStatus := Success.U
      acknowledgedBytes := nextAcknowledgedBytes

      when(responseStatus =/= Success.U) {
        generateReady(nextAcknowledgedBytes, responseStatus)
      }.elsewhen(nextAcknowledgedBytes === expectedBytes) {
        successfulFinalAckSeen := true.B
        generateReady(nextAcknowledgedBytes, Success.U)
      }
    }
  }

  when(io.readyOut.fire) {
    active := false.B
    readyValid := false.B
    rowOutstanding := false.B
  }

  // Current external-SPAD memory serialization guarantees at most one
  // manager-width publication A or D event in a cycle.
  assert(!(io.writerA.valid && io.writerD.valid))
  assert(acknowledgedBytes <= issuedBytes)
  assert(issuedBytes <= expectedBytes || !active)
  when(rowOutstanding) {
    assert(active)
  }
  when(readyValid) {
    assert(active)
    when(readyBits.status === Success.U) {
      assert(successfulFinalAckSeen)
      assert(readyBits.actualBytes === expectedBytes)
      assert(readyBits.actualBytes === acknowledgedBytes)
    }
  }

  private val readyBlocked = io.readyOut.valid && !io.readyOut.ready
  private val previousReadyBlocked = RegNext(readyBlocked, false.B)
  private val previousReadyBits = RegEnable(io.readyOut.bits.asUInt, readyBlocked)
  when(previousReadyBlocked) {
    assert(io.readyOut.valid)
    assert(io.readyOut.bits.asUInt === previousReadyBits)
  }
}

/** Identity adapter placed only on Gemmini's dedicated spad_writer branch.
  *
  * It observes the manager-width request and the D handshake returned to the
  * writer. Optional D stalling is validation-only and never enabled by the
  * production configuration.
  */
class GemminiSpadPublicationMonitor(
  params: GemminiSpadProducerAdapterParams,
  stallFinalAck: Boolean,
  stallCycles: Int)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  require(stallCycles >= 0)
  require(stallFinalAck == (stallCycles != 0))

  val node = TLAdapterNode()

  override lazy val module = new MonitorImpl
  class MonitorImpl extends Impl {
    val io = IO(new Bundle {
      val writerA = Valid(new GemminiSpadWriterAEvent(params))
      val writerD = Valid(new GemminiSpadWriterDEvent)
      val stallResponse = Input(Bool())
      val dBlocked = Output(Bool())
      val aFireCount = Output(UInt(32.W))
      val dFireCount = Output(UInt(32.W))
      val dBlockedCycleCount = Output(UInt(32.W))
      val lastAAddress = Output(UInt(64.W))
    })

    withClockAndReset(clock, reset) {
      require(node.in.size == 1 && node.out.size == 1)
      val ((in, inEdge), (out, outEdge)) = (node.in.head, node.out.head)
      require(inEdge.manager.beatBytes == params.fullWidthRowBytes)
      require(outEdge.manager.beatBytes == params.fullWidthRowBytes)
      require(in.a.bits.mask.getWidth == params.fullWidthRowBytes)

      out.a <> in.a
      in.b <> out.b
      out.c <> in.c
      out.e <> in.e

      val dFireCount = RegInit(0.U(32.W))
      val currentResponseStalled = RegInit(false.B)
      val stallRemainingWidth = math.max(1, log2Ceil(stallCycles + 1))
      val stallRemaining = RegInit(0.U(stallRemainingWidth.W))
      val targetResponse = if (stallFinalAck) {
        !currentResponseStalled && io.stallResponse && out.d.valid
      } else false.B
      val dBlocked = targetResponse || stallRemaining =/= 0.U

      in.d.valid := out.d.valid && !dBlocked
      in.d.bits := out.d.bits
      out.d.ready := in.d.ready && !dBlocked

      if (stallCycles > 0) {
        when(targetResponse) {
          currentResponseStalled := true.B
          stallRemaining := (stallCycles - 1).U
        }.elsewhen(stallRemaining =/= 0.U) {
          stallRemaining := stallRemaining - 1.U
        }
      }

      val aFireCount = RegInit(0.U(32.W))
      val dBlockedCycleCount = RegInit(0.U(32.W))
      val lastAAddress = RegInit(0.U(64.W))
      when(in.a.fire) {
        aFireCount := aFireCount + 1.U
        lastAAddress := in.a.bits.address
      }
      when(in.d.fire) {
        dFireCount := dFireCount + 1.U
        currentResponseStalled := false.B
      }
      when(dBlocked && out.d.valid) {
        dBlockedCycleCount := dBlockedCycleCount + 1.U
      }

      io.writerA.valid := in.a.fire
      io.writerA.bits.address := in.a.bits.address
      io.writerA.bits.source := in.a.bits.source
      io.writerA.bits.size := in.a.bits.size
      io.writerA.bits.opcode := in.a.bits.opcode
      io.writerA.bits.mask := in.a.bits.mask
      io.writerD.valid := in.d.fire
      io.writerD.bits.source := in.d.bits.source
      io.writerD.bits.size := in.d.bits.size
      io.writerD.bits.denied := in.d.bits.denied
      io.writerD.bits.corrupt := in.d.bits.corrupt
      io.dBlocked := dBlocked && out.d.valid
      io.aFireCount := aFireCount
      io.dFireCount := dFireCount
      io.dBlockedCycleCount := dBlockedCycleCount
      io.lastAAddress := lastAAddress

      when(dBlocked && out.d.valid) {
        assert(!io.writerD.valid)
      }
    }
  }
}
