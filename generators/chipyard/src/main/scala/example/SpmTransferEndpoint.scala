package chipyard.example

import chisel3._
import chisel3.util._

/** Stable protocol constants shared by transfer-endpoint adapters. */
object SpmTransferProtocol {
  val JobIdWidth = 32
  val SlotIdWidth = 32
  val LengthWidth = 32
  val StatusWidth = 32

  object ProducerStatus {
    val Success = 0.U(StatusWidth.W)
  }

  object SlotState {
    val Width = 2
    val Free = 0.U(Width.W)
    val Producing = 1.U(Width.W)
    val Ready = 2.U(Width.W)
    val Reading = 3.U(Width.W)
  }

  object Operation {
    val Width = 3
    val Request = 1.U(Width.W)
    val Ready = 2.U(Width.W)
    val ReadStart = 3.U(Width.W)
    val Release = 4.U(Width.W)
  }

  object ErrorReason {
    val Width = 4
    val None = 0.U(Width.W)
    val InvalidSlot = 1.U(Width.W)
    val InvalidJob = 2.U(Width.W)
    val InvalidLength = 3.U(Width.W)
    val SlotBusy = 4.U(Width.W)
    val DuplicateJob = 5.U(Width.W)
    val JobMismatch = 6.U(Width.W)
    val StateOrderMismatch = 7.U(Width.W)
    val ActualLengthExceedsRequest = 8.U(Width.W)
  }
}

case class SpmTransferEndpointParams(
  slotCount: Int,
  slotSizeBytes: Int,
  beatBytes: Int) {
  require(slotCount > 0)
  require(slotSizeBytes > 0)
  require(isPow2(beatBytes))
  require(slotSizeBytes % beatBytes == 0)
}

object SpmTransferEndpointParams {
  /** The production contract is generated from configs/soc/cgra_soc.yaml. */
  val production: SpmTransferEndpointParams = SpmTransferEndpointParams(
    slotCount = GemminiExternalSpadGenerated.outputSlotCount,
    slotSizeBytes = GemminiExternalSpadGenerated.outputSlotSizeBytes,
    beatBytes = GemminiExternalSpadGenerated.spadRowBytes)
}

class SpmTransferRequest extends Bundle {
  val jobId = UInt(SpmTransferProtocol.JobIdWidth.W)
  val slot = UInt(SpmTransferProtocol.SlotIdWidth.W)
  val maxBytes = UInt(SpmTransferProtocol.LengthWidth.W)
}

class SpmTransferReady extends Bundle {
  val jobId = UInt(SpmTransferProtocol.JobIdWidth.W)
  val slot = UInt(SpmTransferProtocol.SlotIdWidth.W)
  val actualBytes = UInt(SpmTransferProtocol.LengthWidth.W)
  val status = UInt(SpmTransferProtocol.StatusWidth.W)
}

class SpmTransferIdentity extends Bundle {
  val jobId = UInt(SpmTransferProtocol.JobIdWidth.W)
  val slot = UInt(SpmTransferProtocol.SlotIdWidth.W)
}

class SpmTransferProtocolError extends Bundle {
  val jobId = UInt(SpmTransferProtocol.JobIdWidth.W)
  val slot = UInt(SpmTransferProtocol.SlotIdWidth.W)
  val operation = UInt(SpmTransferProtocol.Operation.Width.W)
  val reason = UInt(SpmTransferProtocol.ErrorReason.Width.W)
}

class SpmTransferSlotDebug extends Bundle {
  val state = UInt(SpmTransferProtocol.SlotState.Width.W)
  val jobId = UInt(SpmTransferProtocol.JobIdWidth.W)
  val maxBytes = UInt(SpmTransferProtocol.LengthWidth.W)
  val actualBytes = UInt(SpmTransferProtocol.LengthWidth.W)
  val status = UInt(SpmTransferProtocol.StatusWidth.W)
  val requestPending = Bool()
  val requestForwarded = Bool()
  val readyPending = Bool()
  val readyDelivered = Bool()
}

/** Event-driven ownership and protocol core for static accelerator SPM slots.
  *
  * The core moves no payload and derives no completion locally. Producer and
  * consumer adapters supply the READY, read-start, and RELEASE events. Shared
  * request and READY outputs use deterministic lowest-slot-first arbitration;
  * an already selected output remains registered until it handshakes.
  */
class SpmTransferEndpoint(params: SpmTransferEndpointParams) extends Module {
  val io = IO(new Bundle {
    val requestIn = Flipped(Decoupled(new SpmTransferRequest))
    val requestOut = Decoupled(new SpmTransferRequest)
    val readyIn = Flipped(Decoupled(new SpmTransferReady))
    val readyOut = Decoupled(new SpmTransferReady)
    val readStartIn = Flipped(Decoupled(new SpmTransferIdentity))
    val releaseIn = Flipped(Decoupled(new SpmTransferIdentity))
    val errorOut = Decoupled(new SpmTransferProtocolError)
    val slots = Output(Vec(params.slotCount, new SpmTransferSlotDebug))
  })

  import SpmTransferProtocol._

  private val slotIndexWidth = math.max(1, log2Ceil(params.slotCount))
  private val beatMask = (params.beatBytes - 1).U(LengthWidth.W)

  private val states = RegInit(VecInit(Seq.fill(params.slotCount)(SlotState.Free)))
  private val jobIds = RegInit(VecInit(Seq.fill(params.slotCount)(0.U(JobIdWidth.W))))
  private val maxBytes = RegInit(VecInit(Seq.fill(params.slotCount)(0.U(LengthWidth.W))))
  private val actualBytes = RegInit(VecInit(Seq.fill(params.slotCount)(0.U(LengthWidth.W))))
  private val statuses = RegInit(VecInit(Seq.fill(params.slotCount)(0.U(StatusWidth.W))))
  private val requestPending = RegInit(VecInit(Seq.fill(params.slotCount)(false.B)))
  private val requestForwarded = RegInit(VecInit(Seq.fill(params.slotCount)(false.B)))
  private val readyPending = RegInit(VecInit(Seq.fill(params.slotCount)(false.B)))
  private val readyDelivered = RegInit(VecInit(Seq.fill(params.slotCount)(false.B)))
  private val stateTransitionAuthorized = WireInit(
    VecInit(Seq.fill(params.slotCount)(false.B)))

  private def validSlot(slot: UInt): Bool = slot < params.slotCount.U
  private def alignedLength(length: UInt): Bool =
    (length & beatMask) === 0.U
  private def activeJob(jobId: UInt): Bool =
    VecInit((0 until params.slotCount).map { index =>
      states(index) =/= SlotState.Free && jobIds(index) === jobId
    }).asUInt.orR

  private val requestSlotValid = validSlot(io.requestIn.bits.slot)
  private val requestSlot = io.requestIn.bits.slot(slotIndexWidth - 1, 0)
  private val requestReason = WireDefault(ErrorReason.None)
  when (!requestSlotValid) {
    requestReason := ErrorReason.InvalidSlot
  }.elsewhen(io.requestIn.bits.jobId === 0.U) {
    requestReason := ErrorReason.InvalidJob
  }.elsewhen(
    io.requestIn.bits.maxBytes === 0.U ||
      io.requestIn.bits.maxBytes > params.slotSizeBytes.U ||
      !alignedLength(io.requestIn.bits.maxBytes)) {
    requestReason := ErrorReason.InvalidLength
  }.elsewhen(activeJob(io.requestIn.bits.jobId)) {
    requestReason := ErrorReason.DuplicateJob
  }.elsewhen(states(requestSlot) =/= SlotState.Free) {
    requestReason := ErrorReason.SlotBusy
  }
  private val requestAccepted = requestReason === ErrorReason.None

  private val readySlotValid = validSlot(io.readyIn.bits.slot)
  private val readySlot = io.readyIn.bits.slot(slotIndexWidth - 1, 0)
  private val readyReason = WireDefault(ErrorReason.None)
  when (!readySlotValid) {
    readyReason := ErrorReason.InvalidSlot
  }.elsewhen(states(readySlot) =/= SlotState.Producing ||
    !requestForwarded(readySlot)) {
    readyReason := ErrorReason.StateOrderMismatch
  }.elsewhen(jobIds(readySlot) =/= io.readyIn.bits.jobId) {
    readyReason := ErrorReason.JobMismatch
  }.elsewhen(io.readyIn.bits.actualBytes > maxBytes(readySlot)) {
    readyReason := ErrorReason.ActualLengthExceedsRequest
  }.elsewhen(
    (io.readyIn.bits.status === ProducerStatus.Success &&
      io.readyIn.bits.actualBytes === 0.U) ||
      (io.readyIn.bits.actualBytes =/= 0.U &&
        !alignedLength(io.readyIn.bits.actualBytes))) {
    readyReason := ErrorReason.InvalidLength
  }
  private val readyAccepted = readyReason === ErrorReason.None

  private val readStartSlotValid = validSlot(io.readStartIn.bits.slot)
  private val readStartSlot = io.readStartIn.bits.slot(slotIndexWidth - 1, 0)
  private val readStartReason = WireDefault(ErrorReason.None)
  when (!readStartSlotValid) {
    readStartReason := ErrorReason.InvalidSlot
  }.elsewhen(
    states(readStartSlot) =/= SlotState.Ready ||
      !readyDelivered(readStartSlot) ||
      statuses(readStartSlot) =/= ProducerStatus.Success) {
    readStartReason := ErrorReason.StateOrderMismatch
  }.elsewhen(jobIds(readStartSlot) =/= io.readStartIn.bits.jobId) {
    readStartReason := ErrorReason.JobMismatch
  }
  private val readStartAccepted = readStartReason === ErrorReason.None

  private val releaseSlotValid = validSlot(io.releaseIn.bits.slot)
  private val releaseSlot = io.releaseIn.bits.slot(slotIndexWidth - 1, 0)
  private val releaseStateValid = releaseSlotValid &&
    (states(releaseSlot) === SlotState.Reading ||
      (states(releaseSlot) === SlotState.Ready &&
        readyDelivered(releaseSlot) &&
        statuses(releaseSlot) =/= ProducerStatus.Success))
  private val releaseReason = WireDefault(ErrorReason.None)
  when (!releaseSlotValid) {
    releaseReason := ErrorReason.InvalidSlot
  }.elsewhen(!releaseStateValid) {
    releaseReason := ErrorReason.StateOrderMismatch
  }.elsewhen(jobIds(releaseSlot) =/= io.releaseIn.bits.jobId) {
    releaseReason := ErrorReason.JobMismatch
  }
  private val releaseAccepted = releaseReason === ErrorReason.None

  // One lossless protocol-error buffer. Round-robin arbitration advances only
  // when an invalid event is admitted, preventing a replenished source from
  // starving the other operations while an earlier error is stalled.
  private val errorValid = RegInit(false.B)
  private val errorBits = Reg(new SpmTransferProtocolError)
  io.errorOut.valid := errorValid
  io.errorOut.bits := errorBits
  private val errorCanEnqueue = !errorValid || io.errorOut.ready
  private val requestErrorCandidate = io.requestIn.valid && !requestAccepted
  private val readyErrorCandidate = io.readyIn.valid && !readyAccepted
  private val readStartErrorCandidate = io.readStartIn.valid && !readStartAccepted
  private val releaseErrorCandidate = io.releaseIn.valid && !releaseAccepted
  private val errorArbiter = Module(new RRArbiter(new SpmTransferProtocolError, 4))

  errorArbiter.io.in(0).valid := requestErrorCandidate
  errorArbiter.io.in(0).bits.jobId := io.requestIn.bits.jobId
  errorArbiter.io.in(0).bits.slot := io.requestIn.bits.slot
  errorArbiter.io.in(0).bits.operation := Operation.Request
  errorArbiter.io.in(0).bits.reason := requestReason

  errorArbiter.io.in(1).valid := readyErrorCandidate
  errorArbiter.io.in(1).bits.jobId := io.readyIn.bits.jobId
  errorArbiter.io.in(1).bits.slot := io.readyIn.bits.slot
  errorArbiter.io.in(1).bits.operation := Operation.Ready
  errorArbiter.io.in(1).bits.reason := readyReason

  errorArbiter.io.in(2).valid := readStartErrorCandidate
  errorArbiter.io.in(2).bits.jobId := io.readStartIn.bits.jobId
  errorArbiter.io.in(2).bits.slot := io.readStartIn.bits.slot
  errorArbiter.io.in(2).bits.operation := Operation.ReadStart
  errorArbiter.io.in(2).bits.reason := readStartReason

  errorArbiter.io.in(3).valid := releaseErrorCandidate
  errorArbiter.io.in(3).bits.jobId := io.releaseIn.bits.jobId
  errorArbiter.io.in(3).bits.slot := io.releaseIn.bits.slot
  errorArbiter.io.in(3).bits.operation := Operation.Release
  errorArbiter.io.in(3).bits.reason := releaseReason

  errorArbiter.io.out.ready := errorCanEnqueue
  io.requestIn.ready := requestAccepted || errorArbiter.io.in(0).ready
  io.readyIn.ready := readyAccepted || errorArbiter.io.in(1).ready
  io.readStartIn.ready := readStartAccepted || errorArbiter.io.in(2).ready
  io.releaseIn.ready := releaseAccepted || errorArbiter.io.in(3).ready

  when(errorArbiter.io.out.fire) {
    errorValid := true.B
    errorBits := errorArbiter.io.out.bits
  }.elsewhen(io.errorOut.fire) {
    errorValid := false.B
  }

  when(io.requestIn.fire && requestAccepted) {
    stateTransitionAuthorized(requestSlot) := true.B
    states(requestSlot) := SlotState.Producing
    jobIds(requestSlot) := io.requestIn.bits.jobId
    maxBytes(requestSlot) := io.requestIn.bits.maxBytes
    actualBytes(requestSlot) := 0.U
    statuses(requestSlot) := ProducerStatus.Success
    requestPending(requestSlot) := true.B
    requestForwarded(requestSlot) := false.B
    readyPending(requestSlot) := false.B
    readyDelivered(requestSlot) := false.B
  }

  when(io.readyIn.fire && readyAccepted) {
    stateTransitionAuthorized(readySlot) := true.B
    states(readySlot) := SlotState.Ready
    actualBytes(readySlot) := io.readyIn.bits.actualBytes
    statuses(readySlot) := io.readyIn.bits.status
    readyPending(readySlot) := true.B
    readyDelivered(readySlot) := false.B
  }

  when(io.readStartIn.fire && readStartAccepted) {
    stateTransitionAuthorized(readStartSlot) := true.B
    states(readStartSlot) := SlotState.Reading
  }

  when(io.releaseIn.fire && releaseAccepted) {
    stateTransitionAuthorized(releaseSlot) := true.B
    states(releaseSlot) := SlotState.Free
    jobIds(releaseSlot) := 0.U
    maxBytes(releaseSlot) := 0.U
    actualBytes(releaseSlot) := 0.U
    statuses(releaseSlot) := ProducerStatus.Success
    requestPending(releaseSlot) := false.B
    requestForwarded(releaseSlot) := false.B
    readyPending(releaseSlot) := false.B
    readyDelivered(releaseSlot) := false.B
  }

  // Register shared outputs before asserting valid so that later arrivals
  // cannot change a payload while the receiver applies backpressure.
  private val requestOutputValid = RegInit(false.B)
  private val requestOutputBits = Reg(new SpmTransferRequest)
  private val requestOutputSlot = Reg(UInt(slotIndexWidth.W))
  private val anyRequestPending = requestPending.asUInt.orR
  private val nextRequestSlot = PriorityEncoder(requestPending.asUInt)
  io.requestOut.valid := requestOutputValid
  io.requestOut.bits := requestOutputBits
  when(!requestOutputValid) {
    when(anyRequestPending) {
      requestOutputValid := true.B
      requestOutputSlot := nextRequestSlot
      requestOutputBits.jobId := jobIds(nextRequestSlot)
      requestOutputBits.slot := nextRequestSlot
      requestOutputBits.maxBytes := maxBytes(nextRequestSlot)
    }
  }.elsewhen(io.requestOut.fire) {
    requestOutputValid := false.B
    requestPending(requestOutputSlot) := false.B
    requestForwarded(requestOutputSlot) := true.B
  }

  private val readyOutputValid = RegInit(false.B)
  private val readyOutputBits = Reg(new SpmTransferReady)
  private val readyOutputSlot = Reg(UInt(slotIndexWidth.W))
  private val anyReadyPending = readyPending.asUInt.orR
  private val nextReadySlot = PriorityEncoder(readyPending.asUInt)
  io.readyOut.valid := readyOutputValid
  io.readyOut.bits := readyOutputBits
  when(!readyOutputValid) {
    when(anyReadyPending) {
      readyOutputValid := true.B
      readyOutputSlot := nextReadySlot
      readyOutputBits.jobId := jobIds(nextReadySlot)
      readyOutputBits.slot := nextReadySlot
      readyOutputBits.actualBytes := actualBytes(nextReadySlot)
      readyOutputBits.status := statuses(nextReadySlot)
    }
  }.elsewhen(io.readyOut.fire) {
    readyOutputValid := false.B
    readyPending(readyOutputSlot) := false.B
    readyDelivered(readyOutputSlot) := true.B
  }

  for (index <- 0 until params.slotCount) {
    io.slots(index).state := states(index)
    io.slots(index).jobId := jobIds(index)
    io.slots(index).maxBytes := maxBytes(index)
    io.slots(index).actualBytes := actualBytes(index)
    io.slots(index).status := statuses(index)
    io.slots(index).requestPending := requestPending(index)
    io.slots(index).requestForwarded := requestForwarded(index)
    io.slots(index).readyPending := readyPending(index)
    io.slots(index).readyDelivered := readyDelivered(index)
  }

  // Structural protocol assertions remain with the reusable production core.
  private def assertStable[T <: Data](channel: DecoupledIO[T]): Unit = {
    val blocked = channel.valid && !channel.ready
    val wasBlocked = RegNext(blocked, false.B)
    val heldBits = RegEnable(channel.bits.asUInt, blocked)
    when(wasBlocked) {
      assert(channel.valid)
      assert(channel.bits.asUInt === heldBits)
    }
  }
  assertStable(io.requestOut)
  assertStable(io.readyOut)
  assertStable(io.errorOut)

  for (left <- 0 until params.slotCount; right <- left + 1 until params.slotCount) {
    when(states(left) =/= SlotState.Free && states(right) =/= SlotState.Free) {
      assert(jobIds(left) =/= jobIds(right))
    }
  }

  for (index <- 0 until params.slotCount) {
    when(states(index) =/= SlotState.Free) {
      assert(jobIds(index) =/= 0.U)
      assert(maxBytes(index) =/= 0.U)
      assert(maxBytes(index) <= params.slotSizeBytes.U)
      assert(alignedLength(maxBytes(index)))
    }
    when(states(index) === SlotState.Ready || states(index) === SlotState.Reading) {
      assert(actualBytes(index) <= maxBytes(index))
      when(statuses(index) === ProducerStatus.Success) {
        assert(actualBytes(index) =/= 0.U)
      }
      when(actualBytes(index) =/= 0.U) {
        assert(alignedLength(actualBytes(index)))
      }
    }
    when(states(index) === SlotState.Free) {
      assert(jobIds(index) === 0.U)
      assert(maxBytes(index) === 0.U)
      assert(actualBytes(index) === 0.U)
      assert(statuses(index) === ProducerStatus.Success)
      assert(!requestPending(index))
      assert(!requestForwarded(index))
      assert(!readyPending(index))
      assert(!readyDelivered(index))
    }
  }

  private val pastValid = RegNext(true.B, false.B)
  private val previousStates = RegNext(states)
  private val previousStatuses = RegNext(statuses)
  private val previousReadyDelivered = RegNext(readyDelivered)
  private val previousTransitionAuthorized = RegNext(stateTransitionAuthorized)
  when(pastValid) {
    for (index <- 0 until params.slotCount) {
      when(states(index) =/= previousStates(index)) {
        assert(previousTransitionAuthorized(index))
      }
      switch(previousStates(index)) {
        is(SlotState.Free) {
          assert(states(index) === SlotState.Free ||
            states(index) === SlotState.Producing)
        }
        is(SlotState.Producing) {
          assert(states(index) === SlotState.Producing ||
            states(index) === SlotState.Ready)
        }
        is(SlotState.Ready) {
          assert(states(index) === SlotState.Ready ||
            states(index) === SlotState.Reading ||
            (states(index) === SlotState.Free &&
              previousStatuses(index) =/= ProducerStatus.Success &&
              previousReadyDelivered(index)))
        }
        is(SlotState.Reading) {
          assert(states(index) === SlotState.Reading ||
            states(index) === SlotState.Free)
        }
      }
    }
  }
}
