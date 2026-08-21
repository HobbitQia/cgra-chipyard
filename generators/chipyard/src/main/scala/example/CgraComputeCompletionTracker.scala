package chipyard.example

import chisel3._
import chisel3.util._

object CgraComputeCompletionStatus {
  val Width = 32
  val Success =
    CgraTransferControlGenerated.ComputeStatusSuccess.U(Width.W)
}

object CgraComputeCompletionError {
  object Operation {
    val Width = 1
    val LaunchResult =
      CgraTransferControlGenerated.ComputeErrorOperationLaunchResult.U(Width.W)
    val Complete =
      CgraTransferControlGenerated.ComputeErrorOperationComplete.U(Width.W)
  }

  object Reason {
    val Width = 3
    val UnexpectedEvent =
      CgraTransferControlGenerated.ComputeErrorReasonUnexpectedEvent.U(Width.W)
    val IdentityMismatch =
      CgraTransferControlGenerated.ComputeErrorReasonIdentityMismatch.U(Width.W)
    val CompleteBeforeLaunch =
      CgraTransferControlGenerated.ComputeErrorReasonCompleteBeforeLaunch.U(
        Width.W)
    val DuplicateComplete =
      CgraTransferControlGenerated.ComputeErrorReasonDuplicateComplete.U(
        Width.W)
    val DuplicateLaunchResult =
      CgraTransferControlGenerated.ComputeErrorReasonDuplicateLaunchResult.U(
        Width.W)
  }
}

class CgraComputeCompletion(params: CgraComputeLaunchGateParams)
    extends Bundle {
  val jobId = UInt(SpmTransferProtocol.JobIdWidth.W)
  val slot = UInt(SpmTransferProtocol.SlotIdWidth.W)
  val requestedBytes = UInt(SpmTransferProtocol.LengthWidth.W)
  val actualBytes = UInt(SpmTransferProtocol.LengthWidth.W)
  val spmWordAddress = UInt(params.spmAddressWidth.W)
  val dmaTag = UInt(params.dmaTagWidth.W)
  val packetCount = UInt(params.packetCountWidth.W)
  val completeData = UInt(CGRAGenerated.params.dataPayloadWidth.W)
  val status = UInt(CgraComputeCompletionStatus.Width.W)
}

class CgraComputeCompletionProtocolError(
  params: CgraComputeLaunchGateParams)
    extends Bundle {
  val jobId = UInt(SpmTransferProtocol.JobIdWidth.W)
  val slot = UInt(SpmTransferProtocol.SlotIdWidth.W)
  val requestedBytes = UInt(SpmTransferProtocol.LengthWidth.W)
  val actualBytes = UInt(SpmTransferProtocol.LengthWidth.W)
  val spmWordAddress = UInt(params.spmAddressWidth.W)
  val dmaTag = UInt(params.dmaTagWidth.W)
  val operation = UInt(CgraComputeCompletionError.Operation.Width.W)
  val reason = UInt(CgraComputeCompletionError.Reason.Width.W)
}

/** Correlates the untagged CGRA CMD_COMPLETE packet through one retained
  * automatic-compute owner. Reservation is established atomically with T6
  * header acceptance, while the owner becomes launched only on T6's real
  * LaunchAccepted result. A CGRA completion is backpressured until it can be
  * retained exactly once.
  */
class CgraComputeCompletionTracker(params: CgraComputeLaunchGateParams)
    extends Module {
  val io = IO(new Bundle {
    val reservationIn = Flipped(
      Decoupled(new CgraLaunchSequenceHeader(params)))
    val launchAcceptedIn = Flipped(Valid(new CgraLaunchResult(params)))
    val launchResultIn = Flipped(Decoupled(new CgraLaunchResult(params)))
    val launchResultOut = Decoupled(new CgraLaunchResult(params))
    val completeIn = Flipped(
      Decoupled(UInt(CGRAGenerated.params.dataPayloadWidth.W)))
    val completionOut = Decoupled(new CgraComputeCompletion(params))
    val errorOut = Decoupled(
      new CgraComputeCompletionProtocolError(params))
    val active = Output(Bool())
    val launched = Output(Bool())
    val completed = Output(Bool())
  })

  import CgraComputeCompletionError._

  private val active = RegInit(false.B)
  private val launched = RegInit(false.B)
  private val launchResultSeen = RegInit(false.B)
  private val header = Reg(new CgraLaunchSequenceHeader(params))
  private val acceptedLaunch = Reg(new CgraLaunchResult(params))

  private val completionPending = RegInit(false.B)
  private val completion = Reg(new CgraComputeCompletion(params))
  io.completionOut.valid := completionPending
  io.completionOut.bits := completion

  private val launchResultQueue = Module(
    new Queue(new CgraLaunchResult(params), 2))
  io.launchResultOut <> launchResultQueue.io.deq

  private val errorQueue = Module(
    new Queue(new CgraComputeCompletionProtocolError(params), 2))
  io.errorOut <> errorQueue.io.deq
  errorQueue.io.enq.valid := false.B
  errorQueue.io.enq.bits := 0.U.asTypeOf(
    new CgraComputeCompletionProtocolError(params))

  // Reserve space for this owner's eventual typed launch result. This keeps
  // result backpressure from delaying the precise final-packet launch event.
  io.reservationIn.ready := !active && !completionPending &&
    launchResultQueue.io.enq.ready
  when(io.reservationIn.fire) {
    active := true.B
    launched := false.B
    launchResultSeen := false.B
    header := io.reservationIn.bits
  }

  private def launchIdentityMatches(result: CgraLaunchResult): Bool = {
    result.jobId === header.jobId &&
    result.slot === header.slot &&
    result.requestedBytes === header.bytes &&
    result.spmWordAddress === header.spmWordAddress &&
    result.dmaTag === header.dmaTag &&
    result.packetCount === header.packetCount
  }

  private val incomingLaunchMatches = launchIdentityMatches(
    io.launchResultIn.bits)
  private val incomingLaunchAccepted =
    io.launchResultIn.bits.status === CgraLaunchStatus.LaunchAccepted
  private val launchResultExpected = active &&
    incomingLaunchMatches && (incomingLaunchAccepted === launched) &&
    !launchResultSeen
  private val duplicateLaunchResult = active && incomingLaunchMatches &&
    launchResultSeen
  private val launchResultNeedsError = !launchResultExpected
  io.launchResultIn.ready := Mux(
    duplicateLaunchResult,
    errorQueue.io.enq.ready,
    launchResultQueue.io.enq.ready &&
      (!launchResultNeedsError || errorQueue.io.enq.ready))
  launchResultQueue.io.enq.valid := io.launchResultIn.valid &&
    !duplicateLaunchResult &&
    (!launchResultNeedsError || errorQueue.io.enq.ready)
  launchResultQueue.io.enq.bits := io.launchResultIn.bits

  when(io.launchResultIn.fire) {
    when(duplicateLaunchResult) {
      errorQueue.io.enq.valid := true.B
      errorQueue.io.enq.bits.jobId := io.launchResultIn.bits.jobId
      errorQueue.io.enq.bits.slot := io.launchResultIn.bits.slot
      errorQueue.io.enq.bits.requestedBytes :=
        io.launchResultIn.bits.requestedBytes
      errorQueue.io.enq.bits.actualBytes :=
        io.launchResultIn.bits.actualBytes
      errorQueue.io.enq.bits.spmWordAddress :=
        io.launchResultIn.bits.spmWordAddress
      errorQueue.io.enq.bits.dmaTag := io.launchResultIn.bits.dmaTag
      errorQueue.io.enq.bits.operation := Operation.LaunchResult
      errorQueue.io.enq.bits.reason := Reason.DuplicateLaunchResult
    }.elsewhen(!active || (incomingLaunchMatches &&
      (incomingLaunchAccepted =/= launched))) {
      errorQueue.io.enq.valid := true.B
      errorQueue.io.enq.bits.jobId := io.launchResultIn.bits.jobId
      errorQueue.io.enq.bits.slot := io.launchResultIn.bits.slot
      errorQueue.io.enq.bits.requestedBytes :=
        io.launchResultIn.bits.requestedBytes
      errorQueue.io.enq.bits.actualBytes :=
        io.launchResultIn.bits.actualBytes
      errorQueue.io.enq.bits.spmWordAddress :=
        io.launchResultIn.bits.spmWordAddress
      errorQueue.io.enq.bits.dmaTag := io.launchResultIn.bits.dmaTag
      errorQueue.io.enq.bits.operation := Operation.LaunchResult
      errorQueue.io.enq.bits.reason := Reason.UnexpectedEvent
    }.elsewhen(!incomingLaunchMatches) {
      errorQueue.io.enq.valid := true.B
      errorQueue.io.enq.bits.jobId := io.launchResultIn.bits.jobId
      errorQueue.io.enq.bits.slot := io.launchResultIn.bits.slot
      errorQueue.io.enq.bits.requestedBytes :=
        io.launchResultIn.bits.requestedBytes
      errorQueue.io.enq.bits.actualBytes :=
        io.launchResultIn.bits.actualBytes
      errorQueue.io.enq.bits.spmWordAddress :=
        io.launchResultIn.bits.spmWordAddress
      errorQueue.io.enq.bits.dmaTag := io.launchResultIn.bits.dmaTag
      errorQueue.io.enq.bits.operation := Operation.LaunchResult
      errorQueue.io.enq.bits.reason := Reason.IdentityMismatch
      active := false.B
      launched := false.B
    }.elsewhen(!incomingLaunchAccepted) {
      active := false.B
      launched := false.B
    }.otherwise {
      launchResultSeen := true.B
    }
  }

  when(io.launchAcceptedIn.valid) {
    assert(active,
      "launch acceptance observed without a reserved compute owner")
    assert(!launched,
      "automatic launch acceptance observed more than once")
    assert(launchIdentityMatches(io.launchAcceptedIn.bits),
      "automatic launch acceptance identity does not match its owner")
    assert(io.launchAcceptedIn.bits.status ===
      CgraLaunchStatus.LaunchAccepted,
      "precise launch event must carry LaunchAccepted status")
    acceptedLaunch := io.launchAcceptedIn.bits
    launched := true.B
  }

  private val launchEstablished = launched || io.launchAcceptedIn.valid
  private val completeNeedsError = active &&
    (!launchEstablished || completionPending)
  io.completeIn.ready := !io.launchResultIn.valid && Mux(
    !active,
    true.B,
    Mux(completeNeedsError, errorQueue.io.enq.ready, !completionPending))

  when(io.completeIn.fire && active) {
    when(!launchEstablished) {
      errorQueue.io.enq.valid := true.B
      errorQueue.io.enq.bits.jobId := header.jobId
      errorQueue.io.enq.bits.slot := header.slot
      errorQueue.io.enq.bits.requestedBytes := header.bytes
      errorQueue.io.enq.bits.actualBytes := 0.U
      errorQueue.io.enq.bits.spmWordAddress := header.spmWordAddress
      errorQueue.io.enq.bits.dmaTag := header.dmaTag
      errorQueue.io.enq.bits.operation := Operation.Complete
      errorQueue.io.enq.bits.reason := Reason.CompleteBeforeLaunch
    }.elsewhen(completionPending) {
      errorQueue.io.enq.valid := true.B
      errorQueue.io.enq.bits.jobId := acceptedLaunch.jobId
      errorQueue.io.enq.bits.slot := acceptedLaunch.slot
      errorQueue.io.enq.bits.requestedBytes :=
        acceptedLaunch.requestedBytes
      errorQueue.io.enq.bits.actualBytes := acceptedLaunch.actualBytes
      errorQueue.io.enq.bits.spmWordAddress :=
        acceptedLaunch.spmWordAddress
      errorQueue.io.enq.bits.dmaTag := acceptedLaunch.dmaTag
      errorQueue.io.enq.bits.operation := Operation.Complete
      errorQueue.io.enq.bits.reason := Reason.DuplicateComplete
    }.otherwise {
      val completionOwner = Mux(
        launched, acceptedLaunch, io.launchAcceptedIn.bits)
      completionPending := true.B
      completion.jobId := completionOwner.jobId
      completion.slot := completionOwner.slot
      completion.requestedBytes := completionOwner.requestedBytes
      completion.actualBytes := completionOwner.actualBytes
      completion.spmWordAddress := completionOwner.spmWordAddress
      completion.dmaTag := completionOwner.dmaTag
      completion.packetCount := completionOwner.packetCount
      completion.completeData := io.completeIn.bits
      completion.status := CgraComputeCompletionStatus.Success
    }
  }

  when(io.completionOut.fire) {
    completionPending := false.B
    active := false.B
    launched := false.B
    launchResultSeen := false.B
  }

  io.active := active
  io.launched := launched
  io.completed := completionPending

  private def assertStable[T <: Data](channel: DecoupledIO[T]): Unit = {
    val blocked = channel.valid && !channel.ready
    val wasBlocked = RegNext(blocked, false.B)
    val heldBits = RegEnable(channel.bits.asUInt, blocked)
    when(wasBlocked) {
      assert(channel.valid)
      assert(channel.bits.asUInt === heldBits)
    }
  }
  assertStable(io.launchResultOut)
  assertStable(io.completionOut)
  assertStable(io.errorOut)

  when(launched) {
    assert(active)
  }
  when(completionPending) {
    assert(active)
    assert(launched)
  }
}
