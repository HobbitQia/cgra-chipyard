package chipyard.example

import chisel3._
import chisel3.util._

object CgraLaunchStatus {
  val Width = 32
  val LaunchAccepted = 0.U(Width.W)
  val InvalidJob = 1.U(Width.W)
  val InvalidSlot = 2.U(Width.W)
  val InvalidLength = 3.U(Width.W)
  val SpmRange = 4.U(Width.W)
  val InvalidPacketCount = 5.U(Width.W)
  val ConsumerFailure = 6.U(Width.W)
  val ProducerFailure = 7.U(Width.W)
  val IdentityMismatch = 8.U(Width.W)
  val InvalidPacket = 9.U(Width.W)
}

object CgraLaunchError {
  object Operation {
    val Width = 2
    val Header = 0.U(Width.W)
    val Packet = 1.U(Width.W)
    val Completion = 2.U(Width.W)
  }

  object Reason {
    val Width = 3
    val UnexpectedEvent = 1.U(Width.W)
    val DuplicateEvent = 2.U(Width.W)
    val IdentityMismatch = 3.U(Width.W)
    val LengthMismatch = 4.U(Width.W)
    val NonLaunchPacket = 5.U(Width.W)
    val MalformedCompletion = 6.U(Width.W)
  }
}

case class CgraComputeLaunchGateParams(
  slotCount: Int,
  slotSizeBytes: Int,
  publicationRowBytes: Int,
  dmaBeatBytes: Int,
  spmWords: Int,
  spmWordBytes: Int,
  spmAddressWidth: Int,
  dmaTagWidth: Int,
  packetWidth: Int,
  packetCommandLsb: Int,
  packetCommandWidth: Int,
  launchCommand: Int,
  packetCapacity: Int = 16) {
  require(slotCount > 0)
  require(slotSizeBytes > 0)
  require(isPow2(publicationRowBytes))
  require(isPow2(dmaBeatBytes))
  require(spmWords > 0)
  require(isPow2(spmWordBytes))
  require(spmWords <= (1 << spmAddressWidth))
  require(dmaTagWidth > 0)
  require(packetWidth > 0)
  require(packetCommandLsb >= 0)
  require(packetCommandWidth > 0)
  require(packetCommandLsb + packetCommandWidth <= packetWidth)
  require(launchCommand >= 0 && launchCommand < (1 << packetCommandWidth))
  require(packetCapacity > 0 && isPow2(packetCapacity))

  val spmCapacityBytes: Int = spmWords * spmWordBytes
  val packetCountWidth: Int = log2Ceil(packetCapacity + 1)
}

object CgraComputeLaunchGateParams {
  val production: CgraComputeLaunchGateParams = {
    val consumer = CgraConsumerPullAdapterParams.production
    val cgra = CGRAGenerated.params
    CgraComputeLaunchGateParams(
      slotCount = consumer.slotCount,
      slotSizeBytes = consumer.slotSizeBytes,
      publicationRowBytes = consumer.publicationRowBytes,
      dmaBeatBytes = consumer.dmaBeatBytes,
      spmWords = consumer.spmWords,
      spmWordBytes = consumer.spmWordBytes,
      spmAddressWidth = consumer.spmAddressWidth,
      dmaTagWidth = consumer.dmaTagWidth,
      packetWidth = cgra.intraPktWidth,
      packetCommandLsb = cgra.packetLayout.cmdLsb,
      packetCommandWidth = cgra.cmdWidth,
      launchCommand = CGRACmdGenerated.CMD_LAUNCH,
      packetCapacity = 16)
  }
}

class CgraLaunchSequenceHeader(params: CgraComputeLaunchGateParams)
    extends Bundle {
  val jobId = UInt(SpmTransferProtocol.JobIdWidth.W)
  val slot = UInt(SpmTransferProtocol.SlotIdWidth.W)
  val bytes = UInt(SpmTransferProtocol.LengthWidth.W)
  val spmWordAddress = UInt(params.spmAddressWidth.W)
  val dmaTag = UInt(params.dmaTagWidth.W)
  val packetCount = UInt(params.packetCountWidth.W)
}

class CgraLaunchPacket(params: CgraComputeLaunchGateParams) extends Bundle {
  val packet = UInt(params.packetWidth.W)
}

class CgraLaunchResult(params: CgraComputeLaunchGateParams) extends Bundle {
  val jobId = UInt(SpmTransferProtocol.JobIdWidth.W)
  val slot = UInt(SpmTransferProtocol.SlotIdWidth.W)
  val requestedBytes = UInt(SpmTransferProtocol.LengthWidth.W)
  val actualBytes = UInt(SpmTransferProtocol.LengthWidth.W)
  val spmWordAddress = UInt(params.spmAddressWidth.W)
  val dmaTag = UInt(params.dmaTagWidth.W)
  val packetCount = UInt(params.packetCountWidth.W)
  val status = UInt(CgraLaunchStatus.Width.W)
}

class CgraLaunchProtocolError(params: CgraComputeLaunchGateParams)
    extends Bundle {
  val jobId = UInt(SpmTransferProtocol.JobIdWidth.W)
  val slot = UInt(SpmTransferProtocol.SlotIdWidth.W)
  val requestedBytes = UInt(SpmTransferProtocol.LengthWidth.W)
  val actualBytes = UInt(SpmTransferProtocol.LengthWidth.W)
  val spmWordAddress = UInt(params.spmAddressWidth.W)
  val dmaTag = UInt(params.dmaTagWidth.W)
  val operation = UInt(CgraLaunchError.Operation.Width.W)
  val reason = UInt(CgraLaunchError.Reason.Width.W)
}

/** Retains one complete launch-only packet sequence and releases it only after
  * a matching successful consumer completion. The output is intended to feed
  * the existing CGRA packet FIFO; this module never interprets packet fields
  * other than the generated command field.
  */
class CgraComputeLaunchGate(params: CgraComputeLaunchGateParams)
    extends Module {
  val io = IO(new Bundle {
    val headerIn = Flipped(Decoupled(new CgraLaunchSequenceHeader(params)))
    val packetIn = Flipped(Decoupled(new CgraLaunchPacket(params)))
    val completionIn = Flipped(
      Decoupled(new CgraConsumerCompletion(
        CgraConsumerPullAdapterParams.production)))
    val packetOut = Decoupled(new CgraLaunchPacket(params))
    val resultOut = Decoupled(new CgraLaunchResult(params))
    val errorOut = Decoupled(new CgraLaunchProtocolError(params))
    val active = Output(Bool())
    val capturedPacketCount = Output(UInt(params.packetCountWidth.W))
    val acceptedPacketCount = Output(UInt(32.W))
    val acceptedSequenceCount = Output(UInt(32.W))
  })

  import CgraLaunchError._

  private val consumerParams = CgraConsumerPullAdapterParams.production
  require(params.slotCount == consumerParams.slotCount)
  require(params.slotSizeBytes == consumerParams.slotSizeBytes)
  require(params.publicationRowBytes == consumerParams.publicationRowBytes)
  require(params.dmaBeatBytes == consumerParams.dmaBeatBytes)
  require(params.spmWords == consumerParams.spmWords)
  require(params.spmWordBytes == consumerParams.spmWordBytes)
  require(params.spmAddressWidth == consumerParams.spmAddressWidth)
  require(params.dmaTagWidth == consumerParams.dmaTagWidth)

  private val headerActive = RegInit(false.B)
  private val header = Reg(new CgraLaunchSequenceHeader(params))
  private val packetMemory = Reg(Vec(
    params.packetCapacity, UInt(params.packetWidth.W)))
  private val capturedPackets = RegInit(0.U(params.packetCountWidth.W))
  private val packetsComplete = RegInit(false.B)
  private val completionPending = RegInit(false.B)
  private val completion = Reg(new CgraConsumerCompletion(consumerParams))
  private val draining = RegInit(false.B)
  private val drainIndex = RegInit(0.U(log2Ceil(params.packetCapacity).W))

  private val resultPending = RegInit(false.B)
  private val result = Reg(new CgraLaunchResult(params))
  io.resultOut.valid := resultPending
  io.resultOut.bits := result

  private val errorPending = RegInit(false.B)
  private val error = Reg(new CgraLaunchProtocolError(params))
  io.errorOut.valid := errorPending
  io.errorOut.bits := error
  private val errorCanAccept = !errorPending || io.errorOut.ready

  private val acceptedPacketCount = RegInit(0.U(32.W))
  private val acceptedSequenceCount = RegInit(0.U(32.W))
  io.active := headerActive || completionPending || draining || resultPending
  io.capturedPacketCount := capturedPackets
  io.acceptedPacketCount := acceptedPacketCount
  io.acceptedSequenceCount := acceptedSequenceCount

  private val headerWords =
    io.headerIn.bits.bytes >> log2Ceil(params.spmWordBytes)
  private val headerSpmEnd =
    io.headerIn.bits.spmWordAddress +& headerWords
  private val headerStatus = WireDefault(CgraLaunchStatus.LaunchAccepted)
  when(io.headerIn.bits.jobId === 0.U) {
    headerStatus := CgraLaunchStatus.InvalidJob
  }.elsewhen(io.headerIn.bits.slot >= params.slotCount.U) {
    headerStatus := CgraLaunchStatus.InvalidSlot
  }.elsewhen(
    io.headerIn.bits.bytes === 0.U ||
      io.headerIn.bits.bytes > params.slotSizeBytes.U ||
      io.headerIn.bits.bytes > params.spmCapacityBytes.U ||
      (io.headerIn.bits.bytes & (params.publicationRowBytes - 1).U).orR ||
      (io.headerIn.bits.bytes & (params.dmaBeatBytes - 1).U).orR) {
    headerStatus := CgraLaunchStatus.InvalidLength
  }.elsewhen(headerSpmEnd > params.spmWords.U) {
    headerStatus := CgraLaunchStatus.SpmRange
  }.elsewhen(
    io.headerIn.bits.packetCount === 0.U ||
      io.headerIn.bits.packetCount > params.packetCapacity.U) {
    headerStatus := CgraLaunchStatus.InvalidPacketCount
  }
  private val headerValid =
    headerStatus === CgraLaunchStatus.LaunchAccepted

  private def sameIdentity(
    launch: CgraLaunchSequenceHeader,
    completed: CgraConsumerCompletion): Bool = {
    launch.jobId === completed.jobId &&
    launch.slot === completed.slot &&
    launch.bytes === completed.requestedBytes &&
    launch.spmWordAddress === completed.spmWordAddress &&
    launch.dmaTag === completed.dmaTag
  }

  private def completionSucceeded(completed: CgraConsumerCompletion): Bool = {
    completed.consumerStatus === CgraConsumerStatus.Success &&
    completed.producerStatus === SpmTransferProtocol.ProducerStatus.Success
  }

  private def completionMatches(
    launch: CgraLaunchSequenceHeader,
    completed: CgraConsumerCompletion): Bool = {
    sameIdentity(launch, completed) &&
    (!completionSucceeded(completed) ||
      completed.actualBytes === completed.requestedBytes)
  }

  private val completionWords = io.completionIn.bits.requestedBytes >>
    log2Ceil(params.spmWordBytes)
  private val completionSpmEnd =
    io.completionIn.bits.spmWordAddress +& completionWords
  private val incomingCompletionSuccess =
    completionSucceeded(io.completionIn.bits)
  private val completionRequestedBytesValid =
    io.completionIn.bits.requestedBytes =/= 0.U &&
    io.completionIn.bits.requestedBytes <= params.slotSizeBytes.U &&
    io.completionIn.bits.requestedBytes <= params.spmCapacityBytes.U &&
    !(io.completionIn.bits.requestedBytes &
      (params.publicationRowBytes - 1).U).orR &&
    !(io.completionIn.bits.requestedBytes &
      (params.dmaBeatBytes - 1).U).orR &&
    completionSpmEnd <= params.spmWords.U
  private val completionActualBytesValid = Mux(
    incomingCompletionSuccess,
    io.completionIn.bits.actualBytes === io.completionIn.bits.requestedBytes,
    io.completionIn.bits.actualBytes <= io.completionIn.bits.requestedBytes &&
      (io.completionIn.bits.actualBytes === 0.U ||
        !(io.completionIn.bits.actualBytes &
          (params.publicationRowBytes - 1).U).orR))
  private val completionStructurallyValid =
    io.completionIn.bits.jobId =/= 0.U &&
    io.completionIn.bits.slot < params.slotCount.U &&
    io.completionIn.bits.spmWordAddress < params.spmWords.U &&
    completionRequestedBytesValid && completionActualBytesValid

  private val canAcceptNewHeader =
    !headerActive && !draining && !resultPending
  private val packetCommand = io.packetIn.bits.packet(
    params.packetCommandLsb + params.packetCommandWidth - 1,
    params.packetCommandLsb)
  private val packetIsLaunch = packetCommand === params.launchCommand.U
  private val collectingPacket =
    headerActive && !packetsComplete && !draining && !resultPending
  private val incomingIdentityMatchesActive =
    sameIdentity(header, io.completionIn.bits)
  private val canStoreCompletion =
    !completionPending && !draining && !resultPending
  private val headerNeedsError = !canAcceptNewHeader
  private val packetNeedsError = !collectingPacket || !packetIsLaunch
  private val completionLengthMismatch =
    headerActive && incomingIdentityMatchesActive &&
      incomingCompletionSuccess &&
      io.completionIn.bits.actualBytes =/= header.bytes
  private val completionNeedsError =
    !canStoreCompletion ||
      completionLengthMismatch ||
      !completionStructurallyValid ||
      (headerActive && !incomingIdentityMatchesActive)

  // All three input channels share state and one lossless error buffer. A
  // round-robin token arbiter permits at most one input handshake per cycle,
  // so simultaneous rejects cannot overwrite each other or race a legal
  // state transition. A first header has priority over a simultaneously
  // crossed first packet; otherwise legal events remain eligible while an
  // invalid event waits for a stalled error output.
  private val inputArbiter = Module(new RRArbiter(UInt(2.W), 3))
  private val forceFirstHeader = !headerActive && io.headerIn.valid
  private val headerEligible = !headerNeedsError || errorCanAccept
  private val packetEligible = !packetNeedsError || errorCanAccept
  private val completionEligible = !completionNeedsError || errorCanAccept
  inputArbiter.io.in(0).valid := io.headerIn.valid && headerEligible
  inputArbiter.io.in(0).bits := 0.U
  inputArbiter.io.in(1).valid :=
    io.packetIn.valid && packetEligible && !forceFirstHeader
  inputArbiter.io.in(1).bits := 1.U
  inputArbiter.io.in(2).valid :=
    io.completionIn.valid && completionEligible && !forceFirstHeader
  inputArbiter.io.in(2).bits := 2.U
  inputArbiter.io.out.ready := true.B
  io.headerIn.ready := inputArbiter.io.in(0).ready && headerEligible
  io.packetIn.ready :=
    inputArbiter.io.in(1).ready && packetEligible && !forceFirstHeader
  io.completionIn.ready :=
    inputArbiter.io.in(2).ready && completionEligible && !forceFirstHeader

  io.packetOut.valid := draining
  io.packetOut.bits.packet := packetMemory(drainIndex)

  private def setResultFromHeader(status: UInt): Unit = {
    resultPending := true.B
    result.jobId := io.headerIn.bits.jobId
    result.slot := io.headerIn.bits.slot
    result.requestedBytes := io.headerIn.bits.bytes
    result.actualBytes := io.headerIn.bits.bytes
    result.spmWordAddress := io.headerIn.bits.spmWordAddress
    result.dmaTag := io.headerIn.bits.dmaTag
    result.packetCount := io.headerIn.bits.packetCount
    result.status := status
  }

  private def setResultFromRetained(status: UInt): Unit = {
    resultPending := true.B
    result.jobId := header.jobId
    result.slot := header.slot
    result.requestedBytes := header.bytes
    result.actualBytes := Mux(
      completionPending, completion.actualBytes, header.bytes)
    result.spmWordAddress := header.spmWordAddress
    result.dmaTag := header.dmaTag
    result.packetCount := header.packetCount
    result.status := status
  }

  private def setHeaderError(reason: UInt): Unit = {
    errorPending := true.B
    error.jobId := io.headerIn.bits.jobId
    error.slot := io.headerIn.bits.slot
    error.requestedBytes := io.headerIn.bits.bytes
    error.actualBytes := io.headerIn.bits.bytes
    error.spmWordAddress := io.headerIn.bits.spmWordAddress
    error.dmaTag := io.headerIn.bits.dmaTag
    error.operation := Operation.Header
    error.reason := reason
  }

  private def setPacketError(reason: UInt): Unit = {
    errorPending := true.B
    error.jobId := Mux(headerActive, header.jobId, 0.U)
    error.slot := Mux(headerActive, header.slot, 0.U)
    error.requestedBytes := Mux(headerActive, header.bytes, 0.U)
    error.actualBytes := Mux(headerActive, header.bytes, 0.U)
    error.spmWordAddress := Mux(
      headerActive, header.spmWordAddress, 0.U)
    error.dmaTag := Mux(headerActive, header.dmaTag, 0.U)
    error.operation := Operation.Packet
    error.reason := reason
  }

  private def setCompletionError(reason: UInt): Unit = {
    errorPending := true.B
    error.jobId := io.completionIn.bits.jobId
    error.slot := io.completionIn.bits.slot
    error.requestedBytes := io.completionIn.bits.requestedBytes
    error.actualBytes := io.completionIn.bits.actualBytes
    error.spmWordAddress := io.completionIn.bits.spmWordAddress
    error.dmaTag := io.completionIn.bits.dmaTag
    error.operation := Operation.Completion
    error.reason := reason
  }

  when(io.resultOut.fire) {
    resultPending := false.B
  }
  when(io.errorOut.fire) {
    errorPending := false.B
  }

  when(io.headerIn.fire) {
    when(!canAcceptNewHeader) {
      setHeaderError(Reason.DuplicateEvent)
    }.elsewhen(!headerValid) {
      setResultFromHeader(headerStatus)
    }.elsewhen(
      completionPending &&
        !completionMatches(io.headerIn.bits, completion)) {
      setResultFromHeader(CgraLaunchStatus.IdentityMismatch)
    }.otherwise {
      header := io.headerIn.bits
      headerActive := true.B
      capturedPackets := 0.U
      packetsComplete := false.B
      drainIndex := 0.U
    }
  }

  when(io.packetIn.fire) {
    when(!collectingPacket) {
      setPacketError(Reason.UnexpectedEvent)
    }.elsewhen(!packetIsLaunch) {
      setPacketError(Reason.NonLaunchPacket)
      setResultFromRetained(CgraLaunchStatus.InvalidPacket)
      headerActive := false.B
      completionPending := false.B
      capturedPackets := 0.U
      packetsComplete := false.B
    }.otherwise {
      packetMemory(capturedPackets(
        log2Ceil(params.packetCapacity) - 1, 0)) := io.packetIn.bits.packet
      capturedPackets := capturedPackets + 1.U
      when(capturedPackets + 1.U === header.packetCount) {
        packetsComplete := true.B
      }
    }
  }

  when(io.completionIn.fire) {
    when(!canStoreCompletion) {
      setCompletionError(Reason.DuplicateEvent)
    }.elsewhen(headerActive && !incomingIdentityMatchesActive) {
      setCompletionError(Reason.IdentityMismatch)
    }.elsewhen(completionLengthMismatch) {
      setCompletionError(Reason.LengthMismatch)
    }.elsewhen(!completionStructurallyValid) {
      setCompletionError(Reason.MalformedCompletion)
    }.otherwise {
      completion := io.completionIn.bits
      completionPending := true.B
    }
  }

  when(
    headerActive && packetsComplete && completionPending && !draining &&
      !resultPending) {
    assert(completionMatches(header, completion))
    when(completionSucceeded(completion)) {
      draining := true.B
      drainIndex := 0.U
    }.otherwise {
      val failureStatus = Mux(
        completion.consumerStatus =/= CgraConsumerStatus.Success,
        CgraLaunchStatus.ConsumerFailure,
        CgraLaunchStatus.ProducerFailure)
      setResultFromRetained(failureStatus)
      headerActive := false.B
      completionPending := false.B
      capturedPackets := 0.U
      packetsComplete := false.B
    }
  }

  when(io.packetOut.fire) {
    acceptedPacketCount := acceptedPacketCount + 1.U
    when(drainIndex === header.packetCount - 1.U) {
      setResultFromRetained(CgraLaunchStatus.LaunchAccepted)
      acceptedSequenceCount := acceptedSequenceCount + 1.U
      headerActive := false.B
      completionPending := false.B
      capturedPackets := 0.U
      packetsComplete := false.B
      draining := false.B
      drainIndex := 0.U
    }.otherwise {
      drainIndex := drainIndex + 1.U
    }
  }

  private def assertStable[T <: Data](channel: DecoupledIO[T]): Unit = {
    val blocked = channel.valid && !channel.ready
    val wasBlocked = RegNext(blocked, false.B)
    val heldBits = RegEnable(channel.bits.asUInt, blocked)
    when(wasBlocked) {
      assert(channel.valid)
      assert(channel.bits.asUInt === heldBits)
    }
  }
  assertStable(io.packetOut)
  assertStable(io.resultOut)
  assertStable(io.errorOut)
  assert(PopCount(Seq(
    io.headerIn.fire, io.packetIn.fire, io.completionIn.fire)) <= 1.U)

  when(draining) {
    assert(headerActive)
    assert(packetsComplete)
    assert(completionPending)
    assert(completionSucceeded(completion))
    assert(completionMatches(header, completion))
  }
  when(io.packetOut.valid) {
    assert(io.packetOut.bits.packet(
      params.packetCommandLsb + params.packetCommandWidth - 1,
      params.packetCommandLsb) === params.launchCommand.U)
  }
}

/** Fixed-priority arbitration at the existing packet-FIFO boundary. DMA keeps
  * its atomic six-packet priority, CPU raw/config traffic is next, and the
  * automatic launch sequence backpressures behind both.
  */
class CgraPacketFifoInputArbiter(packetWidth: Int) extends Module {
  require(packetWidth > 0)

  val io = IO(new Bundle {
    val dmaPacketIn = Flipped(Decoupled(UInt(packetWidth.W)))
    val cpuPacketIn = Flipped(Decoupled(UInt(packetWidth.W)))
    val launchPacketIn = Flipped(Decoupled(UInt(packetWidth.W)))
    val packetOut = Decoupled(UInt(packetWidth.W))
  })

  private val selectDma = 0.U(2.W)
  private val selectCpu = 1.U(2.W)
  private val selectLaunch = 2.U(2.W)
  private val prioritySelection = Mux(
    io.dmaPacketIn.valid,
    selectDma,
    Mux(io.cpuPacketIn.valid, selectCpu, selectLaunch))
  private val selectionLocked = RegInit(false.B)
  private val lockedSelection = Reg(UInt(2.W))
  private val selection = Mux(
    selectionLocked, lockedSelection, prioritySelection)

  io.packetOut.valid := MuxLookup(selection, false.B)(Seq(
    selectDma -> io.dmaPacketIn.valid,
    selectCpu -> io.cpuPacketIn.valid,
    selectLaunch -> io.launchPacketIn.valid))
  io.packetOut.bits := MuxLookup(selection, 0.U)(Seq(
    selectDma -> io.dmaPacketIn.bits,
    selectCpu -> io.cpuPacketIn.bits,
    selectLaunch -> io.launchPacketIn.bits))
  io.dmaPacketIn.ready := io.packetOut.ready && Mux(
    selectionLocked, lockedSelection === selectDma, true.B)
  io.cpuPacketIn.ready := io.packetOut.ready && Mux(
    selectionLocked,
    lockedSelection === selectCpu,
    !io.dmaPacketIn.valid)
  io.launchPacketIn.ready := io.packetOut.ready && Mux(
    selectionLocked,
    lockedSelection === selectLaunch,
    !io.dmaPacketIn.valid && !io.cpuPacketIn.valid)

  when(!selectionLocked && io.packetOut.valid && !io.packetOut.ready) {
    selectionLocked := true.B
    lockedSelection := selection
  }.elsewhen(selectionLocked && io.packetOut.fire) {
    selectionLocked := false.B
  }

  val blocked = io.packetOut.valid && !io.packetOut.ready
  val wasBlocked = RegNext(blocked, false.B)
  val heldBits = RegEnable(io.packetOut.bits, blocked)
  when(wasBlocked) {
    assert(io.packetOut.valid)
    assert(io.packetOut.bits === heldBits)
  }
}
