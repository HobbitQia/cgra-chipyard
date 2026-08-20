package chipyard.example

import chisel3._
import chisel3.util._

import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams}

object CgraConsumerStatus {
  val Width = 32
  val Success = 0.U(Width.W)
  val InvalidJob = 1.U(Width.W)
  val InvalidSlot = 2.U(Width.W)
  val InvalidLength = 3.U(Width.W)
  val SpmRange = 4.U(Width.W)
  val ProducerFailure = 5.U(Width.W)
}

object CgraConsumerError {
  object Operation {
    val Width = 2
    val Ready = 0.U(Width.W)
    val ReadStart = 1.U(Width.W)
    val DmaDone = 2.U(Width.W)
  }

  object Reason {
    val Width = 3
    val UnexpectedEvent = 1.U(Width.W)
    val IdentityMismatch = 2.U(Width.W)
    val TagMismatch = 3.U(Width.W)
    val LengthMismatch = 4.U(Width.W)
    val DuplicateEvent = 5.U(Width.W)
  }
}

case class CgraConsumerPullAdapterParams(
  slotBases: Seq[BigInt],
  slotSizeBytes: Int,
  publicationRowBytes: Int,
  dmaBeatBytes: Int,
  spmWords: Int,
  spmWordBytes: Int,
  spmAddressWidth: Int,
  dmaTagWidth: Int,
  dramAddressWidth: Int) {
  require(slotBases.nonEmpty)
  require(slotBases.distinct.size == slotBases.size)
  require(slotSizeBytes > 0)
  require(isPow2(publicationRowBytes))
  require(isPow2(dmaBeatBytes))
  require(slotSizeBytes % publicationRowBytes == 0)
  require(publicationRowBytes % dmaBeatBytes == 0)
  require(spmWords > 0)
  require(isPow2(spmWordBytes))
  require(spmWords <= (1 << spmAddressWidth))
  require(dmaTagWidth > 0)
  require(dramAddressWidth > 0)
  require(slotBases.forall(_ % dmaBeatBytes == 0))

  val slotCount: Int = slotBases.size
  val spmCapacityBytes: Int = spmWords * spmWordBytes
}

object CgraConsumerPullAdapterParams {
  val production: CgraConsumerPullAdapterParams = {
    val dma = CGRAGenerated.params.dma
    CgraConsumerPullAdapterParams(
      slotBases = GemminiExternalSpadGenerated.outputSlotBases,
      slotSizeBytes = GemminiExternalSpadGenerated.outputSlotSizeBytes,
      publicationRowBytes = GemminiExternalSpadGenerated.fullWidthRowBytes,
      dmaBeatBytes = CGRAGenerated.params.dma.dramDataWidth / 8,
      spmWords = dma.spmWords,
      spmWordBytes = CGRAGenerated.params.dataPayloadWidth / 8,
      spmAddressWidth = dma.spmAddrWidth,
      dmaTagWidth = dma.tagWidth,
      dramAddressWidth = dma.dramAddrWidth)
  }
}

class CgraConsumerPullDescriptor(params: CgraConsumerPullAdapterParams)
    extends Bundle {
  val jobId = UInt(SpmTransferProtocol.JobIdWidth.W)
  val slot = UInt(SpmTransferProtocol.SlotIdWidth.W)
  val bytes = UInt(SpmTransferProtocol.LengthWidth.W)
  val spmWordAddress = UInt(params.spmAddressWidth.W)
  val dmaTag = UInt(params.dmaTagWidth.W)
}

class CgraConsumerCompletion(params: CgraConsumerPullAdapterParams)
    extends Bundle {
  val jobId = UInt(SpmTransferProtocol.JobIdWidth.W)
  val slot = UInt(SpmTransferProtocol.SlotIdWidth.W)
  val actualBytes = UInt(SpmTransferProtocol.LengthWidth.W)
  val dmaTag = UInt(params.dmaTagWidth.W)
  val consumerStatus = UInt(CgraConsumerStatus.Width.W)
  val producerStatus = UInt(SpmTransferProtocol.StatusWidth.W)
}

class CgraConsumerProtocolError(params: CgraConsumerPullAdapterParams)
    extends Bundle {
  val jobId = UInt(SpmTransferProtocol.JobIdWidth.W)
  val slot = UInt(SpmTransferProtocol.SlotIdWidth.W)
  val dmaTag = UInt(params.dmaTagWidth.W)
  val operation = UInt(CgraConsumerError.Operation.Width.W)
  val reason = UInt(CgraConsumerError.Reason.Width.W)
}

class CgraAutomaticDmaCommand(params: CgraConsumerPullAdapterParams)
    extends Bundle {
  val jobId = UInt(SpmTransferProtocol.JobIdWidth.W)
  val slot = UInt(SpmTransferProtocol.SlotIdWidth.W)
  val sourceAddress = UInt(params.dramAddressWidth.W)
  val spmWordAddress = UInt(params.spmAddressWidth.W)
  val bytes = UInt(SpmTransferProtocol.LengthWidth.W)
  val dmaTag = UInt(params.dmaTagWidth.W)
}

class CgraAutomaticDmaEvent(params: CgraConsumerPullAdapterParams)
    extends Bundle {
  val jobId = UInt(SpmTransferProtocol.JobIdWidth.W)
  val slot = UInt(SpmTransferProtocol.SlotIdWidth.W)
  val dmaTag = UInt(params.dmaTagWidth.W)
}

/** Clock-safe typed bridge between the pbus transfer endpoint and the CGRA
  * tile. Each direction is a safe singleton asynchronous queue.
  */
class CgraConsumerAsyncLink(params: CgraConsumerPullAdapterParams)
    extends Bundle {
  private val crossing = AsyncQueueParams.singleton()
  val dmaCommand = new AsyncBundle(new CgraAutomaticDmaCommand(params), crossing)
  val readStart = Flipped(
    new AsyncBundle(new CgraAutomaticDmaEvent(params), crossing))
  val dmaDone = Flipped(
    new AsyncBundle(new CgraAutomaticDmaEvent(params), crossing))
}

/** Thin consumer-pull control adapter. Payload movement is delegated to the
  * existing CGRA semantic DMA path; this module only sequences typed events.
  */
class CgraConsumerPullAdapter(params: CgraConsumerPullAdapterParams)
    extends Module {
  val io = IO(new Bundle {
    val descriptorIn = Flipped(
      Decoupled(new CgraConsumerPullDescriptor(params)))
    val requestOut = Decoupled(new SpmTransferRequest)
    val readyIn = Flipped(Decoupled(new SpmTransferReady))
    val readStartOut = Decoupled(new SpmTransferIdentity)
    val releaseOut = Decoupled(new SpmTransferIdentity)
    val dmaCommandOut = Decoupled(new CgraAutomaticDmaCommand(params))
    val dmaReadStartIn = Flipped(
      Decoupled(new CgraAutomaticDmaEvent(params)))
    val dmaDoneIn = Flipped(Decoupled(new CgraAutomaticDmaEvent(params)))
    val completionOut = Decoupled(new CgraConsumerCompletion(params))
    val errorOut = Decoupled(new CgraConsumerProtocolError(params))
    val active = Output(Bool())
    val dmaIssued = Output(Bool())
    val readStartDelivered = Output(Bool())
    val dmaDoneSeen = Output(Bool())
    val requestCount = Output(UInt(32.W))
    val dmaCommandCount = Output(UInt(32.W))
    val readStartCount = Output(UInt(32.W))
    val dmaDoneCount = Output(UInt(32.W))
    val releaseCount = Output(UInt(32.W))
  })

  import CgraConsumerError._

  private val idle :: sendRequest :: waitReady :: issueDma :: waitDmaEvents :: releaseSlot :: enqueueCompletion :: Nil = Enum(7)
  private val state = RegInit(idle)
  private val descriptor = Reg(new CgraConsumerPullDescriptor(params))
  private val actualBytes = RegInit(0.U(SpmTransferProtocol.LengthWidth.W))
  private val producerStatus = RegInit(0.U(SpmTransferProtocol.StatusWidth.W))
  private val resultStatus = RegInit(CgraConsumerStatus.Success)
  private val readStartSeen = RegInit(false.B)
  private val readStartDelivered = RegInit(false.B)
  private val dmaDoneSeen = RegInit(false.B)
  private val requestCount = RegInit(0.U(32.W))
  private val dmaCommandCount = RegInit(0.U(32.W))
  private val readStartCount = RegInit(0.U(32.W))
  private val dmaDoneCount = RegInit(0.U(32.W))
  private val releaseCount = RegInit(0.U(32.W))

  private val completionQueue = Module(
    new Queue(new CgraConsumerCompletion(params), 2))
  io.completionOut <> completionQueue.io.deq
  completionQueue.io.enq.valid := false.B
  completionQueue.io.enq.bits := 0.U.asTypeOf(
    new CgraConsumerCompletion(params))

  private val descriptorSlotValid =
    io.descriptorIn.bits.slot < params.slotCount.U
  private val descriptorWords =
    io.descriptorIn.bits.bytes >> log2Ceil(params.spmWordBytes)
  private val descriptorSpmEnd =
    io.descriptorIn.bits.spmWordAddress +& descriptorWords
  private val descriptorStatus = WireDefault(CgraConsumerStatus.Success)
  when(io.descriptorIn.bits.jobId === 0.U) {
    descriptorStatus := CgraConsumerStatus.InvalidJob
  }.elsewhen(!descriptorSlotValid) {
    descriptorStatus := CgraConsumerStatus.InvalidSlot
  }.elsewhen(
    io.descriptorIn.bits.bytes === 0.U ||
      io.descriptorIn.bits.bytes > params.slotSizeBytes.U ||
      io.descriptorIn.bits.bytes > params.spmCapacityBytes.U ||
      (io.descriptorIn.bits.bytes &
        (params.publicationRowBytes - 1).U).orR ||
      (io.descriptorIn.bits.bytes & (params.dmaBeatBytes - 1).U).orR) {
    descriptorStatus := CgraConsumerStatus.InvalidLength
  }.elsewhen(descriptorSpmEnd > params.spmWords.U) {
    descriptorStatus := CgraConsumerStatus.SpmRange
  }
  private val descriptorValid =
    descriptorStatus === CgraConsumerStatus.Success

  io.descriptorIn.ready := state === idle && completionQueue.io.enq.ready
  when(io.descriptorIn.fire) {
    when(descriptorValid) {
      descriptor := io.descriptorIn.bits
      actualBytes := 0.U
      producerStatus := 0.U
      resultStatus := CgraConsumerStatus.Success
      readStartSeen := false.B
      readStartDelivered := false.B
      dmaDoneSeen := false.B
      state := sendRequest
    }.otherwise {
      completionQueue.io.enq.valid := true.B
      completionQueue.io.enq.bits.jobId := io.descriptorIn.bits.jobId
      completionQueue.io.enq.bits.slot := io.descriptorIn.bits.slot
      completionQueue.io.enq.bits.actualBytes := 0.U
      completionQueue.io.enq.bits.dmaTag := io.descriptorIn.bits.dmaTag
      completionQueue.io.enq.bits.consumerStatus := descriptorStatus
      completionQueue.io.enq.bits.producerStatus := 0.U
      assert(completionQueue.io.enq.ready)
    }
  }

  io.requestOut.valid := state === sendRequest
  io.requestOut.bits.jobId := descriptor.jobId
  io.requestOut.bits.slot := descriptor.slot
  io.requestOut.bits.maxBytes := descriptor.bytes
  when(io.requestOut.fire) {
    requestCount := requestCount + 1.U
    state := waitReady
  }

  private val readyExpectedState = state === waitReady
  private val readyIdentityMatches =
    io.readyIn.bits.jobId === descriptor.jobId &&
      io.readyIn.bits.slot === descriptor.slot
  private val readyLengthMatches =
    io.readyIn.bits.status =/= SpmTransferProtocol.ProducerStatus.Success ||
      io.readyIn.bits.actualBytes === descriptor.bytes
  private val readyAccepted =
    readyExpectedState && readyIdentityMatches && readyLengthMatches
  private val readyErrorReason = WireDefault(Reason.UnexpectedEvent)
  when(readyExpectedState && !readyIdentityMatches) {
    readyErrorReason := Reason.IdentityMismatch
  }.elsewhen(readyExpectedState && readyIdentityMatches &&
    !readyLengthMatches) {
    readyErrorReason := Reason.LengthMismatch
  }

  private val readStartExpectedState = state === waitDmaEvents
  private val readStartIdentityMatches =
    io.dmaReadStartIn.bits.jobId === descriptor.jobId &&
      io.dmaReadStartIn.bits.slot === descriptor.slot
  private val readStartTagMatches =
    io.dmaReadStartIn.bits.dmaTag === descriptor.dmaTag
  private val readStartAccepted = readStartExpectedState &&
    !readStartSeen && readStartIdentityMatches && readStartTagMatches
  private val readStartErrorReason = WireDefault(Reason.UnexpectedEvent)
  when(readStartExpectedState && readStartSeen) {
    readStartErrorReason := Reason.DuplicateEvent
  }.elsewhen(readStartExpectedState && !readStartIdentityMatches) {
    readStartErrorReason := Reason.IdentityMismatch
  }.elsewhen(readStartExpectedState && readStartIdentityMatches &&
    !readStartTagMatches) {
    readStartErrorReason := Reason.TagMismatch
  }

  private val dmaDoneExpectedState = state === waitDmaEvents
  private val dmaDoneIdentityMatches =
    io.dmaDoneIn.bits.jobId === descriptor.jobId &&
      io.dmaDoneIn.bits.slot === descriptor.slot
  private val dmaDoneTagMatches =
    io.dmaDoneIn.bits.dmaTag === descriptor.dmaTag
  private val dmaDoneAccepted = dmaDoneExpectedState &&
    !dmaDoneSeen && dmaDoneIdentityMatches && dmaDoneTagMatches
  private val dmaDoneErrorReason = WireDefault(Reason.UnexpectedEvent)
  when(dmaDoneExpectedState && dmaDoneSeen) {
    dmaDoneErrorReason := Reason.DuplicateEvent
  }.elsewhen(dmaDoneExpectedState && !dmaDoneIdentityMatches) {
    dmaDoneErrorReason := Reason.IdentityMismatch
  }.elsewhen(dmaDoneExpectedState && dmaDoneIdentityMatches &&
    !dmaDoneTagMatches) {
    dmaDoneErrorReason := Reason.TagMismatch
  }

  // Unexpected producer/DMA events are retained in a lossless one-entry
  // buffer. Legal events remain independent of a stalled error consumer.
  private val errorValid = RegInit(false.B)
  private val errorBits = Reg(new CgraConsumerProtocolError(params))
  io.errorOut.valid := errorValid
  io.errorOut.bits := errorBits
  private val errorCanEnqueue = !errorValid || io.errorOut.ready
  private val errorArbiter = Module(
    new RRArbiter(new CgraConsumerProtocolError(params), 3))

  errorArbiter.io.in(0).valid := io.readyIn.valid && !readyAccepted
  errorArbiter.io.in(0).bits.jobId := io.readyIn.bits.jobId
  errorArbiter.io.in(0).bits.slot := io.readyIn.bits.slot
  errorArbiter.io.in(0).bits.dmaTag := descriptor.dmaTag
  errorArbiter.io.in(0).bits.operation := Operation.Ready
  errorArbiter.io.in(0).bits.reason := readyErrorReason

  errorArbiter.io.in(1).valid :=
    io.dmaReadStartIn.valid && !readStartAccepted
  errorArbiter.io.in(1).bits.jobId := io.dmaReadStartIn.bits.jobId
  errorArbiter.io.in(1).bits.slot := io.dmaReadStartIn.bits.slot
  errorArbiter.io.in(1).bits.dmaTag := io.dmaReadStartIn.bits.dmaTag
  errorArbiter.io.in(1).bits.operation := Operation.ReadStart
  errorArbiter.io.in(1).bits.reason := readStartErrorReason

  errorArbiter.io.in(2).valid := io.dmaDoneIn.valid && !dmaDoneAccepted
  errorArbiter.io.in(2).bits.jobId := io.dmaDoneIn.bits.jobId
  errorArbiter.io.in(2).bits.slot := io.dmaDoneIn.bits.slot
  errorArbiter.io.in(2).bits.dmaTag := io.dmaDoneIn.bits.dmaTag
  errorArbiter.io.in(2).bits.operation := Operation.DmaDone
  errorArbiter.io.in(2).bits.reason := dmaDoneErrorReason

  errorArbiter.io.out.ready := errorCanEnqueue
  io.readyIn.ready := readyAccepted || errorArbiter.io.in(0).ready
  io.dmaReadStartIn.ready :=
    readStartAccepted || errorArbiter.io.in(1).ready
  io.dmaDoneIn.ready := dmaDoneAccepted || errorArbiter.io.in(2).ready

  when(errorArbiter.io.out.fire) {
    errorValid := true.B
    errorBits := errorArbiter.io.out.bits
  }.elsewhen(io.errorOut.fire) {
    errorValid := false.B
  }

  when(io.readyIn.fire && readyAccepted) {
    actualBytes := io.readyIn.bits.actualBytes
    producerStatus := io.readyIn.bits.status
    when(io.readyIn.bits.status ===
      SpmTransferProtocol.ProducerStatus.Success) {
      state := issueDma
    }.otherwise {
      resultStatus := CgraConsumerStatus.ProducerFailure
      state := releaseSlot
    }
  }

  private val slotIndexWidth = math.max(1, log2Ceil(params.slotCount))
  private val slotBases = VecInit(
    params.slotBases.map(_.U(params.dramAddressWidth.W)))
  private val selectedSlot = descriptor.slot(slotIndexWidth - 1, 0)
  io.dmaCommandOut.valid := state === issueDma
  io.dmaCommandOut.bits.jobId := descriptor.jobId
  io.dmaCommandOut.bits.slot := descriptor.slot
  io.dmaCommandOut.bits.sourceAddress := slotBases(selectedSlot)
  io.dmaCommandOut.bits.spmWordAddress := descriptor.spmWordAddress
  io.dmaCommandOut.bits.bytes := actualBytes
  io.dmaCommandOut.bits.dmaTag := descriptor.dmaTag
  when(io.dmaCommandOut.fire) {
    dmaCommandCount := dmaCommandCount + 1.U
    state := waitDmaEvents
  }

  when(io.dmaReadStartIn.fire && readStartAccepted) {
    readStartSeen := true.B
    readStartCount := readStartCount + 1.U
  }
  when(io.dmaDoneIn.fire && dmaDoneAccepted) {
    dmaDoneSeen := true.B
    dmaDoneCount := dmaDoneCount + 1.U
  }

  io.readStartOut.valid :=
    state === waitDmaEvents && readStartSeen && !readStartDelivered
  io.readStartOut.bits.jobId := descriptor.jobId
  io.readStartOut.bits.slot := descriptor.slot
  when(io.readStartOut.fire) {
    readStartDelivered := true.B
  }

  private val willHaveReadStart =
    readStartDelivered || io.readStartOut.fire
  private val willHaveDmaDone =
    dmaDoneSeen || (io.dmaDoneIn.fire && dmaDoneAccepted)
  when(state === waitDmaEvents && willHaveReadStart && willHaveDmaDone) {
    state := releaseSlot
  }

  io.releaseOut.valid := state === releaseSlot
  io.releaseOut.bits.jobId := descriptor.jobId
  io.releaseOut.bits.slot := descriptor.slot
  when(io.releaseOut.fire) {
    releaseCount := releaseCount + 1.U
    state := enqueueCompletion
  }

  when(state === enqueueCompletion) {
    completionQueue.io.enq.valid := true.B
    completionQueue.io.enq.bits.jobId := descriptor.jobId
    completionQueue.io.enq.bits.slot := descriptor.slot
    completionQueue.io.enq.bits.actualBytes := actualBytes
    completionQueue.io.enq.bits.dmaTag := descriptor.dmaTag
    completionQueue.io.enq.bits.consumerStatus := resultStatus
    completionQueue.io.enq.bits.producerStatus := producerStatus
    when(completionQueue.io.enq.fire) {
      state := idle
    }
  }

  io.active := state =/= idle
  io.dmaIssued := state === waitDmaEvents || state === releaseSlot ||
    state === enqueueCompletion
  io.readStartDelivered := readStartDelivered
  io.dmaDoneSeen := dmaDoneSeen
  io.requestCount := requestCount
  io.dmaCommandCount := dmaCommandCount
  io.readStartCount := readStartCount
  io.dmaDoneCount := dmaDoneCount
  io.releaseCount := releaseCount

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
  assertStable(io.dmaCommandOut)
  assertStable(io.readStartOut)
  assertStable(io.releaseOut)
  assertStable(io.completionOut)
  assertStable(io.errorOut)

  when(state =/= idle) {
    assert(descriptor.jobId =/= 0.U)
    assert(descriptor.slot < params.slotCount.U)
    assert(descriptor.bytes =/= 0.U)
    assert(descriptor.bytes <= params.slotSizeBytes.U)
    assert(descriptor.bytes <= params.spmCapacityBytes.U)
    assert((descriptor.bytes & (params.publicationRowBytes - 1).U) === 0.U)
  }
  when(io.dmaCommandOut.valid) {
    assert(io.dmaCommandOut.bits.bytes === descriptor.bytes)
    assert(io.dmaCommandOut.bits.sourceAddress === slotBases(selectedSlot))
  }
  when(io.releaseOut.valid && resultStatus === CgraConsumerStatus.Success) {
    assert(readStartDelivered)
    assert(dmaDoneSeen)
  }
}
