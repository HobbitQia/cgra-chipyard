package chipyard.example

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.subsystem.{BaseSubsystem, InstantiatesHierarchicalElements, PBUS}
import freechips.rocketchip.tile.RocketTile
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{AsyncQueueParams, FromAsyncBundle, ToAsyncBundle}
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import org.chipsalliance.diplomacy.lazymodule.LazyModule

case class GemminiExternalSpadParams(
  baseAddress: BigInt,
  sizeBytes: Int,
  controlAddress: BigInt,
  spadRowBytes: Int,
  fullWidthRowStride: Int,
  outputSlotCount: Int,
  outputSlotSizeBytes: Int) {
  val fullWidthRowBytes: Int = spadRowBytes * fullWidthRowStride
  val matrixDimension: Int = outputSlotSizeBytes / fullWidthRowBytes
  val outputReservedBytes: Int = outputSlotCount * outputSlotSizeBytes
  val outputReservedBase: BigInt = baseAddress + sizeBytes - outputReservedBytes
  val outputSlotBases: Seq[BigInt] = Seq.tabulate(outputSlotCount) { index =>
    outputReservedBase + index * outputSlotSizeBytes
  }
  val outputSlotRows: Seq[Int] = outputSlotBases.map { address =>
    ((address - baseAddress) / spadRowBytes).toInt
  }
}

case object GemminiExternalSpadKey
    extends Field[Option[GemminiExternalSpadParams]](None)

/** TileLink-visible 1R1W backing for Gemmini's external scratchpad.
  *
  * System and Gemmini readers arbitrate on the read xbar. Gemmini's ordinary
  * scratchpad writes and full-width STORE_SPAD publication arbitrate on the
  * write xbar. The two directions meet only at one byte-masked 1R1W SRAM.
  * Same-line SRAM accesses are serialized through read-data capture. Once a
  * response is captured, writes may proceed while its stable D beat is held;
  * other lines may use both physical ports concurrently.
  */
class GemminiExternalSpadMemory(
  gemminiAccelerator: gemmini.Gemmini[chisel3.SInt, gemmini.Float, gemmini.Float],
  params: GemminiExternalSpadParams)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  private val readBeatBytes = params.spadRowBytes
  private val writeBeatBytes = params.fullWidthRowBytes
  private val gemminiBeatBytes = gemminiAccelerator.config.dma_buswidth / 8
  private val address = AddressSet(params.baseAddress, params.sizeBytes - 1)
  private val gemminiConfig = gemminiAccelerator.config
  private val gemminiSpadBytes = gemminiConfig.sp_capacity
    .asInstanceOf[gemmini.CapacityInKilobytes].kilobytes * 1024
  private val gemminiRows = gemminiConfig.meshRows * gemminiConfig.tileRows
  private val gemminiColumns = gemminiConfig.meshColumns * gemminiConfig.tileColumns
  private val gemminiSpadRowBytes = gemminiConfig.sp_width / 8
  private val gemminiFullWidthRowBytes =
    gemminiColumns * gemminiConfig.accType.getWidth / 8

  require(isPow2(params.sizeBytes) && params.sizeBytes >= writeBeatBytes)
  require((params.baseAddress & (params.sizeBytes - 1)) == 0)
  require(isPow2(params.spadRowBytes))
  require(isPow2(params.fullWidthRowStride))
  require(params.outputSlotCount == 2)
  require(isPow2(params.outputSlotSizeBytes))
  require(params.outputReservedBytes <= params.sizeBytes)
  require(params.outputReservedBase % params.outputSlotSizeBytes == 0)
  require(params.outputSlotBases.distinct.size == params.outputSlotCount)
  require(params.outputSlotBases.zipWithIndex.forall { case (slotBase, index) =>
    slotBase == params.outputReservedBase + index * params.outputSlotSizeBytes
  })
  require(params.outputSlotRows.zip(params.outputSlotBases).forall { case (row, slotBase) =>
    params.baseAddress + row * params.spadRowBytes == slotBase
  })
  require(params.outputSlotBases.last + params.outputSlotSizeBytes ==
    params.baseAddress + params.sizeBytes)
  require(params.baseAddress == gemminiConfig.tl_ext_mem_base)
  require(params.sizeBytes == gemminiSpadBytes)
  require(params.controlAddress ==
    GemminiExternalSpadGenerated.productionControlAddress)
  require(params.controlAddress >= params.baseAddress + params.sizeBytes)
  require(params.spadRowBytes == gemminiSpadRowBytes)
  require(params.fullWidthRowBytes == gemminiFullWidthRowBytes)
  require(params.fullWidthRowStride ==
    gemminiConfig.accType.getWidth / gemminiConfig.inputType.getWidth)
  require(gemminiRows == gemminiColumns)
  require(params.matrixDimension == gemminiRows)
  require(params.outputSlotSizeBytes == gemminiRows * gemminiFullWidthRowBytes)
  require(gemminiBeatBytes == readBeatBytes)
  require(writeBeatBytes % gemminiBeatBytes == 0)
  require(gemminiConfig.use_shared_ext_mem)
  require(gemminiConfig.use_tl_ext_mem)
  require(!gemminiConfig.sp_singleported)
  require(gemminiAccelerator.spad.spad_writer.isDefined)

  private val memoryDevice = new SimpleDevice(
    "gemmini-external-spad",
    Seq("ucbbar,gemmini-external-spad"))
  private val controlDevice = new SimpleDevice(
    "cgra-transfer-control",
    Seq("ucbbar,cgra-transfer-control"))

  private val readManager = TLManagerNode(Seq(TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(address),
      resources = memoryDevice.reg,
      regionType = RegionType.IDEMPOTENT,
      executable = false,
      supportsGet = TransferSizes(1, readBeatBytes),
      fifoId = Some(0))),
    beatBytes = readBeatBytes,
    minLatency = 1)))

  private val writeManager = TLManagerNode(Seq(TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(address),
      resources = Nil,
      regionType = RegionType.IDEMPOTENT,
      executable = false,
      supportsPutFull = TransferSizes(1, writeBeatBytes),
      supportsPutPartial = TransferSizes(1, writeBeatBytes),
      fifoId = Some(0))),
    beatBytes = writeBeatBytes,
    minLatency = 1)))

  val systemReadNode = TLIdentityNode()
  val consumerNode = BundleBridgeSource(() =>
    new CgraConsumerAsyncLink(CgraConsumerPullAdapterParams.production))
  val controlNode = TLRegisterNode(
    address = Seq(AddressSet(
      params.controlAddress,
      GemminiExternalSpadGenerated.controlPageSizeBytes - 1)),
    device = controlDevice,
    beatBytes = 8,
    concurrency = 1)

  private val readXbar = TLXbar()
  private val writeXbar = TLXbar()
  private val producerAdapterParams =
    GemminiSpadProducerAdapterParams.production
  val publicationMonitor = LazyModule(new GemminiSpadPublicationMonitor(
    producerAdapterParams))

  readManager := readXbar
  readXbar := systemReadNode
  readXbar :=* gemminiAccelerator.spad_read_nodes

  writeManager := writeXbar
  writeXbar :=* TLWidthWidget(gemminiBeatBytes) :=* TLBuffer() :=*
    gemminiAccelerator.spad_write_nodes
  writeXbar := publicationMonitor.node := TLWidthWidget(gemminiBeatBytes) :=
    TLBuffer() := gemminiAccelerator.spad.spad_writer.get.node

  override lazy val module = new MemoryImpl
  class MemoryImpl extends Impl {
    withClockAndReset(clock, reset) {
      val (read, readEdge) = readManager.in.head
      val (write, writeEdge) = writeManager.in.head
      val lineCount = params.sizeBytes / writeBeatBytes
      val lineIndexBits = log2Ceil(lineCount)
      val lineOffsetBits = log2Ceil(writeBeatBytes)
      val readBeatIndexBits = log2Ceil(writeBeatBytes / readBeatBytes)
      val mem = SyncReadMem(lineCount, Vec(writeBeatBytes, UInt(8.W)))

      val transferEndpoint = Module(new SpmTransferEndpoint(
        SpmTransferEndpointParams.production))
      val producerAdapter = Module(new GemminiSpadProducerAdapter(
        producerAdapterParams))
      val consumerAdapter = Module(new CgraConsumerPullAdapter(
        CgraConsumerPullAdapterParams.production))
      transferEndpoint.io.requestOut <> producerAdapter.io.requestIn
      transferEndpoint.io.readyIn <> producerAdapter.io.readyOut
      producerAdapter.io.writerA <> publicationMonitor.module.io.writerA
      producerAdapter.io.writerD <> publicationMonitor.module.io.writerD

      val consumerLink = consumerNode.out(0)._1
      consumerLink.dmaCommand <> ToAsyncBundle(
        consumerAdapter.io.dmaCommandOut, AsyncQueueParams.singleton())
      consumerAdapter.io.dmaReadStartIn <>
        FromAsyncBundle(consumerLink.readStart)
      consumerAdapter.io.dmaDoneIn <>
        FromAsyncBundle(consumerLink.dmaDone)
      val launchParams = CgraComputeLaunchGateParams.production
      val completionToCgra = Wire(
        Decoupled(new CgraConsumerCompletion(
          CgraConsumerPullAdapterParams.production)))
      val launchHeaderToCgra = Wire(
        Decoupled(new CgraLaunchSequenceHeader(launchParams)))
      val launchPacketToCgra = Wire(
        Decoupled(new CgraLaunchPacket(launchParams)))
      val launchResultFromCgra = Wire(
        Decoupled(new CgraLaunchResult(launchParams)))
      val launchErrorFromCgra = Wire(
        Decoupled(new CgraLaunchProtocolError(launchParams)))
      val computeCompletionFromCgra = Wire(
        Decoupled(new CgraComputeCompletion(launchParams)))
      val computeErrorFromCgra = Wire(Decoupled(
        new CgraComputeCompletionProtocolError(launchParams)))
      consumerLink.completion <> ToAsyncBundle(
        completionToCgra, AsyncQueueParams.singleton())
      consumerLink.launchHeader <> ToAsyncBundle(
        launchHeaderToCgra, AsyncQueueParams.singleton())
      consumerLink.launchPacket <> ToAsyncBundle(
        launchPacketToCgra, AsyncQueueParams.singleton())
      launchResultFromCgra <> FromAsyncBundle(consumerLink.launchResult)
      launchErrorFromCgra <> FromAsyncBundle(consumerLink.launchError)
      computeCompletionFromCgra <>
        FromAsyncBundle(consumerLink.computeCompletion)
      computeErrorFromCgra <> FromAsyncBundle(consumerLink.computeError)
      transferEndpoint.io.errorOut.ready := true.B

      val readLineIndex = read.a.bits.address(
        lineOffsetBits + lineIndexBits - 1, lineOffsetBits)
      val writeLineIndex = write.a.bits.address(
        lineOffsetBits + lineIndexBits - 1, lineOffsetBits)
      val writePending = RegInit(false.B)
      val writeSource = Reg(chiselTypeOf(write.a.bits.source))
      val writeSize = Reg(chiselTypeOf(write.a.bits.size))
      val writeCanCommit = !writePending
      val sameLineRequest = read.a.valid && write.a.valid &&
        readLineIndex === writeLineIndex

      val readSramPending = RegInit(false.B)
      val readResponseValid = RegInit(false.B)
      val readSource = Reg(chiselTypeOf(read.a.bits.source))
      val readSize = Reg(chiselTypeOf(read.a.bits.size))
      val readBeat = Reg(UInt(readBeatIndexBits.W))
      val readResponseLineIndex = Reg(UInt(lineIndexBits.W))
      val readResponseData = Reg(UInt((readBeatBytes * 8).W))
      val readLine = mem.read(readLineIndex, read.a.fire)
      val readBeats = readLine.asUInt.asTypeOf(
        Vec(writeBeatBytes / readBeatBytes, UInt((readBeatBytes * 8).W)))
      val writeConflictsWithReadCapture = readSramPending && write.a.valid &&
        writeLineIndex === readResponseLineIndex

      read.a.ready := !readSramPending && !readResponseValid &&
        !(writeCanCommit && sameLineRequest)
      read.d.valid := readResponseValid
      read.d.bits := readEdge.AccessAck(readSource, readSize, readResponseData)
      read.b.valid := false.B
      read.c.ready := true.B
      read.e.ready := true.B

      when (read.a.fire) {
        assert(read.a.bits.opcode === TLMessages.Get)
        readSramPending := true.B
        readSource := read.a.bits.source
        readSize := read.a.bits.size
        readBeat := read.a.bits.address(
          lineOffsetBits - 1, log2Ceil(readBeatBytes))
        readResponseLineIndex := readLineIndex
      }
      when (readSramPending) {
        readSramPending := false.B
        readResponseValid := true.B
        readResponseData := readBeats(readBeat)
      }
      when (read.d.fire) {
        readResponseValid := false.B
      }

      val readResponseBlocked = read.d.valid && !read.d.ready
      val previousReadResponseBlocked = RegNext(readResponseBlocked, false.B)
      val previousReadResponseBits = RegEnable(read.d.bits, readResponseBlocked)
      when (previousReadResponseBlocked) {
        assert(read.d.valid)
        assert(read.d.bits.asUInt === previousReadResponseBits.asUInt)
      }

      write.a.ready := writeCanCommit && !writeConflictsWithReadCapture
      write.d.valid := writePending
      write.d.bits := writeEdge.AccessAck(writeSource, writeSize)
      write.b.valid := false.B
      write.c.ready := true.B
      write.e.ready := true.B

      when (write.a.fire) {
        assert(!(readSramPending && writeLineIndex === readResponseLineIndex))
        assert(write.a.bits.opcode === TLMessages.PutFullData ||
          write.a.bits.opcode === TLMessages.PutPartialData)
        mem.write(
          writeLineIndex,
          write.a.bits.data.asTypeOf(Vec(writeBeatBytes, UInt(8.W))),
          write.a.bits.mask.asBools)
        writePending := true.B
        writeSource := write.a.bits.source
        writeSize := write.a.bits.size
      }
      when (write.d.fire) {
        writePending := false.B
      }

      // Production control ABI. Only typed metadata and full launch packets
      // enter these bounded queues; payload data remains on the external-SPAD
      // and CGRA DMA paths. Submit writes complete at queue acceptance and do
      // not wait for publication, DMA, launch, or compute completion.
      val controlQueues = Module(new CgraTransferControlQueues(launchParams))
      val pullJobId = RegInit(0.U(32.W))
      val pullSlot = RegInit(0.U(32.W))
      val pullBytes = RegInit(0.U(32.W))
      val pullSpmWordAddress = RegInit(0.U(32.W))
      val pullDmaTag = RegInit(0.U(32.W))
      val pullSubmit = Wire(Decoupled(UInt(1.W)))
      controlQueues.io.descriptorSubmit.valid :=
        pullSubmit.valid && pullSubmit.bits.asBool
      controlQueues.io.descriptorSubmit.bits.jobId := pullJobId
      controlQueues.io.descriptorSubmit.bits.slot := pullSlot
      controlQueues.io.descriptorSubmit.bits.bytes := pullBytes
      controlQueues.io.descriptorSubmit.bits.spmWordAddress :=
        pullSpmWordAddress
      controlQueues.io.descriptorSubmit.bits.dmaTag := pullDmaTag
      pullSubmit.ready := controlQueues.io.descriptorSubmit.ready

      val launchJobId = RegInit(0.U(32.W))
      val launchSlot = RegInit(0.U(32.W))
      val launchBytes = RegInit(0.U(32.W))
      val launchSpmWordAddress = RegInit(0.U(32.W))
      val launchDmaTag = RegInit(0.U(32.W))
      val launchPacketCount = RegInit(0.U(32.W))
      val launchSubmit = Wire(Decoupled(UInt(1.W)))
      controlQueues.io.launchHeaderSubmit.valid :=
        launchSubmit.valid && launchSubmit.bits.asBool
      controlQueues.io.launchHeaderSubmit.bits.jobId := launchJobId
      controlQueues.io.launchHeaderSubmit.bits.slot := launchSlot
      controlQueues.io.launchHeaderSubmit.bits.bytes := launchBytes
      controlQueues.io.launchHeaderSubmit.bits.spmWordAddress :=
        launchSpmWordAddress
      controlQueues.io.launchHeaderSubmit.bits.dmaTag := launchDmaTag
      controlQueues.io.launchHeaderSubmit.bits.packetCount :=
        launchPacketCount
      launchSubmit.ready := controlQueues.io.launchHeaderSubmit.ready

      val launchPacketLo = RegInit(0.U(64.W))
      val launchPacketMid = RegInit(0.U(64.W))
      val launchPacketHi = RegInit(0.U(64.W))
      val launchPacketTop = RegInit(0.U(64.W))
      val launchPacketSubmit = Wire(Decoupled(UInt(1.W)))
      val launchPacketBits = Cat(
        launchPacketTop, launchPacketHi, launchPacketMid, launchPacketLo)
      controlQueues.io.launchPacketSubmit.valid :=
        launchPacketSubmit.valid && launchPacketSubmit.bits.asBool
      controlQueues.io.launchPacketSubmit.bits.packet :=
        launchPacketBits(launchParams.packetWidth - 1, 0)
      launchPacketSubmit.ready := controlQueues.io.launchPacketSubmit.ready

      val launchResultPop = Wire(Decoupled(UInt(1.W)))
      val launchResultSnapshot = RegInit(0.U.asTypeOf(
        new CgraLaunchResult(launchParams)))
      launchResultPop.ready := Mux(
        launchResultPop.bits.asBool,
        controlQueues.io.launchResultOut.valid,
        true.B)
      controlQueues.io.launchResultOut.ready :=
        launchResultPop.valid && launchResultPop.bits.asBool
      when(controlQueues.io.launchResultOut.fire) {
        launchResultSnapshot := controlQueues.io.launchResultOut.bits
      }

      val launchErrorPop = Wire(Decoupled(UInt(1.W)))
      val launchErrorSnapshot = RegInit(0.U.asTypeOf(
        new CgraTransferControlProtocolError))
      launchErrorPop.ready := Mux(
        launchErrorPop.bits.asBool,
        controlQueues.io.launchErrorOut.valid,
        true.B)
      controlQueues.io.launchErrorOut.ready :=
        launchErrorPop.valid && launchErrorPop.bits.asBool
      when(controlQueues.io.launchErrorOut.fire) {
        launchErrorSnapshot := controlQueues.io.launchErrorOut.bits
      }

      val computeResultPop = Wire(Decoupled(UInt(1.W)))
      val computeResultSnapshot = RegInit(0.U.asTypeOf(
        new CgraComputeCompletion(launchParams)))
      computeResultPop.ready := Mux(
        computeResultPop.bits.asBool,
        controlQueues.io.computeCompletionOut.valid,
        true.B)
      controlQueues.io.computeCompletionOut.ready :=
        computeResultPop.valid && computeResultPop.bits.asBool
      when(controlQueues.io.computeCompletionOut.fire) {
        computeResultSnapshot := controlQueues.io.computeCompletionOut.bits
      }

      val computeErrorPop = Wire(Decoupled(UInt(1.W)))
      val computeErrorSnapshot = RegInit(0.U.asTypeOf(
        new CgraComputeCompletionProtocolError(launchParams)))
      computeErrorPop.ready := Mux(
        computeErrorPop.bits.asBool,
        controlQueues.io.computeErrorOut.valid,
        true.B)
      controlQueues.io.computeErrorOut.ready :=
        computeErrorPop.valid && computeErrorPop.bits.asBool
      when(controlQueues.io.computeErrorOut.fire) {
        computeErrorSnapshot := controlQueues.io.computeErrorOut.bits
      }

      import CgraTransferControlGenerated._
      controlNode.regmap(
        PULL_JOB_ID -> Seq(RegField(32, pullJobId)),
        PULL_SLOT -> Seq(RegField(32, pullSlot)),
        PULL_BYTES -> Seq(RegField(32, pullBytes)),
        PULL_SPM_WORD_ADDRESS -> Seq(RegField(32, pullSpmWordAddress)),
        PULL_DMA_TAG -> Seq(RegField(32, pullDmaTag)),
        PULL_SUBMIT -> Seq(RegField.w(1, pullSubmit)),
        LAUNCH_JOB_ID -> Seq(RegField(32, launchJobId)),
        LAUNCH_SLOT -> Seq(RegField(32, launchSlot)),
        LAUNCH_BYTES -> Seq(RegField(32, launchBytes)),
        LAUNCH_SPM_WORD_ADDRESS -> Seq(RegField(32, launchSpmWordAddress)),
        LAUNCH_DMA_TAG -> Seq(RegField(32, launchDmaTag)),
        LAUNCH_PACKET_COUNT -> Seq(RegField(32, launchPacketCount)),
        LAUNCH_SUBMIT -> Seq(RegField.w(1, launchSubmit)),
        PACKET_LO -> Seq(RegField(64, launchPacketLo)),
        PACKET_MID -> Seq(RegField(64, launchPacketMid)),
        PACKET_HI -> Seq(RegField(64, launchPacketHi)),
        PACKET_TOP -> Seq(RegField(64, launchPacketTop)),
        PACKET_SUBMIT -> Seq(RegField.w(1, launchPacketSubmit)),
        LAUNCH_RESULT_VALID -> Seq(RegField.r(1,
          controlQueues.io.launchResultOut.valid)),
        LAUNCH_RESULT_POP -> Seq(RegField.w(1, launchResultPop)),
        LAUNCH_RESULT_JOB_ID -> Seq(RegField.r(32,
          launchResultSnapshot.jobId)),
        LAUNCH_RESULT_SLOT -> Seq(RegField.r(32,
          launchResultSnapshot.slot)),
        LAUNCH_RESULT_REQUESTED_BYTES -> Seq(RegField.r(32,
          launchResultSnapshot.requestedBytes)),
        LAUNCH_RESULT_ACTUAL_BYTES -> Seq(RegField.r(32,
          launchResultSnapshot.actualBytes)),
        LAUNCH_RESULT_SPM_WORD_ADDRESS -> Seq(RegField.r(32,
          launchResultSnapshot.spmWordAddress)),
        LAUNCH_RESULT_DMA_TAG -> Seq(RegField.r(32,
          launchResultSnapshot.dmaTag)),
        LAUNCH_RESULT_PACKET_COUNT -> Seq(RegField.r(32,
          launchResultSnapshot.packetCount)),
        LAUNCH_RESULT_STATUS -> Seq(RegField.r(32,
          launchResultSnapshot.status)),
        LAUNCH_ERROR_VALID -> Seq(RegField.r(1,
          controlQueues.io.launchErrorOut.valid)),
        LAUNCH_ERROR_POP -> Seq(RegField.w(1, launchErrorPop)),
        LAUNCH_ERROR_JOB_ID -> Seq(RegField.r(32,
          launchErrorSnapshot.jobId)),
        LAUNCH_ERROR_SLOT -> Seq(RegField.r(32,
          launchErrorSnapshot.slot)),
        LAUNCH_ERROR_REQUESTED_BYTES -> Seq(RegField.r(32,
          launchErrorSnapshot.requestedBytes)),
        LAUNCH_ERROR_ACTUAL_BYTES -> Seq(RegField.r(32,
          launchErrorSnapshot.actualBytes)),
        LAUNCH_ERROR_SPM_WORD_ADDRESS -> Seq(RegField.r(32,
          launchErrorSnapshot.spmWordAddress)),
        LAUNCH_ERROR_DMA_TAG -> Seq(RegField.r(32,
          launchErrorSnapshot.dmaTag)),
        LAUNCH_ERROR_OPERATION -> Seq(RegField.r(32,
          launchErrorSnapshot.operation)),
        LAUNCH_ERROR_REASON -> Seq(RegField.r(32,
          launchErrorSnapshot.reason)),
        COMPUTE_RESULT_VALID -> Seq(RegField.r(1,
          controlQueues.io.computeCompletionOut.valid)),
        COMPUTE_RESULT_POP -> Seq(RegField.w(1, computeResultPop)),
        COMPUTE_RESULT_JOB_ID -> Seq(RegField.r(32,
          computeResultSnapshot.jobId)),
        COMPUTE_RESULT_SLOT -> Seq(RegField.r(32,
          computeResultSnapshot.slot)),
        COMPUTE_RESULT_REQUESTED_BYTES -> Seq(RegField.r(32,
          computeResultSnapshot.requestedBytes)),
        COMPUTE_RESULT_ACTUAL_BYTES -> Seq(RegField.r(32,
          computeResultSnapshot.actualBytes)),
        COMPUTE_RESULT_SPM_WORD_ADDRESS -> Seq(RegField.r(32,
          computeResultSnapshot.spmWordAddress)),
        COMPUTE_RESULT_DMA_TAG -> Seq(RegField.r(32,
          computeResultSnapshot.dmaTag)),
        COMPUTE_RESULT_PACKET_COUNT -> Seq(RegField.r(32,
          computeResultSnapshot.packetCount)),
        COMPUTE_RESULT_DATA -> Seq(RegField.r(32,
          computeResultSnapshot.completeData)),
        COMPUTE_RESULT_STATUS -> Seq(RegField.r(32,
          computeResultSnapshot.status)),
        COMPUTE_ERROR_VALID -> Seq(RegField.r(1,
          controlQueues.io.computeErrorOut.valid)),
        COMPUTE_ERROR_POP -> Seq(RegField.w(1, computeErrorPop)),
        COMPUTE_ERROR_JOB_ID -> Seq(RegField.r(32,
          computeErrorSnapshot.jobId)),
        COMPUTE_ERROR_SLOT -> Seq(RegField.r(32,
          computeErrorSnapshot.slot)),
        COMPUTE_ERROR_REQUESTED_BYTES -> Seq(RegField.r(32,
          computeErrorSnapshot.requestedBytes)),
        COMPUTE_ERROR_ACTUAL_BYTES -> Seq(RegField.r(32,
          computeErrorSnapshot.actualBytes)),
        COMPUTE_ERROR_SPM_WORD_ADDRESS -> Seq(RegField.r(32,
          computeErrorSnapshot.spmWordAddress)),
        COMPUTE_ERROR_DMA_TAG -> Seq(RegField.r(32,
          computeErrorSnapshot.dmaTag)),
        COMPUTE_ERROR_OPERATION -> Seq(RegField.r(32,
          computeErrorSnapshot.operation)),
        COMPUTE_ERROR_REASON -> Seq(RegField.r(32,
          computeErrorSnapshot.reason)))

      transferEndpoint.io.requestIn <> consumerAdapter.io.requestOut
      consumerAdapter.io.readyIn <> transferEndpoint.io.readyOut
      transferEndpoint.io.readStartIn <> consumerAdapter.io.readStartOut
      transferEndpoint.io.releaseIn <> consumerAdapter.io.releaseOut
      consumerAdapter.io.descriptorIn <> controlQueues.io.descriptorOut
      completionToCgra <> consumerAdapter.io.completionOut
      launchHeaderToCgra <> controlQueues.io.launchHeaderOut
      launchPacketToCgra <> controlQueues.io.launchPacketOut
      controlQueues.io.launchResultIn <> launchResultFromCgra
      controlQueues.io.launchErrorIn <> launchErrorFromCgra
      controlQueues.io.computeCompletionIn <> computeCompletionFromCgra
      controlQueues.io.computeErrorIn <> computeErrorFromCgra
      consumerAdapter.io.errorOut.ready := true.B
    }
  }
}

trait CanHaveGemminiExternalSpad {
  this: BaseSubsystem with InstantiatesHierarchicalElements =>
  private val pbus = locateTLBusWrapper(PBUS)

  val gemminiExternalSpad = p(GemminiExternalSpadKey).map { params =>
    val gemminis = totalTiles.values.toSeq.flatMap {
      case tile: RocketTile => tile.roccs.collect {
        case accelerator: gemmini.Gemmini[_, _, _] => accelerator
      }
      case _ => Nil
    }
    require(gemminis.size == 1,
      s"external SPAD requires exactly one Gemmini, found ${gemminis.size}")

    val accelerator = gemminis.head.asInstanceOf[
      gemmini.Gemmini[chisel3.SInt, gemmini.Float, gemmini.Float]]
    val cgras = totalTiles.values.toSeq.flatMap {
      case tile: RocketTile => tile.roccs.collect {
        case accelerator: CGRAAccelerator => accelerator
      }
      case _ => Nil
    }
    require(cgras.size == 1,
      s"external SPAD consumer requires exactly one CGRA, found ${cgras.size}")
    require(cgras.head.consumerNode.isDefined,
      "external SPAD consumer requires the typed CGRA bridge")
    val memory = LazyModule(new GemminiExternalSpadMemory(accelerator, params))
    cgras.head.consumerNode.get := memory.consumerNode
    memory.clockNode := pbus.fixedClockNode
    memory.publicationMonitor.clockNode := pbus.fixedClockNode

    pbus.coupleTo("gemmini-external-spad-read") {
      memory.systemReadNode :=
        TLFragmenter(16, pbus.blockBytes) := TLWidthWidget(pbus) := _
    }
    pbus.coupleTo("cgra-transfer-control") {
      memory.controlNode := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _
    }
    memory
  }
}

class WithGemminiExternalSpad(params: GemminiExternalSpadParams)
    extends Config((_, _, _) => {
      case GemminiExternalSpadKey => Some(params)
    })
