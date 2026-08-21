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
  outputSlotSizeBytes: Int,
  telemetryAddress: Option[BigInt] = None,
  systemReadResponseStallCycles: Int = 0,
  publicationResponseStallFinalAck: Boolean = false,
  publicationResponseStallCycles: Int = 0) {
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

/** Validation-only adapter which backpressures the first system-read D beat.
  *
  * This creates a deterministic TileLink stability regression without
  * changing the production path. Requests and all non-D channels pass
  * through unchanged.
  */
class GemminiExternalSpadReadResponseStaller(stallCycles: Int)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  require(stallCycles > 0)

  val node = TLAdapterNode()

  override lazy val module = new StallerImpl
  class StallerImpl extends Impl {
    withClockAndReset(clock, reset) {
      (node.in zip node.out).foreach { case ((in, _), (out, _)) =>
        out.a <> in.a
        in.b <> out.b
        out.c <> in.c
        out.e <> in.e

        val holdFirstResponse = RegInit(true.B)
        val remaining = RegInit(0.U(log2Ceil(stallCycles + 1).W))
        val startHold = holdFirstResponse && out.d.valid
        val holdResponse = startHold || remaining =/= 0.U

        in.d.valid := out.d.valid && !holdResponse
        in.d.bits := out.d.bits
        out.d.ready := in.d.ready && !holdResponse

        when (startHold) {
          holdFirstResponse := false.B
          remaining := (stallCycles - 1).U
        }.elsewhen(remaining =/= 0.U) {
          remaining := remaining - 1.U
        }
      }
    }
  }
}

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
  require(params.telemetryAddress.forall { telemetryAddress =>
    telemetryAddress >= params.baseAddress + params.sizeBytes &&
      (telemetryAddress & 0xfff) == 0
  })
  require(params.systemReadResponseStallCycles >= 0)
  require(params.systemReadResponseStallCycles == 0 || params.telemetryAddress.isDefined)
  require(params.publicationResponseStallCycles >= 0)
  require(params.publicationResponseStallFinalAck ==
    (params.publicationResponseStallCycles != 0))
  require(params.publicationResponseStallCycles == 0 ||
    params.telemetryAddress.isDefined)

  require(params.baseAddress == gemminiConfig.tl_ext_mem_base)
  require(params.sizeBytes == gemminiSpadBytes)
  require(params.controlAddress ==
    GemminiExternalSpadGenerated.productionControlAddress)
  require(params.controlAddress >= params.baseAddress + params.sizeBytes)
  require(params.telemetryAddress.forall(_ != params.controlAddress))
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
  private val telemetryDevice = new SimpleDevice(
    "gemmini-external-spad-validation-stats",
    Seq("ucbbar,gemmini-external-spad-validation-telemetry"))
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
  val telemetryNode = params.telemetryAddress.map { telemetryAddress =>
    TLRegisterNode(
      address = Seq(AddressSet(telemetryAddress, 0xfff)),
      device = telemetryDevice,
      beatBytes = 8,
      concurrency = 1)
  }
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
    producerAdapterParams,
    params.publicationResponseStallFinalAck,
    params.publicationResponseStallCycles))

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
      publicationMonitor.module.io.stallResponse :=
        producerAdapter.io.finalRowOutstanding

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

      val writeCommitCount = RegInit(0.U(32.W))
      val writeAckCount = RegInit(0.U(32.W))
      val fullLineWriteCount = RegInit(0.U(32.W))
      val partialWriteCount = RegInit(0.U(32.W))
      val lastWriteAddress = RegInit(0.U(64.W))
      val lastWriteMask = RegInit(0.U(64.W))
      val sawOutstandingWrite = RegInit(false.B)
      val readCount = RegInit(0.U(32.W))
      val readResponseBackpressureCycleCount = RegInit(0.U(32.W))
      val sameLineWriteWhileReadBlockedCount = RegInit(0.U(32.W))

      when (write.a.fire) {
        writeCommitCount := writeCommitCount + 1.U
        lastWriteAddress := write.a.bits.address
        lastWriteMask := write.a.bits.mask
        when (write.a.bits.size === log2Ceil(writeBeatBytes).U &&
          write.a.bits.mask.andR) {
          fullLineWriteCount := fullLineWriteCount + 1.U
        }.otherwise {
          partialWriteCount := partialWriteCount + 1.U
        }
      }
      when (write.d.fire) {
        writeAckCount := writeAckCount + 1.U
      }
      when (writePending) {
        sawOutstandingWrite := true.B
      }
      when (read.a.fire) {
        readCount := readCount + 1.U
      }
      when (readResponseBlocked) {
        readResponseBackpressureCycleCount :=
          readResponseBackpressureCycleCount + 1.U
      }
      when (readResponseBlocked && write.a.fire &&
        writeLineIndex === readResponseLineIndex) {
        sameLineWriteWhileReadBlockedCount :=
          sameLineWriteWhileReadBlockedCount + 1.U
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

      telemetryNode match {
        case Some(node) =>
          // The following injection and observation controls exist only in the
          // validation configuration. Production correctness consumes typed
          // endpoint events and the dedicated writer monitor directly.
          val validationRequestJobId = RegInit(0.U(32.W))
          val validationRequestSlot = RegInit(0.U(32.W))
          val validationRequestMaxBytes = RegInit(0.U(32.W))
          val validationRequestSubmit = Wire(Decoupled(UInt(1.W)))
          val validationRequestQueue = Module(new Queue(
            new SpmTransferRequest, 1))
          validationRequestQueue.io.enq.valid :=
            validationRequestSubmit.valid &&
              validationRequestSubmit.bits.asBool
          validationRequestQueue.io.enq.bits.jobId := validationRequestJobId
          validationRequestQueue.io.enq.bits.slot := validationRequestSlot
          validationRequestQueue.io.enq.bits.maxBytes :=
            validationRequestMaxBytes
          validationRequestSubmit.ready := validationRequestQueue.io.enq.ready

          val validationReadyAccept = RegInit(false.B)
          val validationConsumerEnable = RegInit(false.B)

          transferEndpoint.io.requestIn.valid := Mux(
            validationConsumerEnable,
            consumerAdapter.io.requestOut.valid,
            validationRequestQueue.io.deq.valid)
          transferEndpoint.io.requestIn.bits := Mux(
            validationConsumerEnable,
            consumerAdapter.io.requestOut.bits,
            validationRequestQueue.io.deq.bits)
          consumerAdapter.io.requestOut.ready :=
            transferEndpoint.io.requestIn.ready && validationConsumerEnable
          validationRequestQueue.io.deq.ready :=
            transferEndpoint.io.requestIn.ready && !validationConsumerEnable

          consumerAdapter.io.readyIn.valid :=
            transferEndpoint.io.readyOut.valid && validationConsumerEnable
          consumerAdapter.io.readyIn.bits := transferEndpoint.io.readyOut.bits
          transferEndpoint.io.readyOut.ready := Mux(
            validationConsumerEnable,
            consumerAdapter.io.readyIn.ready,
            validationReadyAccept)

          transferEndpoint.io.readStartIn.valid :=
            consumerAdapter.io.readStartOut.valid && validationConsumerEnable
          transferEndpoint.io.readStartIn.bits :=
            consumerAdapter.io.readStartOut.bits
          consumerAdapter.io.readStartOut.ready :=
            transferEndpoint.io.readStartIn.ready && validationConsumerEnable
          transferEndpoint.io.releaseIn.valid :=
            consumerAdapter.io.releaseOut.valid && validationConsumerEnable
          transferEndpoint.io.releaseIn.bits := consumerAdapter.io.releaseOut.bits
          consumerAdapter.io.releaseOut.ready :=
            transferEndpoint.io.releaseIn.ready && validationConsumerEnable

          val validationPullJobId = RegInit(0.U(32.W))
          val validationPullSlot = RegInit(0.U(32.W))
          val validationPullBytes = RegInit(0.U(32.W))
          val validationPullSpmWordAddress = RegInit(0.U(32.W))
          val validationPullDmaTag = RegInit(0.U(32.W))
          val validationPullSubmit = Wire(Decoupled(UInt(1.W)))
          val validationPullQueue = Module(new Queue(
            new CgraConsumerPullDescriptor(
              CgraConsumerPullAdapterParams.production), 1))
          validationPullQueue.io.enq.valid :=
            validationPullSubmit.valid &&
              validationPullSubmit.bits.asBool && validationConsumerEnable
          validationPullQueue.io.enq.bits.jobId := validationPullJobId
          validationPullQueue.io.enq.bits.slot := validationPullSlot
          validationPullQueue.io.enq.bits.bytes := validationPullBytes
          validationPullQueue.io.enq.bits.spmWordAddress :=
            validationPullSpmWordAddress
          validationPullQueue.io.enq.bits.dmaTag := validationPullDmaTag
          validationPullSubmit.ready :=
            validationPullQueue.io.enq.ready && validationConsumerEnable

          val validationCompletionAccept = RegInit(false.B)
          val validationLaunchGateEnable = RegInit(false.B)
          val validationLaunchJobId = RegInit(0.U(32.W))
          val validationLaunchSlot = RegInit(0.U(32.W))
          val validationLaunchBytes = RegInit(0.U(32.W))
          val validationLaunchSpmWordAddress = RegInit(0.U(32.W))
          val validationLaunchDmaTag = RegInit(0.U(32.W))
          val validationLaunchPacketCount = RegInit(0.U(32.W))
          val validationLaunchSubmit = Wire(Decoupled(UInt(1.W)))
          val validationLaunchHeaderQueue = Module(new Queue(
            new CgraLaunchSequenceHeader(launchParams), 1))
          validationLaunchHeaderQueue.io.enq.valid :=
            validationLaunchSubmit.valid &&
              validationLaunchSubmit.bits.asBool && validationLaunchGateEnable
          validationLaunchHeaderQueue.io.enq.bits.jobId :=
            validationLaunchJobId
          validationLaunchHeaderQueue.io.enq.bits.slot := validationLaunchSlot
          validationLaunchHeaderQueue.io.enq.bits.bytes :=
            validationLaunchBytes
          validationLaunchHeaderQueue.io.enq.bits.spmWordAddress :=
            validationLaunchSpmWordAddress
          validationLaunchHeaderQueue.io.enq.bits.dmaTag :=
            validationLaunchDmaTag
          validationLaunchHeaderQueue.io.enq.bits.packetCount :=
            validationLaunchPacketCount
          validationLaunchSubmit.ready :=
            validationLaunchHeaderQueue.io.enq.ready &&
              validationLaunchGateEnable

          val controlSourceSelector = Module(
            new CgraTransferControlSourceSelector(launchParams))
          controlSourceSelector.io.productionDescriptorIn <>
            controlQueues.io.descriptorOut
          controlSourceSelector.io.validationDescriptorIn <>
            validationPullQueue.io.deq
          consumerAdapter.io.descriptorIn <>
            controlSourceSelector.io.descriptorOut
          controlSourceSelector.io.productionLaunchHeaderIn <>
            controlQueues.io.launchHeaderOut
          controlSourceSelector.io.validationLaunchHeaderIn <>
            validationLaunchHeaderQueue.io.deq
          launchHeaderToCgra <> controlSourceSelector.io.launchHeaderOut
          controlSourceSelector.io.completionIn <>
            consumerAdapter.io.completionOut

          completionToCgra.valid :=
            controlSourceSelector.io.productionCompletionOut.valid ||
              (controlSourceSelector.io.validationCompletionOut.valid &&
                validationCompletionAccept && validationLaunchGateEnable)
          completionToCgra.bits := Mux(
            controlSourceSelector.io.productionCompletionOut.valid,
            controlSourceSelector.io.productionCompletionOut.bits,
            controlSourceSelector.io.validationCompletionOut.bits)
          controlSourceSelector.io.productionCompletionOut.ready :=
            completionToCgra.ready
          controlSourceSelector.io.validationCompletionOut.ready :=
            validationCompletionAccept && Mux(
              validationLaunchGateEnable, completionToCgra.ready, true.B)
          val consumerCompletionCount = RegInit(0.U(32.W))
          val lastConsumerCompletion = Reg(
            new CgraConsumerCompletion(
              CgraConsumerPullAdapterParams.production))
          when(controlSourceSelector.io.completionIn.fire) {
            consumerCompletionCount := consumerCompletionCount + 1.U
            lastConsumerCompletion := controlSourceSelector.io.completionIn.bits
          }

          val validationLaunchPacketLo = RegInit(0.U(64.W))
          val validationLaunchPacketMid = RegInit(0.U(64.W))
          val validationLaunchPacketHi = RegInit(0.U(64.W))
          val validationLaunchPacketTop = RegInit(0.U(64.W))
          val validationLaunchPacketSubmit = Wire(Decoupled(UInt(1.W)))
          val validationLaunchPacketQueue = Module(new Queue(
            new CgraLaunchPacket(launchParams), 1))
          val validationLaunchPacketBits = Cat(
            validationLaunchPacketTop,
            validationLaunchPacketHi,
            validationLaunchPacketMid,
            validationLaunchPacketLo)
          validationLaunchPacketQueue.io.enq.valid :=
            validationLaunchPacketSubmit.valid &&
              validationLaunchPacketSubmit.bits.asBool &&
              validationLaunchGateEnable
          validationLaunchPacketQueue.io.enq.bits.packet :=
            validationLaunchPacketBits(launchParams.packetWidth - 1, 0)
          validationLaunchPacketSubmit.ready :=
            validationLaunchPacketQueue.io.enq.ready &&
              validationLaunchGateEnable
          launchPacketToCgra.valid := Mux(
            controlSourceSelector.io.productionSelected,
            controlQueues.io.launchPacketOut.valid,
            validationLaunchPacketQueue.io.deq.valid)
          launchPacketToCgra.bits := Mux(
            controlSourceSelector.io.productionSelected,
            controlQueues.io.launchPacketOut.bits,
            validationLaunchPacketQueue.io.deq.bits)
          controlQueues.io.launchPacketOut.ready :=
            launchPacketToCgra.ready &&
              controlSourceSelector.io.productionSelected
          validationLaunchPacketQueue.io.deq.ready :=
            launchPacketToCgra.ready &&
              !controlSourceSelector.io.productionSelected

          val validationLaunchResultAccept = RegInit(false.B)
          controlQueues.io.launchResultIn.valid :=
            launchResultFromCgra.valid &&
              controlSourceSelector.io.productionSelected
          controlQueues.io.launchResultIn.bits := launchResultFromCgra.bits
          launchResultFromCgra.ready := Mux(
            controlSourceSelector.io.productionSelected,
            controlQueues.io.launchResultIn.ready,
            validationLaunchResultAccept)
          val launchResultCount = RegInit(0.U(32.W))
          val launchAcceptedPacketCount = RegInit(0.U(32.W))
          val completionCountAtLaunchResult = RegInit(0.U(32.W))
          val lastLaunchResult = Reg(new CgraLaunchResult(launchParams))
          when(launchResultFromCgra.fire) {
            launchResultCount := launchResultCount + 1.U
            lastLaunchResult := launchResultFromCgra.bits
            completionCountAtLaunchResult := consumerCompletionCount
            when(launchResultFromCgra.bits.status ===
              CgraLaunchStatus.LaunchAccepted) {
              launchAcceptedPacketCount := launchAcceptedPacketCount +
                launchResultFromCgra.bits.packetCount
              assert(consumerCompletionCount =/= 0.U)
            }
          }

          val validationLaunchErrorAccept = RegInit(true.B)
          controlQueues.io.launchErrorIn.valid :=
            launchErrorFromCgra.valid &&
              controlSourceSelector.io.productionSelected
          controlQueues.io.launchErrorIn.bits := launchErrorFromCgra.bits
          launchErrorFromCgra.ready := Mux(
            controlSourceSelector.io.productionSelected,
            controlQueues.io.launchErrorIn.ready,
            validationLaunchErrorAccept)
          val launchErrorCount = RegInit(0.U(32.W))
          val lastLaunchError = Reg(
            new CgraLaunchProtocolError(launchParams))
          when(launchErrorFromCgra.fire) {
            launchErrorCount := launchErrorCount + 1.U
            lastLaunchError := launchErrorFromCgra.bits
          }
          controlQueues.io.computeCompletionIn.valid :=
            computeCompletionFromCgra.valid &&
              controlSourceSelector.io.productionSelected
          controlQueues.io.computeCompletionIn.bits :=
            computeCompletionFromCgra.bits
          computeCompletionFromCgra.ready := Mux(
            controlSourceSelector.io.productionSelected,
            controlQueues.io.computeCompletionIn.ready,
            true.B)
          controlQueues.io.computeErrorIn.valid :=
            computeErrorFromCgra.valid &&
              controlSourceSelector.io.productionSelected
          controlQueues.io.computeErrorIn.bits := computeErrorFromCgra.bits
          computeErrorFromCgra.ready := Mux(
            controlSourceSelector.io.productionSelected,
            controlQueues.io.computeErrorIn.ready,
            true.B)
          val consumerErrorCount = RegInit(0.U(32.W))
          val lastConsumerErrorReason = RegInit(0.U(32.W))
          consumerAdapter.io.errorOut.ready := true.B
          when(consumerAdapter.io.errorOut.fire) {
            consumerErrorCount := consumerErrorCount + 1.U
            lastConsumerErrorReason := consumerAdapter.io.errorOut.bits.reason
          }

          // Validation-only causal telemetry. The producer count advances
          // only after its final writer-D-derived READY is accepted by T3.
          // A consumer DMA command without a preceding such event is sticky
          // evidence of an early issue; software need not race a fixed D
          // stall window to establish the ordering.
          val successfulProducerReadyCount = RegInit(0.U(32.W))
          val consumerEarlyDmaIssueCount = RegInit(0.U(32.W))
          when(producerAdapter.io.readyOut.fire &&
            producerAdapter.io.readyOut.bits.status ===
              SpmTransferProtocol.ProducerStatus.Success) {
            successfulProducerReadyCount := successfulProducerReadyCount + 1.U
          }
          when(consumerAdapter.io.dmaCommandOut.fire &&
            consumerAdapter.io.dmaCommandCount >=
              successfulProducerReadyCount) {
            consumerEarlyDmaIssueCount := consumerEarlyDmaIssueCount + 1.U
          }

          val readyDeliveryCount = RegInit(0.U(32.W))
          val lastReadyJobId = RegInit(0.U(32.W))
          val lastReadySlot = RegInit(0.U(32.W))
          val lastReadyActualBytes = RegInit(0.U(32.W))
          val lastReadyStatus = RegInit(0.U(32.W))
          when(transferEndpoint.io.readyOut.fire) {
            readyDeliveryCount := readyDeliveryCount + 1.U
            lastReadyJobId := transferEndpoint.io.readyOut.bits.jobId
            lastReadySlot := transferEndpoint.io.readyOut.bits.slot
            lastReadyActualBytes :=
              transferEndpoint.io.readyOut.bits.actualBytes
            lastReadyStatus := transferEndpoint.io.readyOut.bits.status
          }

          val protocolErrorCount = RegInit(0.U(32.W))
          val lastProtocolErrorReason = RegInit(0.U(32.W))
          when(transferEndpoint.io.errorOut.fire) {
            protocolErrorCount := protocolErrorCount + 1.U
            lastProtocolErrorReason :=
              transferEndpoint.io.errorOut.bits.reason
          }

          node.regmap(
            0x00 -> Seq(RegField.r(32, writeCommitCount)),
            0x08 -> Seq(RegField.r(32, writeAckCount)),
            0x10 -> Seq(RegField.r(32, fullLineWriteCount)),
            0x18 -> Seq(RegField.r(32, partialWriteCount)),
            0x20 -> Seq(RegField.r(64, lastWriteAddress)),
            0x28 -> Seq(RegField.r(64, lastWriteMask)),
            0x30 -> Seq(RegField.r(1, sawOutstandingWrite)),
            0x38 -> Seq(RegField.r(32, readCount)),
            0x40 -> Seq(RegField.r(32,
              readResponseBackpressureCycleCount)),
            0x48 -> Seq(RegField.r(32,
              sameLineWriteWhileReadBlockedCount)),
            0x50 -> Seq(RegField.r(32,
              params.systemReadResponseStallCycles.U)),
            0x100 -> Seq(RegField(32, validationRequestJobId)),
            0x108 -> Seq(RegField(32, validationRequestSlot)),
            0x110 -> Seq(RegField(32, validationRequestMaxBytes)),
            0x118 -> Seq(RegField.w(1, validationRequestSubmit)),
            0x120 -> Seq(RegField(1, validationReadyAccept)),
            0x128 -> Seq(RegField.r(1,
              transferEndpoint.io.readyOut.valid)),
            0x130 -> Seq(RegField.r(32,
              transferEndpoint.io.readyOut.bits.jobId)),
            0x138 -> Seq(RegField.r(32,
              transferEndpoint.io.readyOut.bits.slot)),
            0x140 -> Seq(RegField.r(32,
              transferEndpoint.io.readyOut.bits.actualBytes)),
            0x148 -> Seq(RegField.r(32,
              transferEndpoint.io.readyOut.bits.status)),
            0x150 -> Seq(RegField.r(32, readyDeliveryCount)),
            0x158 -> Seq(RegField.r(32, lastReadyJobId)),
            0x160 -> Seq(RegField.r(32, lastReadySlot)),
            0x168 -> Seq(RegField.r(32, lastReadyActualBytes)),
            0x170 -> Seq(RegField.r(32, lastReadyStatus)),
            0x178 -> Seq(RegField.r(1, producerAdapter.io.active)),
            0x180 -> Seq(RegField.r(32,
              producerAdapter.io.issuedBytes)),
            0x188 -> Seq(RegField.r(32,
              producerAdapter.io.acknowledgedBytes)),
            0x190 -> Seq(RegField.r(1,
              producerAdapter.io.rowOutstanding)),
            0x198 -> Seq(RegField.r(32,
              publicationMonitor.module.io.aFireCount)),
            0x1a0 -> Seq(RegField.r(32,
              publicationMonitor.module.io.dFireCount)),
            0x1a8 -> Seq(RegField.r(1,
              publicationMonitor.module.io.dBlocked)),
            0x1b0 -> Seq(RegField.r(32,
              publicationMonitor.module.io.dBlockedCycleCount)),
            0x1b8 -> Seq(RegField.r(64,
              publicationMonitor.module.io.lastAAddress)),
            0x1c0 -> Seq(RegField.r(32, protocolErrorCount)),
            0x1c8 -> Seq(RegField.r(32, lastProtocolErrorReason)),
            0x1d0 -> Seq(RegField.r(32,
              params.matrixDimension.U)),
            0x1d8 -> Seq(RegField.r(32,
              params.publicationResponseStallCycles.U)),
            0x1e0 -> Seq(RegField.r(1,
              params.publicationResponseStallFinalAck.B)),
            0x200 -> Seq(RegField(1, validationConsumerEnable)),
            0x208 -> Seq(RegField(32, validationPullJobId)),
            0x210 -> Seq(RegField(32, validationPullSlot)),
            0x218 -> Seq(RegField(32, validationPullBytes)),
            0x220 -> Seq(RegField(32, validationPullSpmWordAddress)),
            0x228 -> Seq(RegField(32, validationPullDmaTag)),
            0x230 -> Seq(RegField.w(1, validationPullSubmit)),
            0x238 -> Seq(RegField(1, validationCompletionAccept)),
            0x240 -> Seq(RegField.r(1,
              consumerAdapter.io.completionOut.valid)),
            0x248 -> Seq(RegField.r(32,
              consumerAdapter.io.completionOut.bits.jobId)),
            0x250 -> Seq(RegField.r(32,
              consumerAdapter.io.completionOut.bits.slot)),
            0x258 -> Seq(RegField.r(32,
              consumerAdapter.io.completionOut.bits.actualBytes)),
            0x260 -> Seq(RegField.r(32,
              consumerAdapter.io.completionOut.bits.dmaTag)),
            0x268 -> Seq(RegField.r(32,
              consumerAdapter.io.completionOut.bits.consumerStatus)),
            0x270 -> Seq(RegField.r(32,
              consumerAdapter.io.completionOut.bits.producerStatus)),
            0x278 -> Seq(RegField.r(32, consumerCompletionCount)),
            0x280 -> Seq(RegField.r(32, lastConsumerCompletion.jobId)),
            0x288 -> Seq(RegField.r(32, lastConsumerCompletion.slot)),
            0x290 -> Seq(RegField.r(32, lastConsumerCompletion.actualBytes)),
            0x298 -> Seq(RegField.r(32, lastConsumerCompletion.dmaTag)),
            0x2a0 -> Seq(RegField.r(32,
              lastConsumerCompletion.consumerStatus)),
            0x2a8 -> Seq(RegField.r(32,
              lastConsumerCompletion.producerStatus)),
            0x2b0 -> Seq(RegField.r(1, consumerAdapter.io.active)),
            0x2b8 -> Seq(RegField.r(1, consumerAdapter.io.dmaIssued)),
            0x2c0 -> Seq(RegField.r(1,
              consumerAdapter.io.readStartDelivered)),
            0x2c8 -> Seq(RegField.r(1,
              consumerAdapter.io.dmaDoneSeen)),
            0x2d0 -> Seq(RegField.r(32, consumerErrorCount)),
            0x2d8 -> Seq(RegField.r(32, lastConsumerErrorReason)),
            0x2e0 -> Seq(RegField.r(32, consumerAdapter.io.requestCount)),
            0x2e8 -> Seq(RegField.r(32, consumerAdapter.io.dmaCommandCount)),
            0x2f0 -> Seq(RegField.r(32, consumerAdapter.io.readStartCount)),
            0x2f8 -> Seq(RegField.r(32, consumerAdapter.io.dmaDoneCount)),
            0x300 -> Seq(RegField.r(32, consumerAdapter.io.releaseCount)),
            0x308 -> Seq(RegField.r(32,
              transferEndpoint.io.slots(0).state.pad(32))),
            0x310 -> Seq(RegField.r(32,
              transferEndpoint.io.slots(1).state.pad(32))),
            0x318 -> Seq(RegField.r(32, successfulProducerReadyCount)),
            0x320 -> Seq(RegField.r(32, consumerEarlyDmaIssueCount)),
            0x328 -> Seq(RegField.r(32,
              consumerAdapter.io.completionOut.bits.spmWordAddress)),
            0x330 -> Seq(RegField.r(32,
              lastConsumerCompletion.spmWordAddress)),
            0x338 -> Seq(RegField.r(32,
              consumerAdapter.io.completionOut.bits.requestedBytes)),
            0x340 -> Seq(RegField.r(32,
              lastConsumerCompletion.requestedBytes)),
            0x400 -> Seq(RegField(1, validationLaunchGateEnable)),
            0x408 -> Seq(RegField(32, validationLaunchJobId)),
            0x410 -> Seq(RegField(32, validationLaunchSlot)),
            0x418 -> Seq(RegField(32, validationLaunchBytes)),
            0x420 -> Seq(RegField(32,
              validationLaunchSpmWordAddress)),
            0x428 -> Seq(RegField(32, validationLaunchDmaTag)),
            0x430 -> Seq(RegField(32, validationLaunchPacketCount)),
            0x438 -> Seq(RegField.w(1, validationLaunchSubmit)),
            0x440 -> Seq(RegField(64, validationLaunchPacketLo)),
            0x448 -> Seq(RegField(64, validationLaunchPacketMid)),
            0x450 -> Seq(RegField(64, validationLaunchPacketHi)),
            0x458 -> Seq(RegField(64, validationLaunchPacketTop)),
            0x460 -> Seq(RegField.w(1, validationLaunchPacketSubmit)),
            0x468 -> Seq(RegField(1, validationLaunchResultAccept)),
            0x470 -> Seq(RegField.r(1, launchResultFromCgra.valid)),
            0x478 -> Seq(RegField.r(32, launchResultCount)),
            0x480 -> Seq(RegField.r(32, lastLaunchResult.jobId)),
            0x488 -> Seq(RegField.r(32, lastLaunchResult.slot)),
            0x490 -> Seq(RegField.r(32, lastLaunchResult.actualBytes)),
            0x498 -> Seq(RegField.r(32,
              lastLaunchResult.spmWordAddress)),
            0x4a0 -> Seq(RegField.r(32, lastLaunchResult.dmaTag)),
            0x4a8 -> Seq(RegField.r(32, lastLaunchResult.packetCount)),
            0x4b0 -> Seq(RegField.r(32, lastLaunchResult.status)),
            0x4b8 -> Seq(RegField.r(32, launchAcceptedPacketCount)),
            0x4c0 -> Seq(RegField.r(32,
              completionCountAtLaunchResult)),
            0x4c8 -> Seq(RegField(1, validationLaunchErrorAccept)),
            0x4d0 -> Seq(RegField.r(1, launchErrorFromCgra.valid)),
            0x4d8 -> Seq(RegField.r(32, launchErrorCount)),
            0x4e0 -> Seq(RegField.r(32, lastLaunchError.operation)),
            0x4e8 -> Seq(RegField.r(32, lastLaunchError.reason)),
            0x4f0 -> Seq(RegField.r(32,
              lastLaunchResult.requestedBytes)),
            0x4f8 -> Seq(RegField.r(32,
              lastLaunchError.requestedBytes)))

        case None =>
          transferEndpoint.io.requestIn <> consumerAdapter.io.requestOut
          consumerAdapter.io.readyIn <> transferEndpoint.io.readyOut
          transferEndpoint.io.readStartIn <> consumerAdapter.io.readStartOut
          transferEndpoint.io.releaseIn <> consumerAdapter.io.releaseOut
          consumerAdapter.io.descriptorIn <>
            controlQueues.io.descriptorOut
          completionToCgra <> consumerAdapter.io.completionOut
          launchHeaderToCgra <> controlQueues.io.launchHeaderOut
          launchPacketToCgra <> controlQueues.io.launchPacketOut
          controlQueues.io.launchResultIn <> launchResultFromCgra
          controlQueues.io.launchErrorIn <> launchErrorFromCgra
          controlQueues.io.computeCompletionIn <>
            computeCompletionFromCgra
          controlQueues.io.computeErrorIn <> computeErrorFromCgra
          consumerAdapter.io.errorOut.ready := true.B
      }
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

    if (params.systemReadResponseStallCycles > 0) {
      val staller = LazyModule(new GemminiExternalSpadReadResponseStaller(
        params.systemReadResponseStallCycles))
      staller.clockNode := pbus.fixedClockNode
      pbus.coupleTo("gemmini-external-spad-read") {
        memory.systemReadNode := staller.node :=
          TLFragmenter(16, pbus.blockBytes) := TLWidthWidget(pbus) := _
      }
    } else {
      pbus.coupleTo("gemmini-external-spad-read") {
        memory.systemReadNode :=
          TLFragmenter(16, pbus.blockBytes) := TLWidthWidget(pbus) := _
      }
    }
    memory.telemetryNode.foreach { telemetryNode =>
      pbus.coupleTo("gemmini-external-spad-validation-telemetry") {
        telemetryNode := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _
      }
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
