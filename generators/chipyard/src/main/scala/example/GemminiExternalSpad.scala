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
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import org.chipsalliance.diplomacy.lazymodule.LazyModule

case class GemminiExternalSpadParams(
  baseAddress: BigInt,
  sizeBytes: Int,
  spadRowBytes: Int,
  fullWidthRowStride: Int,
  outputSlotCount: Int,
  outputSlotSizeBytes: Int,
  telemetryAddress: Option[BigInt] = None,
  systemReadResponseStallCycles: Int = 0) {
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

  require(params.baseAddress == gemminiConfig.tl_ext_mem_base)
  require(params.sizeBytes == gemminiSpadBytes)
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
  val telemetryNode = params.telemetryAddress.map { telemetryAddress =>
    TLRegisterNode(
      address = Seq(AddressSet(telemetryAddress, 0xfff)),
      device = telemetryDevice,
      beatBytes = 8,
      concurrency = 1)
  }

  private val readXbar = TLXbar()
  private val writeXbar = TLXbar()

  readManager := readXbar
  readXbar := systemReadNode
  readXbar :=* gemminiAccelerator.spad_read_nodes

  writeManager := writeXbar
  writeXbar :=* TLWidthWidget(gemminiBeatBytes) :=* TLBuffer() :=*
    gemminiAccelerator.spad_write_nodes
  writeXbar := TLWidthWidget(gemminiBeatBytes) := TLBuffer() :=
    gemminiAccelerator.spad.spad_writer.get.node

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

      telemetryNode.foreach(_.regmap(
        0x00 -> Seq(RegField.r(32, writeCommitCount)),
        0x08 -> Seq(RegField.r(32, writeAckCount)),
        0x10 -> Seq(RegField.r(32, fullLineWriteCount)),
        0x18 -> Seq(RegField.r(32, partialWriteCount)),
        0x20 -> Seq(RegField.r(64, lastWriteAddress)),
        0x28 -> Seq(RegField.r(64, lastWriteMask)),
        0x30 -> Seq(RegField.r(1, sawOutstandingWrite)),
        0x38 -> Seq(RegField.r(32, readCount)),
        0x40 -> Seq(RegField.r(32, readResponseBackpressureCycleCount)),
        0x48 -> Seq(RegField.r(32, sameLineWriteWhileReadBlockedCount)),
        0x50 -> Seq(RegField.r(32, params.systemReadResponseStallCycles.U))))
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
    val memory = LazyModule(new GemminiExternalSpadMemory(accelerator, params))
    memory.clockNode := pbus.fixedClockNode

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
    memory
  }
}

class WithGemminiExternalSpad(params: GemminiExternalSpadParams)
    extends Config((_, _, _) => {
      case GemminiExternalSpadKey => Some(params)
    })
