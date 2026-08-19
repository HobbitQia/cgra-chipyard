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

case class GemminiExternalSpadValidationParams(
  baseAddress: BigInt = 0x60000000L,
  sizeBytes: Int = 64 * 1024,
  telemetryAddress: BigInt = 0x60010000L)

case object GemminiExternalSpadValidationKey
    extends Field[Option[GemminiExternalSpadValidationParams]](None)

/** A dedicated T1-only external-SPAD model.
  *
  * Gemmini reads and the independent CPU validation reader share the SRAM read
  * port. Gemmini's ordinary SPAD writes and STORE_SPAD writer share the SRAM
  * write port. The two directions remain separate until the 1R1W byte array.
  * A 64-byte width widget combines a full-width accumulator row into one SRAM
  * commit, while preserving its byte address and mask.
  */
class GemminiExternalSpadValidationMemory(
  gemminiAccelerator: gemmini.Gemmini[chisel3.SInt, gemmini.Float, gemmini.Float],
  params: GemminiExternalSpadValidationParams)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  private val readBeatBytes = 16
  private val writeBeatBytes = 64
  private val gemminiBeatBytes = gemminiAccelerator.config.dma_buswidth / 8
  private val address = AddressSet(params.baseAddress, params.sizeBytes - 1)

  require(isPow2(params.sizeBytes) && params.sizeBytes >= writeBeatBytes)
  require((params.baseAddress & (params.sizeBytes - 1)) == 0)
  require(params.telemetryAddress >= params.baseAddress + params.sizeBytes)
  require(gemminiBeatBytes == readBeatBytes)
  require(writeBeatBytes % gemminiBeatBytes == 0)
  require(params.sizeBytes == gemminiAccelerator.config.sp_capacity
    .asInstanceOf[gemmini.CapacityInKilobytes].kilobytes * 1024)
  require(gemminiAccelerator.config.use_shared_ext_mem)
  require(gemminiAccelerator.config.use_tl_ext_mem)
  require(!gemminiAccelerator.config.sp_singleported)
  require(gemminiAccelerator.spad.spad_writer.isDefined)

  private val memoryDevice = new SimpleDevice(
    "gemmini-ext-spad-t1",
    Seq("ucbbar,gemmini-external-spad-validation"))
  private val telemetryDevice = new SimpleDevice(
    "gemmini-ext-spad-t1-stats",
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

  val cpuReadNode = TLIdentityNode()
  val telemetryNode = TLRegisterNode(
    address = Seq(AddressSet(params.telemetryAddress, 0xfff)),
    device = telemetryDevice,
    beatBytes = 8,
    concurrency = 1)

  private val readXbar = TLXbar()
  private val writeXbar = TLXbar()

  readManager := readXbar
  readXbar := cpuReadNode
  readXbar :=* gemminiAccelerator.spad_read_nodes

  writeManager := writeXbar
  writeXbar :=* TLWidthWidget(gemminiBeatBytes) :=* TLBuffer() :=* gemminiAccelerator.spad_write_nodes
  writeXbar := TLWidthWidget(gemminiBeatBytes) := TLBuffer() := gemminiAccelerator.spad.spad_writer.get.node

  override lazy val module = new MemoryImpl
  class MemoryImpl extends Impl {
    withClockAndReset(clock, reset) {
    val (read, readEdge) = readManager.in.head
    val (write, writeEdge) = writeManager.in.head
    val lineCount = params.sizeBytes / writeBeatBytes
    val lineIndexBits = log2Ceil(lineCount)
    val mem = SyncReadMem(lineCount, Vec(writeBeatBytes, UInt(8.W)))

    val readPending = RegInit(false.B)
    val readSource = Reg(chiselTypeOf(read.a.bits.source))
    val readSize = Reg(chiselTypeOf(read.a.bits.size))
    val readBeat = Reg(UInt(log2Ceil(writeBeatBytes / readBeatBytes).W))
    val readLine = mem.read(
      read.a.bits.address(log2Ceil(writeBeatBytes) + lineIndexBits - 1, log2Ceil(writeBeatBytes)),
      read.a.fire)
    val readBeats = readLine.asUInt.asTypeOf(Vec(writeBeatBytes / readBeatBytes, UInt((readBeatBytes * 8).W)))

    read.a.ready := !readPending
    read.d.valid := readPending
    read.d.bits := readEdge.AccessAck(readSource, readSize, readBeats(readBeat))
    read.b.valid := false.B
    read.c.ready := true.B
    read.e.ready := true.B

    when (read.a.fire) {
      assert(read.a.bits.opcode === TLMessages.Get)
      readPending := true.B
      readSource := read.a.bits.source
      readSize := read.a.bits.size
      readBeat := read.a.bits.address(log2Ceil(writeBeatBytes) - 1, log2Ceil(readBeatBytes))
    }
    when (read.d.fire) {
      readPending := false.B
    }

    val writePending = RegInit(false.B)
    val writeSource = Reg(chiselTypeOf(write.a.bits.source))
    val writeSize = Reg(chiselTypeOf(write.a.bits.size))

    write.a.ready := !writePending
    write.d.valid := writePending
    write.d.bits := writeEdge.AccessAck(writeSource, writeSize)
    write.b.valid := false.B
    write.c.ready := true.B
    write.e.ready := true.B

    when (write.a.fire) {
      assert(write.a.bits.opcode === TLMessages.PutFullData ||
        write.a.bits.opcode === TLMessages.PutPartialData)
      mem.write(
        write.a.bits.address(log2Ceil(writeBeatBytes) + lineIndexBits - 1, log2Ceil(writeBeatBytes)),
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

    when (write.a.fire) {
      writeCommitCount := writeCommitCount + 1.U
      lastWriteAddress := write.a.bits.address
      lastWriteMask := write.a.bits.mask
      when (write.a.bits.size === log2Ceil(writeBeatBytes).U && write.a.bits.mask.andR) {
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

    telemetryNode.regmap(
      0x00 -> Seq(RegField.r(32, writeCommitCount)),
      0x08 -> Seq(RegField.r(32, writeAckCount)),
      0x10 -> Seq(RegField.r(32, fullLineWriteCount)),
      0x18 -> Seq(RegField.r(32, partialWriteCount)),
      0x20 -> Seq(RegField.r(64, lastWriteAddress)),
      0x28 -> Seq(RegField.r(64, lastWriteMask)),
      0x30 -> Seq(RegField.r(1, sawOutstandingWrite)),
      0x38 -> Seq(RegField.r(32, readCount)))
    }
  }
}

trait CanHaveGemminiExternalSpadValidation {
  this: BaseSubsystem with InstantiatesHierarchicalElements =>
  private val pbus = locateTLBusWrapper(PBUS)

  val gemminiExternalSpadValidation = p(GemminiExternalSpadValidationKey).map { params =>
    val gemminis = totalTiles.values.toSeq.flatMap {
      case tile: RocketTile => tile.roccs.collect {
        case accelerator: gemmini.Gemmini[_, _, _] => accelerator
      }
      case _ => Nil
    }
    require(gemminis.size == 1,
      s"T1 external-SPAD validation requires exactly one Gemmini, found ${gemminis.size}")

    val accelerator = gemminis.head.asInstanceOf[
      gemmini.Gemmini[chisel3.SInt, gemmini.Float, gemmini.Float]]
    val memory = LazyModule(new GemminiExternalSpadValidationMemory(accelerator, params))
    memory.clockNode := pbus.fixedClockNode

    pbus.coupleTo("gemmini-external-spad-validation-read") {
      memory.cpuReadNode := TLFragmenter(16, pbus.blockBytes) := TLWidthWidget(pbus) := _
    }
    pbus.coupleTo("gemmini-external-spad-validation-telemetry") {
      memory.telemetryNode := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _
    }
    memory
  }
}

class WithGemminiExternalSpadValidation(
  params: GemminiExternalSpadValidationParams = GemminiExternalSpadValidationParams())
    extends Config((_, _, _) => {
      case GemminiExternalSpadValidationKey => Some(params)
    })
