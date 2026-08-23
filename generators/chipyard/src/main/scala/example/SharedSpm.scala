package chipyard.example

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters

case class SharedSpmParams(
  baseAddress: BigInt,
  sizeBytes: Int,
  slotCount: Int,
  slotSizeBytes: Int) {
  require(isPow2(sizeBytes))
  require((baseAddress & (sizeBytes - 1)) == 0)
  require(slotCount > 0 && slotCount * slotSizeBytes <= sizeBytes)

  val slotBase: BigInt = baseAddress + sizeBytes - slotCount * slotSizeBytes
  val slotBases: Seq[BigInt] = Seq.tabulate(slotCount)(index => slotBase + index * slotSizeBytes)
}

/** TileLink-visible shared SPM with one physical read port and one physical
  * write port. IP-specific adapters attach to the public crossbars.
  */
class SharedSpm(params: SharedSpmParams, readBeatBytes: Int, writeBeatBytes: Int)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  require(writeBeatBytes % readBeatBytes == 0)
  private val address = AddressSet(params.baseAddress, params.sizeBytes - 1)
  private val device = new SimpleDevice("shared-spm", Seq("coredac,shared-spm"))

  private val readManager = TLManagerNode(Seq(TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(address),
      resources = device.reg,
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

  val readers = TLXbar()
  val writers = TLXbar()
  readManager := readers
  writeManager := writers

  override lazy val module = new SharedSpmImpl
  class SharedSpmImpl extends Impl {
    withClockAndReset(clock, reset) {
      val (read, readEdge) = readManager.in.head
      val (write, writeEdge) = writeManager.in.head
      val lineCount = params.sizeBytes / writeBeatBytes
      val lineIndexBits = log2Ceil(lineCount)
      val lineOffsetBits = log2Ceil(writeBeatBytes)
      val readIndexBits = log2Ceil(writeBeatBytes / readBeatBytes)
      val mem = SyncReadMem(lineCount, Vec(writeBeatBytes, UInt(8.W)))

      val readPending = RegInit(false.B)
      val readSource = Reg(UInt(readEdge.bundle.sourceBits.W))
      val readSize = Reg(UInt(readEdge.bundle.sizeBits.W))
      val readBeat = Reg(UInt(math.max(1, readIndexBits).W))
      val readLine = Reg(UInt(lineIndexBits.W))
      val responseValid = RegInit(false.B)
      val responseData = Reg(UInt((readBeatBytes * 8).W))

      val writePending = RegInit(false.B)
      val writeSource = Reg(UInt(writeEdge.bundle.sourceBits.W))
      val writeSize = Reg(UInt(writeEdge.bundle.sizeBits.W))

      val incomingReadLine = read.a.bits.address(
        lineOffsetBits + lineIndexBits - 1,
        lineOffsetBits)
      val incomingWriteLine = write.a.bits.address(
        lineOffsetBits + lineIndexBits - 1,
        lineOffsetBits)
      val writeCanCommit = !writePending
      val sameLineRequest = read.a.valid && write.a.valid && incomingReadLine === incomingWriteLine
      val readData = mem.read(incomingReadLine, read.a.fire)
      val readBeats = readData.asUInt.asTypeOf(
        Vec(writeBeatBytes / readBeatBytes, UInt((readBeatBytes * 8).W)))
      val writeConflictsWithRead = readPending && incomingWriteLine === readLine

      read.a.ready := !readPending && !responseValid && !(writeCanCommit && sameLineRequest)
      read.d.valid := responseValid
      read.d.bits := readEdge.AccessAck(readSource, readSize, responseData)
      read.b.valid := false.B
      read.c.ready := true.B
      read.e.ready := true.B

      when(read.a.fire) {
        readPending := true.B
        readSource := read.a.bits.source
        readSize := read.a.bits.size
        readLine := incomingReadLine
        if (readIndexBits == 0) {
          readBeat := 0.U
        } else {
          readBeat := read.a.bits.address(lineOffsetBits - 1, log2Ceil(readBeatBytes))
        }
      }
      when(readPending) {
        readPending := false.B
        responseValid := true.B
        responseData := readBeats(readBeat)
      }
      when(read.d.fire) {
        responseValid := false.B
      }

      write.a.ready := writeCanCommit && !writeConflictsWithRead
      write.d.valid := writePending
      write.d.bits := writeEdge.AccessAck(writeSource, writeSize)
      write.b.valid := false.B
      write.c.ready := true.B
      write.e.ready := true.B

      when(write.a.fire) {
        mem.write(
          incomingWriteLine,
          write.a.bits.data.asTypeOf(Vec(writeBeatBytes, UInt(8.W))),
          write.a.bits.mask.asBools)
        writePending := true.B
        writeSource := write.a.bits.source
        writeSize := write.a.bits.size
      }
      when(write.d.fire) {
        writePending := false.B
      }
    }
  }
}
