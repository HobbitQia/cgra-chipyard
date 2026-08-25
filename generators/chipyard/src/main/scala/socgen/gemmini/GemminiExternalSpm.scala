package chipyard.socgen.gemmini

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.subsystem.{BaseSubsystem, InstantiatesHierarchicalElements, SBUS}
import freechips.rocketchip.tile.RocketTile
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import org.chipsalliance.diplomacy.lazymodule.LazyModule

case class GemminiExternalSpmParams(baseAddress: BigInt, sizeBytes: Int) {
  require(isPow2(sizeBytes))
  require((baseAddress & (sizeBytes - 1)) == 0)
}

case object GemminiExternalSpmKey extends Field[Option[GemminiExternalSpmParams]](None)

class WithGemminiExternalSpm(params: GemminiExternalSpmParams) extends Config((_, _, _) => { case GemminiExternalSpmKey => Some(params) })

case object GemminiExternalSpmWriterKey extends Field[Boolean](false)

class WithGemminiExternalSpmWriter extends Config((_, _, _) => { case GemminiExternalSpmWriterKey => true })

/** TileLink backing memory for Gemmini's external scratchpad. */
class GemminiExternalSpm(params: GemminiExternalSpmParams, bankCount: Int, readBeatBytes: Int, writeBeatBytes: Int)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  require(isPow2(bankCount))
  require(params.sizeBytes % bankCount == 0)
  require(writeBeatBytes % readBeatBytes == 0)
  private val bankBytes = params.sizeBytes / bankCount
  require(bankBytes % writeBeatBytes == 0)
  private val device = new SimpleDevice("gemmini-ext-spm", Seq("coredac,gemmini-ext-spm"))

  val readNodes = Seq.tabulate(bankCount) { bank =>
    val address = AddressSet(params.baseAddress + bank * bankBytes, bankBytes - 1)
    TLManagerNode(Seq(TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v1(
        address = Seq(address),
        resources = device.reg,
        regionType = RegionType.IDEMPOTENT,
        executable = false,
        supportsGet = TransferSizes(1, readBeatBytes),
        fifoId = Some(0))),
      beatBytes = readBeatBytes,
      minLatency = 1)))
  }

  val writeNodes = Seq.tabulate(bankCount) { bank =>
    val address = AddressSet(params.baseAddress + bank * bankBytes, bankBytes - 1)
    TLManagerNode(Seq(TLSlavePortParameters.v1(
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
  }

  override lazy val module = new MemoryImpl
  class MemoryImpl extends Impl {
    withClockAndReset(clock, reset) {
      (readNodes zip writeNodes).foreach { case (readNode, writeNode) =>
        val (read, readEdge) = readNode.in.head
        val (write, writeEdge) = writeNode.in.head
        val lineCount = bankBytes / writeBeatBytes
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
        val sameLineRequest = read.a.valid && write.a.valid &&
          incomingReadLine === incomingWriteLine
        val readData = mem.read(incomingReadLine, read.a.fire)
        val readBeats = readData.asUInt.asTypeOf(
          Vec(writeBeatBytes / readBeatBytes, UInt((readBeatBytes * 8).W)))
        val writeConflictsWithRead = readPending && incomingWriteLine === readLine

        read.a.ready := !readPending && !responseValid &&
          !(write.a.valid && !writePending && sameLineRequest)
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
            readBeat := read.a.bits.address(
              lineOffsetBits - 1,
              log2Ceil(readBeatBytes))
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

        write.a.ready := !writePending && !writeConflictsWithRead
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
}

/** Connects Gemmini and system-side readers to one external scratchpad. */
class GemminiExternalSpmAttach(val gemminiAccelerator: gemmini.Gemmini[chisel3.SInt, gemmini.Float, gemmini.Float], params: GemminiExternalSpmParams)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  private val gemminiConfig = gemminiAccelerator.config
  val readBeatBytes: Int = gemminiConfig.sp_width / 8
  val writeBeatBytes: Int =
    gemminiConfig.meshColumns * gemminiConfig.tileColumns * gemminiConfig.accType.getWidth / 8
  private val spmBytes =
    gemminiConfig.sp_banks * gemminiConfig.sp_bank_entries * readBeatBytes

  require(spmBytes == params.sizeBytes)

  val spm = LazyModule(new GemminiExternalSpm(params, gemminiConfig.sp_banks, readBeatBytes, writeBeatBytes))
  val readPorts = TLXbar()
  val writePorts = TLXbar()
  val writerNode = TLIdentityNode()

  spm.readNodes.foreach { node => node := readPorts }
  spm.writeNodes.foreach { node => node := writePorts }
  readPorts :=* gemminiAccelerator.spad_read_nodes
  writePorts :=* TLWidthWidget(readBeatBytes) :=* TLBuffer() :=*
    gemminiAccelerator.spad_write_nodes
  writePorts := writerNode

  override lazy val module = new AttachImpl
  class AttachImpl extends Impl
}

class GemminiExternalSpmWriter(gemminiAccelerator: gemmini.Gemmini[chisel3.SInt, gemmini.Float, gemmini.Float], readBeatBytes: Int)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  val node = TLIdentityNode()

  node := TLWidthWidget(readBeatBytes) := TLBuffer() :=
    gemminiAccelerator.spad.spad_writer.get.node

  override lazy val module = new WriterImpl
  class WriterImpl extends Impl
}

trait CanHaveGemminiExternalSpm {
  this: BaseSubsystem with InstantiatesHierarchicalElements =>
  private val sbus = locateTLBusWrapper(SBUS)

  val gemminiExternalSpm = p(GemminiExternalSpmKey).map { params =>
    val gemminis = totalTiles.values.toSeq.flatMap {
      case tile: RocketTile =>
        tile.roccs.collect { case accelerator: gemmini.Gemmini[_, _, _] => accelerator }
      case _ => Nil
    }
    require(gemminis.size == 1)
    val gemminiAccelerator = gemminis.head.asInstanceOf[
      gemmini.Gemmini[chisel3.SInt, gemmini.Float, gemmini.Float]]
    val attach = LazyModule(new GemminiExternalSpmAttach(gemminiAccelerator, params))

    attach.clockNode := sbus.fixedClockNode
    attach.spm.clockNode := sbus.fixedClockNode
    sbus.coupleTo("gemmini-ext-spm") {
      attach.readPorts := TLFIFOFixer() := TLFragmenter(
        attach.readBeatBytes,
        sbus.blockBytes) := TLWidthWidget(sbus) := _
    }
    attach
  }
}

trait CanHaveGemminiExternalSpmWriter {
  this: BaseSubsystem with InstantiatesHierarchicalElements with CanHaveGemminiExternalSpm =>
  private val sbus = locateTLBusWrapper(SBUS)

  val gemminiExternalSpmWriter = Option.when(p(GemminiExternalSpmWriterKey)) {
    val attach = gemminiExternalSpm.get
    val writer = LazyModule(new GemminiExternalSpmWriter(attach.gemminiAccelerator, attach.readBeatBytes))

    attach.writerNode := writer.node
    writer.clockNode := sbus.fixedClockNode
    writer
  }
}
