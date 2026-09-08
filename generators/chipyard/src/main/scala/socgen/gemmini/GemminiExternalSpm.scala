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
class GemminiExternalSpm(
  params: GemminiExternalSpmParams,
  bankCount: Int,
  readBeatBytes: Int,
  writeBeatBytes: Int,
  systemMaxBytes: Int)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  require(isPow2(bankCount))
  require(params.sizeBytes % bankCount == 0)
  require(writeBeatBytes % readBeatBytes == 0)
  require(writeBeatBytes >= systemMaxBytes)
  require(writeBeatBytes % systemMaxBytes == 0)
  private val bankBytes = params.sizeBytes / bankCount
  require(bankBytes % writeBeatBytes == 0)
  private val device = new SimpleDevice("gemmini-ext-spm", Seq("coredac,gemmini-ext-spm"))

  val readNodes = Seq.tabulate(bankCount) { bank =>
    val address = AddressSet(params.baseAddress + bank * bankBytes, bankBytes - 1)
    TLManagerNode(Seq(TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v1(
        address = Seq(address),
        resources = Nil,
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

  val systemNodes = Seq.tabulate(bankCount) { bank =>
    val address = AddressSet(params.baseAddress + bank * bankBytes, bankBytes - 1)
    TLManagerNode(Seq(TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v1(
        address = Seq(address),
        resources = device.reg,
        regionType = RegionType.IDEMPOTENT,
        executable = false,
        supportsGet = TransferSizes(1, systemMaxBytes),
        supportsPutFull = TransferSizes(1, systemMaxBytes),
        supportsPutPartial = TransferSizes(1, systemMaxBytes),
        fifoId = Some(0))),
      beatBytes = systemMaxBytes,
      minLatency = 1)))
  }

  override lazy val module = new MemoryImpl
  class MemoryImpl extends Impl {
    withClockAndReset(clock, reset) {
      (readNodes zip writeNodes zip systemNodes).foreach { case ((readNode, writeNode), systemNode) =>
        val (read, readEdge) = readNode.in.head
        val (write, writeEdge) = writeNode.in.head
        val (system, systemEdge) = systemNode.in.head
        val lineCount = bankBytes / writeBeatBytes
        val lineIndexBits = log2Ceil(lineCount)
        val lineOffsetBits = log2Ceil(writeBeatBytes)
        val readIndexBits = log2Ceil(writeBeatBytes / readBeatBytes)
        val systemBeatCount = writeBeatBytes / systemMaxBytes
        val systemIndexBits = log2Ceil(systemBeatCount)
        val mem = SyncReadMem(lineCount, Vec(writeBeatBytes, UInt(8.W)))

        // Count the SRAM return against queue capacity before accepting another read.
        // Two outstanding reads stay below Gemmini's four-entry fromDMA queue.
        val responseDepth = 2
        val readResponses = Module(new Queue(chiselTypeOf(read.d.bits), responseDepth, flow = true))
        val writeResponses = Module(new Queue(chiselTypeOf(write.d.bits), responseDepth, pipe = true))
        val systemResponses = Module(new Queue(chiselTypeOf(system.d.bits), responseDepth, flow = true))
        val readPending = RegInit(false.B)
        val readSource = Reg(UInt(readEdge.bundle.sourceBits.W))
        val readSize = Reg(UInt(readEdge.bundle.sizeBits.W))
        val readBeat = Reg(UInt(math.max(1, readIndexBits).W))
        val systemPending = RegInit(false.B)
        val systemResponseRead = Reg(Bool())
        val systemSource = Reg(UInt(systemEdge.bundle.sourceBits.W))
        val systemSize = Reg(UInt(systemEdge.bundle.sizeBits.W))
        val systemBeat = Reg(UInt(math.max(1, systemIndexBits).W))

        val incomingReadLine = read.a.bits.address(
          lineOffsetBits + lineIndexBits - 1,
          lineOffsetBits)
        val incomingWriteLine = write.a.bits.address(
          lineOffsetBits + lineIndexBits - 1,
          lineOffsetBits)
        val incomingSystemLine = system.a.bits.address(
          lineOffsetBits + lineIndexBits - 1,
          lineOffsetBits)
        val systemWrite = systemEdge.hasData(system.a.bits)
        val readSpace = (readResponses.io.count +& readPending.asUInt) < responseDepth.U || read.d.fire
        val systemSpace = (systemResponses.io.count +& systemPending.asUInt) < responseDepth.U || system.d.fire

        val writeEligible = write.a.valid && writeResponses.io.enq.ready
        val systemWriteEligible = system.a.valid && systemWrite && systemSpace
        val selectWrite = writeEligible
        val selectSystemWrite = !selectWrite && systemWriteEligible
        val selectedWriteValid = selectWrite || selectSystemWrite
        val selectedWriteLine = Mux(selectWrite, incomingWriteLine, incomingSystemLine)

        val readEligible = read.a.valid && readSpace &&
          !(selectedWriteValid && incomingReadLine === selectedWriteLine)
        val systemReadEligible = system.a.valid && !systemWrite && systemSpace &&
          !(selectedWriteValid && incomingSystemLine === selectedWriteLine)
        val selectRead = readEligible
        val selectSystemRead = !selectRead && systemReadEligible
        val selectedReadValid = selectRead || selectSystemRead
        val selectedReadLine = Mux(selectRead, incomingReadLine, incomingSystemLine)

        val readData = mem.read(selectedReadLine, selectedReadValid)
        val readBeats = readData.asUInt.asTypeOf(
          Vec(writeBeatBytes / readBeatBytes, UInt((readBeatBytes * 8).W)))
        val systemBeats = readData.asUInt.asTypeOf(
          Vec(systemBeatCount, UInt((systemMaxBytes * 8).W)))
        val systemWriteData = Wire(Vec(writeBeatBytes, UInt(8.W)))
        val systemWriteMask = Wire(Vec(writeBeatBytes, Bool()))
        val incomingSystemBeat = if (systemIndexBits == 0) {
          0.U
        } else {
          system.a.bits.address(lineOffsetBits - 1, log2Ceil(systemMaxBytes))
        }
        for (beat <- 0 until systemBeatCount; byte <- 0 until systemMaxBytes) {
          val index = beat * systemMaxBytes + byte
          systemWriteData(index) := system.a.bits.data(8 * (byte + 1) - 1, 8 * byte)
          systemWriteMask(index) := system.a.bits.mask(byte) && incomingSystemBeat === beat.U
        }

        read.a.ready := selectRead
        read.d <> readResponses.io.deq
        readResponses.io.enq.valid := readPending
        readResponses.io.enq.bits := readEdge.AccessAck(readSource, readSize, readBeats(readBeat))
        read.b.valid := false.B
        read.c.ready := true.B
        read.e.ready := true.B

        readPending := selectRead
        when(selectRead) {
          readSource := read.a.bits.source
          readSize := read.a.bits.size
          if (readIndexBits == 0) {
            readBeat := 0.U
          } else {
            readBeat := read.a.bits.address(
              lineOffsetBits - 1,
              log2Ceil(readBeatBytes))
          }
        }
        write.a.ready := selectWrite
        write.d <> writeResponses.io.deq
        writeResponses.io.enq.valid := selectWrite
        writeResponses.io.enq.bits := writeEdge.AccessAck(write.a.bits.source, write.a.bits.size)
        write.b.valid := false.B
        write.c.ready := true.B
        write.e.ready := true.B

        system.a.ready := Mux(systemWrite, selectSystemWrite, selectSystemRead)
        system.d <> systemResponses.io.deq
        systemResponses.io.enq.valid := systemPending
        systemResponses.io.enq.bits := systemEdge.AccessAck(systemSource, systemSize)
        when(systemResponseRead) {
          systemResponses.io.enq.bits.opcode := TLMessages.AccessAckData
          systemResponses.io.enq.bits.data := systemBeats(systemBeat)
        }
        system.b.valid := false.B
        system.c.ready := true.B
        system.e.ready := true.B

        systemPending := system.a.fire
        when(system.a.fire) {
          systemResponseRead := selectSystemRead
          systemSource := system.a.bits.source
          systemSize := system.a.bits.size
          systemBeat := incomingSystemBeat
        }

        when(selectedWriteValid) {
          mem.write(
            selectedWriteLine,
            Mux(selectWrite, write.a.bits.data, systemWriteData.asUInt)
              .asTypeOf(Vec(writeBeatBytes, UInt(8.W))),
            Mux(selectWrite, write.a.bits.mask, systemWriteMask.asUInt).asBools)
        }
      }
    }
  }
}

/** Connects Gemmini and system traffic to one external scratchpad. */
class GemminiExternalSpmAttach(val gemminiRoCC: GemminiRoCC, params: GemminiExternalSpmParams)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  val gemminiAccelerator = gemminiRoCC.accelerator
  private val gemminiConfig = gemminiAccelerator.config
  val readBeatBytes: Int = gemminiConfig.sp_width / 8
  val writeBeatBytes: Int =
    gemminiConfig.meshColumns * gemminiConfig.tileColumns * gemminiConfig.accType.getWidth / 8
  val systemMaxBytes: Int = gemminiConfig.dma_buswidth / 8
  private val spmBytes =
    gemminiConfig.sp_banks * gemminiConfig.sp_bank_entries * readBeatBytes

  require(spmBytes == params.sizeBytes)

  val spm = LazyModule(new GemminiExternalSpm(
    params,
    gemminiConfig.sp_banks,
    readBeatBytes,
    writeBeatBytes,
    systemMaxBytes))
  val readPorts = TLXbar()
  val writePorts = TLXbar()
  val systemPorts = TLXbar()
  val writerNode = TLIdentityNode()
  val localNode = TLIdentityNode()

  spm.readNodes.foreach { node => node := readPorts }
  spm.writeNodes.foreach { node => node := writePorts }
  spm.systemNodes.foreach { node => node := systemPorts }
  readPorts :=* gemminiAccelerator.spad_read_nodes
  writePorts :=* TLWidthWidget(readBeatBytes) :=* TLBuffer() :=*
    gemminiAccelerator.spad_write_nodes
  writePorts := writerNode
  systemPorts := TLFIFOFixer() := TLWidthWidget(systemMaxBytes) := localNode
  if (gemminiRoCC.linkParams.isEmpty) {
    localNode := gemminiRoCC.localNode
  }

  override lazy val module = new AttachImpl
  class AttachImpl extends Impl
}

class GemminiExternalSpmWriter(gemminiAccelerator: gemmini.Gemmini[chisel3.SInt, gemmini.Float, gemmini.Float])(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  val node = TLIdentityNode()
  private val dmaBeatBytes = gemminiAccelerator.config.dma_buswidth / 8

  node := TLWidthWidget(dmaBeatBytes) := TLBuffer() :=
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
        tile.roccs.collect { case accelerator: GemminiRoCC => accelerator }
      case _ => Nil
    }
    require(gemminis.size == 1)
    val attach = LazyModule(new GemminiExternalSpmAttach(gemminis.head, params))

    attach.clockNode := sbus.fixedClockNode
    attach.spm.clockNode := sbus.fixedClockNode
    sbus.coupleTo("gemmini-ext-spm") {
      attach.systemPorts := TLFIFOFixer() := TLFragmenter(
        attach.systemMaxBytes,
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
    val writer = LazyModule(new GemminiExternalSpmWriter(attach.gemminiAccelerator))

    attach.writerNode := writer.node
    writer.clockNode := sbus.fixedClockNode
    writer
  }
}
