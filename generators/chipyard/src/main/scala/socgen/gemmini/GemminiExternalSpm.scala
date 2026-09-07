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

        val systemReadPending = RegInit(false.B)
        val systemResponseValid = RegInit(false.B)
        val systemResponseRead = Reg(Bool())
        val systemSource = Reg(UInt(systemEdge.bundle.sourceBits.W))
        val systemSize = Reg(UInt(systemEdge.bundle.sizeBits.W))
        val systemBeat = Reg(UInt(math.max(1, systemIndexBits).W))
        val systemData = Reg(UInt((systemMaxBytes * 8).W))

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
        val readBusy = readPending || systemReadPending
        val systemBusy = systemReadPending || systemResponseValid

        val writeEligible = write.a.valid && !writePending &&
          !(readBusy && incomingWriteLine === readLine)
        val systemWriteEligible = system.a.valid && systemWrite && !systemBusy &&
          !(readBusy && incomingSystemLine === readLine)
        val selectWrite = writeEligible
        val selectSystemWrite = !selectWrite && systemWriteEligible
        val selectedWriteValid = selectWrite || selectSystemWrite
        val selectedWriteLine = Mux(selectWrite, incomingWriteLine, incomingSystemLine)

        val readEligible = read.a.valid && !readBusy && !responseValid &&
          !(selectedWriteValid && incomingReadLine === selectedWriteLine)
        val systemReadEligible = system.a.valid && !systemWrite && !systemBusy && !readBusy &&
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
        read.d.valid := responseValid
        read.d.bits := readEdge.AccessAck(readSource, readSize, responseData)
        read.b.valid := false.B
        read.c.ready := true.B
        read.e.ready := true.B

        when(selectRead) {
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

        write.a.ready := selectWrite
        write.d.valid := writePending
        write.d.bits := writeEdge.AccessAck(writeSource, writeSize)
        write.b.valid := false.B
        write.c.ready := true.B
        write.e.ready := true.B

        when(selectWrite) {
          writePending := true.B
          writeSource := write.a.bits.source
          writeSize := write.a.bits.size
        }
        when(write.d.fire) {
          writePending := false.B
        }

        system.a.ready := Mux(systemWrite, selectSystemWrite, selectSystemRead)
        system.d.valid := systemResponseValid
        system.d.bits := systemEdge.AccessAck(systemSource, systemSize)
        when(systemResponseRead) {
          system.d.bits.opcode := TLMessages.AccessAckData
          system.d.bits.data := systemData
        }
        system.b.valid := false.B
        system.c.ready := true.B
        system.e.ready := true.B

        when(selectSystemRead) {
          systemReadPending := true.B
          systemResponseRead := true.B
          systemSource := system.a.bits.source
          systemSize := system.a.bits.size
          readLine := incomingSystemLine
          systemBeat := incomingSystemBeat
        }
        when(systemReadPending) {
          systemReadPending := false.B
          systemResponseValid := true.B
          systemData := systemBeats(systemBeat)
        }
        when(selectSystemWrite) {
          systemResponseValid := true.B
          systemResponseRead := false.B
          systemSource := system.a.bits.source
          systemSize := system.a.bits.size
        }
        when(system.d.fire) {
          systemResponseValid := false.B
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
