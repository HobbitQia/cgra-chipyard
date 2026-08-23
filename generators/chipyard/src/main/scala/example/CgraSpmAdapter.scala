package chipyard.example

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams}

object CgraSpmConfigKind {
  val Width = 1
  val Header = 0.U(Width.W)
  val Packet = 1.U(Width.W)
}

object CgraSpmStatus {
  val BadConfig = 1
  val BadPacket = 2
  val IdentityMismatch = 3
  val DmaMismatch = 4
}

case class CgraSpmParams(
  protocol: SpmDmaParams,
  cgra: CGRAParams,
  slotBases: Seq[BigInt],
  packetCapacity: Int = 16) {
  require(slotBases.size == protocol.slotCount)
  require(packetCapacity > 0)

  val packetCountWidth: Int = log2Ceil(packetCapacity + 1)
  val packetIndexWidth: Int = math.max(1, log2Ceil(packetCapacity))
  val wordBytes: Int = cgra.dataPayloadWidth / 8
  val spmBytes: Int = cgra.dma.spmWords * wordBytes
  val dmaBeatBytes: Int = cgra.dma.dramDataWidth / 8
}

class CgraSpmHeader(params: CgraSpmParams) extends Bundle {
  val jobId = UInt(params.protocol.jobIdWidth.W)
  val slot = UInt(params.protocol.slotWidth.W)
  val bytes = UInt(params.protocol.lengthWidth.W)
  val spmWordAddress = UInt(params.cgra.dma.spmAddrWidth.W)
  val dmaTag = UInt(params.cgra.dma.tagWidth.W)
  val packetCount = UInt(params.packetCountWidth.W)
}

class CgraSpmConfig(params: CgraSpmParams) extends Bundle {
  val kind = UInt(CgraSpmConfigKind.Width.W)
  val header = new CgraSpmHeader(params)
  val packet = UInt(params.cgra.intraPktWidth.W)
}

class CgraSpmConfigAck(params: CgraSpmParams) extends Bundle {
  val jobId = UInt(params.protocol.jobIdWidth.W)
  val slot = UInt(params.protocol.slotWidth.W)
  val bytes = UInt(params.protocol.lengthWidth.W)
  val status = UInt(SpmDmaStatus.Width.W)
  val detail = UInt(params.protocol.detailWidth.W)
}

class CgraSpmDmaRequest(params: CgraSpmParams) extends Bundle {
  val jobId = UInt(params.protocol.jobIdWidth.W)
  val slot = UInt(params.protocol.slotWidth.W)
  val sourceAddress = UInt(params.cgra.dma.dramAddrWidth.W)
  val spmWordAddress = UInt(params.cgra.dma.spmAddrWidth.W)
  val bytes = UInt(params.protocol.lengthWidth.W)
  val dmaTag = UInt(params.cgra.dma.tagWidth.W)
}

class CgraSpmDmaCompletion(params: CgraSpmParams) extends Bundle {
  val jobId = UInt(params.protocol.jobIdWidth.W)
  val slot = UInt(params.protocol.slotWidth.W)
  val dmaTag = UInt(params.cgra.dma.tagWidth.W)
}

class CgraSpmAsyncLink(params: CgraSpmParams) extends Bundle {
  private val crossing = AsyncQueueParams.singleton()
  val config = new AsyncBundle(new CgraSpmConfig(params), crossing)
  val configAck = Flipped(new AsyncBundle(new CgraSpmConfigAck(params), crossing))
  val transferStart = new AsyncBundle(new SpmDmaCommand(params.protocol), crossing)
  val transferDone = Flipped(new AsyncBundle(new SpmDmaResult(params.protocol), crossing))
  val consumerStart = new AsyncBundle(new SpmDmaCommand(params.protocol), crossing)
  val consumerDone = Flipped(new AsyncBundle(new SpmDmaResult(params.protocol), crossing))
}

/** CGRA-side adapter for one preconfigured SPM transfer and launch sequence. */
class CgraSpmEngine(params: CgraSpmParams) extends Module {
  val io = IO(new Bundle {
    val configIn = Flipped(Decoupled(new CgraSpmConfig(params)))
    val configAck = Decoupled(new CgraSpmConfigAck(params))
    val transferStart = Flipped(Decoupled(new SpmDmaCommand(params.protocol)))
    val transferDone = Decoupled(new SpmDmaResult(params.protocol))
    val consumerStart = Flipped(Decoupled(new SpmDmaCommand(params.protocol)))
    val consumerDone = Decoupled(new SpmDmaResult(params.protocol))
    val dmaRequest = Decoupled(new CgraSpmDmaRequest(params))
    val dmaCompletion = Flipped(Decoupled(new CgraSpmDmaCompletion(params)))
    val launchPacket = Decoupled(UInt(params.cgra.intraPktWidth.W))
    val complete = Flipped(Decoupled(UInt(params.protocol.resultWidth.W)))
    val cpuComputeActive = Input(Bool())
    val active = Output(Bool())
    val computeActive = Output(Bool())
  })

  val Seq(empty, collect, configResult, armed, issueDma, waitDma, dmaResult, readyCompute, launch, waitCompute, computeResult) = Enum(11)
  val state = RegInit(empty)
  val header = Reg(new CgraSpmHeader(params))
  val packets = Reg(Vec(params.packetCapacity, UInt(params.cgra.intraPktWidth.W)))
  val packetIndex = RegInit(0.U(params.packetCountWidth.W))
  val configStatus = RegInit(SpmDmaStatus.Success)
  val configDetail = RegInit(0.U(params.protocol.detailWidth.W))
  val stageDetail = RegInit(0.U(params.protocol.detailWidth.W))
  val resultData = RegInit(0.U(params.protocol.resultWidth.W))

  def commandMatches(command: SpmDmaCommand): Bool = {
    command.jobId === header.jobId && command.slot === header.slot && command.bytes === header.bytes
  }

  def setResult(out: DecoupledIO[SpmDmaResult], stage: UInt, detail: UInt, data: UInt): Unit = {
    out.bits.jobId := header.jobId
    out.bits.slot := header.slot
    out.bits.bytes := header.bytes
    out.bits.stage := stage
    out.bits.status := Mux(detail === 0.U, SpmDmaStatus.Success, SpmDmaStatus.StageFailure)
    out.bits.detail := detail
    out.bits.data := data
  }

  val headerWords = io.configIn.bits.header.bytes >> log2Ceil(params.wordBytes)
  val headerEnd = io.configIn.bits.header.spmWordAddress +& headerWords
  val headerValid = io.configIn.bits.header.jobId =/= 0.U &&
    io.configIn.bits.header.slot < params.protocol.slotCount.U &&
    io.configIn.bits.header.bytes =/= 0.U &&
    io.configIn.bits.header.bytes <= params.protocol.slotSizeBytes.U &&
    io.configIn.bits.header.bytes <= params.spmBytes.U &&
    (io.configIn.bits.header.bytes & (params.dmaBeatBytes - 1).U) === 0.U &&
    headerEnd <= params.cgra.dma.spmWords.U &&
    io.configIn.bits.header.packetCount =/= 0.U &&
    io.configIn.bits.header.packetCount <= params.packetCapacity.U

  val packetCommand = io.configIn.bits.packet(
    params.cgra.packetLayout.cmdLsb + params.cgra.cmdWidth - 1,
    params.cgra.packetLayout.cmdLsb)
  val packetValid = io.configIn.bits.kind === CgraSpmConfigKind.Packet &&
    packetCommand === CGRACmdGenerated.CMD_LAUNCH.U

  io.configIn.ready := state === empty || state === collect
  io.configAck.valid := state === configResult &&
    (configStatus =/= SpmDmaStatus.Success || !io.cpuComputeActive)
  io.configAck.bits.jobId := header.jobId
  io.configAck.bits.slot := header.slot
  io.configAck.bits.bytes := header.bytes
  io.configAck.bits.status := configStatus
  io.configAck.bits.detail := configDetail

  io.transferStart.ready := state === armed
  io.transferDone.valid := state === dmaResult
  setResult(io.transferDone, SpmDmaStage.Transfer, stageDetail, 0.U)
  io.consumerStart.ready := state === readyCompute
  io.consumerDone.valid := state === computeResult
  setResult(io.consumerDone, SpmDmaStage.Consumer, stageDetail, resultData)

  io.dmaRequest.valid := state === issueDma
  io.dmaRequest.bits.jobId := header.jobId
  io.dmaRequest.bits.slot := header.slot
  io.dmaRequest.bits.sourceAddress := VecInit(
    params.slotBases.map(_.U(params.cgra.dma.dramAddrWidth.W)))(header.slot)
  io.dmaRequest.bits.spmWordAddress := header.spmWordAddress
  io.dmaRequest.bits.bytes := header.bytes
  io.dmaRequest.bits.dmaTag := header.dmaTag
  io.dmaCompletion.ready := state === waitDma

  io.launchPacket.valid := state === launch
  io.launchPacket.bits := packets(packetIndex(params.packetIndexWidth - 1, 0))
  io.complete.ready := state === waitCompute
  io.active := state =/= empty
  io.computeActive := state === launch || state === waitCompute || state === computeResult

  when(io.configIn.fire) {
    when(state === empty) {
      header := io.configIn.bits.header
      packetIndex := 0.U
      when(io.configIn.bits.kind =/= CgraSpmConfigKind.Header || !headerValid) {
        configStatus := SpmDmaStatus.StageFailure
        configDetail := CgraSpmStatus.BadConfig.U
        state := configResult
      }.otherwise {
        configStatus := SpmDmaStatus.Success
        configDetail := 0.U
        state := collect
      }
    }.otherwise {
      when(!packetValid) {
        configStatus := SpmDmaStatus.StageFailure
        configDetail := CgraSpmStatus.BadPacket.U
        state := configResult
      }.otherwise {
        packets(packetIndex(params.packetIndexWidth - 1, 0)) := io.configIn.bits.packet
        when(packetIndex + 1.U === header.packetCount) {
          state := configResult
        }.otherwise {
          packetIndex := packetIndex + 1.U
        }
      }
    }
  }

  when(io.configAck.fire) {
    state := Mux(configStatus === SpmDmaStatus.Success, armed, empty)
  }

  when(io.transferStart.fire) {
    when(commandMatches(io.transferStart.bits)) {
      stageDetail := 0.U
      state := issueDma
    }.otherwise {
      stageDetail := CgraSpmStatus.IdentityMismatch.U
      state := dmaResult
    }
  }

  when(io.dmaRequest.fire) {
    state := waitDma
  }

  when(io.dmaCompletion.fire) {
    val matches = io.dmaCompletion.bits.jobId === header.jobId &&
      io.dmaCompletion.bits.slot === header.slot && io.dmaCompletion.bits.dmaTag === header.dmaTag
    stageDetail := Mux(matches, 0.U, CgraSpmStatus.DmaMismatch.U)
    state := dmaResult
  }

  when(io.transferDone.fire) {
    state := Mux(stageDetail === 0.U, readyCompute, empty)
  }

  when(io.consumerStart.fire) {
    when(commandMatches(io.consumerStart.bits)) {
      stageDetail := 0.U
      packetIndex := 0.U
      state := launch
    }.otherwise {
      stageDetail := CgraSpmStatus.IdentityMismatch.U
      resultData := 0.U
      state := computeResult
    }
  }

  when(io.launchPacket.fire) {
    when(packetIndex + 1.U === header.packetCount) {
      state := waitCompute
    }.otherwise {
      packetIndex := packetIndex + 1.U
    }
  }

  when(io.complete.fire) {
    stageDetail := 0.U
    resultData := io.complete.bits
    state := computeResult
  }

  when(io.consumerDone.fire) {
    state := empty
  }
}

class CgraPacketArbiter(width: Int) extends Module {
  val io = IO(new Bundle {
    val dma = Flipped(Decoupled(UInt(width.W)))
    val cpu = Flipped(Decoupled(UInt(width.W)))
    val launch = Flipped(Decoupled(UInt(width.W)))
    val out = Decoupled(UInt(width.W))
  })

  val selected = RegInit(0.U(2.W))
  val locked = RegInit(false.B)
  val choice = Mux(locked, selected, Mux(io.dma.valid, 0.U, Mux(io.cpu.valid, 1.U, 2.U)))

  io.out.valid := MuxLookup(choice, false.B)(Seq(
    0.U -> io.dma.valid,
    1.U -> io.cpu.valid,
    2.U -> io.launch.valid))
  io.out.bits := MuxLookup(choice, 0.U)(Seq(
    0.U -> io.dma.bits,
    1.U -> io.cpu.bits,
    2.U -> io.launch.bits))
  io.dma.ready := io.out.ready && Mux(locked, selected === 0.U, true.B)
  io.cpu.ready := io.out.ready && Mux(locked, selected === 1.U, !io.dma.valid)
  io.launch.ready := io.out.ready && Mux(locked, selected === 2.U, !io.dma.valid && !io.cpu.valid)

  when(!locked && io.out.valid && !io.out.ready) {
    locked := true.B
    selected := choice
  }.elsewhen(locked && io.out.fire) {
    locked := false.B
  }
}
