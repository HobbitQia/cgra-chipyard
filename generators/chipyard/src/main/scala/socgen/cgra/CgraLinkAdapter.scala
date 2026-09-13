package chipyard.socgen.cgra

import chisel3._
import chisel3.util._
import chipyard.example.CGRAParams
import chipyard.socgen.link._
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams}
import org.chipsalliance.cde.config.{Config, Field}

object CgraLinkStatus {
  val DmaMismatch = 3
}

object CgraSymbolSource {
  val Width = 2
  val Slot = 0
  val Elements = 1
  val TileId = 2
}

case class CgraLinkParams(auto: AutoLinkParams, cgra: CGRAParams, packetCapacity: Int) {
  require(packetCapacity > 0)

  val jobCount: Int = auto.stages.count(_.endpoint == "cgra")
  require(jobCount > 0)

  val packetCountWidth: Int = log2Ceil(packetCapacity + 1)
  val packetIndexWidth: Int = math.max(1, log2Ceil(packetCapacity))
  val packetAddressWidth: Int = math.max(1, log2Ceil(jobCount * packetCapacity))
  val jobIndexWidth: Int = math.max(1, log2Ceil(jobCount))
  val wordBytes: Int = cgra.dataPayloadWidth / 8
}

case class CgraLinkAttachParams(
  adapter: CgraLinkParams,
  portName: String,
  resultNames: Seq[String],
  controlAddress: BigInt,
  controlBytes: Int) {
  require(adapter.auto.endpoints.exists(_.name == portName))
  require(resultNames.nonEmpty && resultNames.distinct.size == resultNames.size)
  require(resultNames.forall(adapter.auto.resultNames.contains))
}

case object CgraLinkKey extends Field[Option[CgraLinkAttachParams]](None)

class WithCgraLink(params: CgraLinkAttachParams) extends Config((_, _, _) => { case CgraLinkKey => Some(params) })

class CgraLinkConfig(params: CgraLinkParams) extends Bundle {
  val job = UInt(params.jobIndexWidth.W)
  val packetCount = UInt(params.packetCountWidth.W)
  val expectedCompletions = UInt(params.packetCountWidth.W)
  val patchCount = UInt(params.packetCountWidth.W)
}

class CgraPatch extends Bundle {
  val source = UInt(CgraSymbolSource.Width.W)
  val coefficient = UInt(32.W)
  val bias = UInt(32.W)
}

class CgraPatchConfig extends CgraPatch {
  val packetIndex = UInt(32.W)
}

class CgraLinkConfigAck extends Bundle {
  val done = Bool()
}

class CgraLinkDmaRequest(params: CgraLinkParams) extends Bundle {
  val sourceAddress = UInt(params.cgra.dma.dramAddrWidth.W)
  val spmWordAddress = UInt(params.cgra.dma.spmAddrWidth.W)
  val bytes = UInt(params.auto.lengthWidth.W)
  val packed = Bool()
  val dmaTag = UInt(params.cgra.dma.tagWidth.W)
}

class CgraLinkDmaCompletion(params: CgraLinkParams) extends Bundle {
  val dmaTag = UInt(params.cgra.dma.tagWidth.W)
}

class CgraLinkConfigAsync(params: CgraLinkParams) extends Bundle {
  private val crossing = AsyncQueueParams.singleton()
  val config = new AsyncBundle(new CgraLinkConfig(params), crossing)
  val patch = new AsyncBundle(new CgraPatchConfig, crossing)
  val ack = Flipped(new AsyncBundle(new CgraLinkConfigAck, crossing))
}

/** Translates AutoLink copy and compute requests into CGRA traffic. */
class CgraLinkAdapter(params: CgraLinkParams) extends Module {
  val io = IO(new Bundle {
    val configIn = Flipped(Decoupled(new CgraLinkConfig(params)))
    val configAck = Decoupled(new CgraLinkConfigAck)
    val patchIn = Flipped(Decoupled(new CgraPatchConfig))
    val packetIn = Flipped(Decoupled(UInt(params.cgra.intraPktWidth.W)))
    val captureActive = Output(Bool())
    val autoLink = new AutoEndpointIO(params.auto)
    val dmaRequest = Decoupled(new CgraLinkDmaRequest(params))
    val dmaCompletion = Flipped(Decoupled(new CgraLinkDmaCompletion(params)))
    val jobPacket = Decoupled(UInt(params.cgra.intraPktWidth.W))
    val computeResult = Flipped(Decoupled(UInt(params.auto.resultWidth.W)))
    val computeActive = Output(Bool())
  })

  object ConfigState {
    val idle :: collectPatches :: collectPackets :: reportConfig :: Nil = Enum(4)
  }
  object ExecState {
    val idle :: issueDma :: waitDma :: reportCopy :: readPacket :: loadPacket :: sendPacket :: waitCompute :: reportCompute :: Nil = Enum(9)
  }

  val configState = RegInit(ConfigState.idle)
  val execState = RegInit(ExecState.idle)
  val config = Reg(new CgraLinkConfig(params))
  val copy = Reg(new AutoCopyRequest(params.auto))
  // Static controls remain in CGRA; each captured job contains only runtime packets.
  val packets = SyncReadMem(
    params.jobCount * params.packetCapacity,
    UInt(params.cgra.intraPktWidth.W))
  val patches = SyncReadMem(params.jobCount * params.packetCapacity, new CgraPatch)
  val patchValid = RegInit(VecInit(Seq.fill(params.jobCount)(0.U(params.packetCapacity.W))))
  val elements = RegInit(VecInit(Seq.fill(params.jobCount)(0.U(params.auto.lengthWidth.W))))
  val jobPacketCount = Reg(Vec(params.jobCount, UInt(params.packetCountWidth.W)))
  val jobExpectedCompletions = Reg(Vec(params.jobCount, UInt(params.packetCountWidth.W)))
  val configIndex = RegInit(0.U(params.packetCountWidth.W))
  val patchIndex = RegInit(0.U(params.packetCountWidth.W))
  val replayIndex = RegInit(0.U(params.packetCountWidth.W))
  val replayPacket = Reg(UInt(params.cgra.intraPktWidth.W))
  val expectedCompletions = Reg(UInt(params.packetCountWidth.W))
  val completed = RegInit(0.U(params.packetCountWidth.W))
  val configDone = RegInit(false.B)
  val copyDetail = RegInit(0.U(params.auto.detailWidth.W))
  val resultData = RegInit(0.U(params.auto.resultWidth.W))
  val publicationArmed = RegInit(false.B)
  val publicationValid = RegInit(false.B)
  val publicationData = RegInit(0.U(params.auto.resultWidth.W))
  val publicationJob = RegInit(0.U(params.auto.jobWidth.W))
  val computeJob = RegInit(0.U(params.auto.jobWidth.W))
  val tileId = Reg(UInt((2 * params.auto.lengthWidth).W))
  val slot = RegInit(0.U(params.auto.slotWidth.W))

  val captureAddress = (config.job * params.packetCapacity.U + configIndex)(
    params.packetAddressWidth - 1,
    0)
  val replayAddress = (computeJob * params.packetCapacity.U + replayIndex)(
    params.packetAddressWidth - 1,
    0)
  val packetRead = packets.read(replayAddress, execState === ExecState.readPacket)
  val patchRead = patches.read(replayAddress, execState === ExecState.readPacket)
  def selected[T <: Data](values: Vec[T], job: UInt): T = {
    if (params.jobCount == 1) values.head else values(job(params.jobIndexWidth - 1, 0))
  }
  val autoDmaTag = 0.U(params.cgra.dma.tagWidth.W)
  val completionFire = io.computeResult.fire
  val completedNext = completed + completionFire
  val finalResultData = Mux(completionFire, io.computeResult.bits, resultData)

  io.configIn.ready := configState === ConfigState.idle && execState === ExecState.idle
  io.packetIn.ready := configState === ConfigState.collectPackets
  io.patchIn.ready := configState === ConfigState.collectPatches
  io.captureActive := configState =/= ConfigState.idle || io.configIn.valid
  io.configAck.valid := configState === ConfigState.reportConfig
  io.configAck.bits.done := configDone

  io.autoLink.watchOutput.ready := !publicationArmed && !publicationValid
  io.autoLink.reportOutput.valid := publicationValid
  io.autoLink.reportOutput.bits.stage := 0.U
  io.autoLink.reportOutput.bits.job := publicationJob
  io.autoLink.reportOutput.bits.status := AutoLinkStatus.Success
  io.autoLink.reportOutput.bits.detail := 0.U
  io.autoLink.reportOutput.bits.data := publicationData
  io.autoLink.requestCopy.ready := execState === ExecState.idle &&
    configState === ConfigState.idle && !io.configIn.valid &&
    !io.autoLink.requestCompute.valid
  io.autoLink.reportCopy.valid := execState === ExecState.reportCopy
  io.autoLink.reportCopy.bits.task := copy.task
  io.autoLink.reportCopy.bits.status := Mux(
    copyDetail === 0.U,
    AutoLinkStatus.Success,
    AutoLinkStatus.SinkFailure)
  io.autoLink.reportCopy.bits.detail := copyDetail
  io.autoLink.requestCompute.ready := execState === ExecState.idle &&
    configState === ConfigState.idle && !io.configIn.valid
  io.autoLink.reportCompute.valid := execState === ExecState.reportCompute
  io.autoLink.reportCompute.bits.stage := 0.U
  io.autoLink.reportCompute.bits.job := computeJob
  io.autoLink.reportCompute.bits.status := AutoLinkStatus.Success
  io.autoLink.reportCompute.bits.detail := 0.U
  io.autoLink.reportCompute.bits.data := resultData

  io.dmaRequest.valid := execState === ExecState.issueDma
  io.dmaRequest.bits.sourceAddress := copy.sourceAddress
  io.dmaRequest.bits.spmWordAddress :=
    copy.destinationOffset >> log2Ceil(params.wordBytes)
  io.dmaRequest.bits.bytes := copy.destinationBytes
  io.dmaRequest.bits.packed := copy.destinationBytes =/= copy.bytes
  io.dmaRequest.bits.dmaTag := autoDmaTag
  io.dmaCompletion.ready := execState === ExecState.waitDma

  io.jobPacket.valid := execState === ExecState.sendPacket
  io.jobPacket.bits := replayPacket
  io.computeResult.ready := execState === ExecState.readPacket ||
    execState === ExecState.loadPacket || execState === ExecState.sendPacket ||
    execState === ExecState.waitCompute
  io.computeActive := io.computeResult.ready

  when(io.autoLink.watchOutput.fire) {
    publicationJob := io.autoLink.watchOutput.bits.job
    publicationArmed := true.B
  }
  when(io.autoLink.reportOutput.fire) {
    publicationValid := false.B
  }

  when(io.configIn.fire) {
    config := io.configIn.bits
    configIndex := 0.U
    patchIndex := 0.U
    configDone := false.B
    selected(patchValid, io.configIn.bits.job) := 0.U
    selected(elements, io.configIn.bits.job) := 0.U
    configState := ConfigState.reportConfig
  }
  when(io.patchIn.fire) {
    val patch = io.patchIn.bits
    val index = patch.packetIndex(params.packetIndexWidth - 1, 0)
    val address = (config.job * params.packetCapacity.U + patch.packetIndex)(params.packetAddressWidth - 1, 0)
    val entry = Wire(new CgraPatch)
    entry.source := patch.source
    entry.coefficient := patch.coefficient
    entry.bias := patch.bias
    patches.write(address, entry)
    selected(patchValid, config.job) := selected(patchValid, config.job) |
      UIntToOH(index, params.packetCapacity)
    when(patchIndex + 1.U === config.patchCount) {
      configState := ConfigState.collectPackets
    }.otherwise {
      patchIndex := patchIndex + 1.U
    }
  }
  when(io.packetIn.fire) {
    packets.write(captureAddress, io.packetIn.bits)
    when(configIndex + 1.U === config.packetCount) {
      selected(jobPacketCount, config.job) := config.packetCount
      selected(jobExpectedCompletions, config.job) := config.expectedCompletions
      configDone := true.B
      configState := ConfigState.reportConfig
    }.otherwise {
      configIndex := configIndex + 1.U
    }
  }
  when(io.configAck.fire) {
    configState := Mux(
      configDone,
      ConfigState.idle,
      Mux(config.patchCount =/= 0.U, ConfigState.collectPatches, ConfigState.collectPackets))
  }

  when(io.autoLink.requestCopy.fire) {
    copy := io.autoLink.requestCopy.bits
    copyDetail := 0.U
    execState := ExecState.issueDma
  }
  when(io.dmaRequest.fire) {
    execState := ExecState.waitDma
  }
  when(io.dmaCompletion.fire) {
    when(io.dmaCompletion.bits.dmaTag =/= autoDmaTag) {
      copyDetail := CgraLinkStatus.DmaMismatch.U
    }.otherwise {
      selected(elements, copy.job) := copy.destinationBytes >> log2Ceil(params.wordBytes)
    }
    execState := ExecState.reportCopy
  }
  when(io.autoLink.reportCopy.fire) {
    execState := ExecState.idle
  }

  def finishCompute(data: UInt): Unit = {
    resultData := data
    when(publicationArmed) {
      publicationArmed := false.B
      publicationValid := true.B
      publicationData := data
    }
    execState := ExecState.reportCompute
  }

  when(io.autoLink.requestCompute.fire) {
    computeJob := io.autoLink.requestCompute.bits.job
    tileId := io.autoLink.requestCompute.bits.tile.id
    slot := io.autoLink.requestCompute.bits.slot
    when(io.autoLink.requestCompute.bits.start) {
      val job = io.autoLink.requestCompute.bits.job
      replayIndex := 0.U
      expectedCompletions := selected(jobExpectedCompletions, job)
      completed := 0.U
      resultData := 0.U
      execState := ExecState.readPacket
    }.otherwise {
      publicationArmed := false.B
      publicationValid := false.B
    }
  }
  when(execState === ExecState.readPacket) {
    execState := ExecState.loadPacket
  }
  when(execState === ExecState.loadPacket) {
    val value = MuxLookup(patchRead.source, slot)(Seq(
      CgraSymbolSource.Elements.U -> selected(elements, computeJob),
      CgraSymbolSource.TileId.U -> tileId))
    val payload = Wire(UInt(params.cgra.dataPayloadWidth.W))
    payload := value * patchRead.coefficient + patchRead.bias
    val lsb = params.cgra.packetLayout.dataPayloadLsb
    val mask = (((BigInt(1) << params.cgra.dataPayloadWidth) - 1) << lsb).U(params.cgra.intraPktWidth.W)
    replayPacket := Mux(selected(patchValid, computeJob)(replayIndex(params.packetIndexWidth - 1, 0)),
      (packetRead & ~mask) | (payload << lsb), packetRead)
    execState := ExecState.sendPacket
  }
  when(execState === ExecState.sendPacket && io.jobPacket.fire) {
    when(replayIndex + 1.U === selected(jobPacketCount, computeJob)) {
      when(completedNext === expectedCompletions) {
        finishCompute(finalResultData)
      }.otherwise {
        execState := ExecState.waitCompute
      }
    }.otherwise {
      replayIndex := replayIndex + 1.U
      execState := ExecState.readPacket
    }
  }
  when(completionFire) {
    resultData := io.computeResult.bits
    completed := completedNext
    when(execState === ExecState.waitCompute && completedNext === expectedCompletions) {
      finishCompute(io.computeResult.bits)
    }
  }
  when(io.autoLink.reportCompute.fire) {
    execState := ExecState.idle
  }
}

class CgraPacketArbiter(width: Int) extends Module {
  val io = IO(new Bundle {
    val dma = Flipped(Decoupled(UInt(width.W)))
    val cpu = Flipped(Decoupled(UInt(width.W)))
    val link = Flipped(Decoupled(UInt(width.W)))
    val out = Decoupled(UInt(width.W))
  })

  val selected = RegInit(0.U(2.W))
  val locked = RegInit(false.B)
  val choice = Mux(locked, selected, Mux(io.dma.valid, 0.U, Mux(io.cpu.valid, 1.U, 2.U)))

  io.out.valid := MuxLookup(choice, false.B)(Seq(
    0.U -> io.dma.valid,
    1.U -> io.cpu.valid,
    2.U -> io.link.valid))
  io.out.bits := MuxLookup(choice, 0.U)(Seq(
    0.U -> io.dma.bits,
    1.U -> io.cpu.bits,
    2.U -> io.link.bits))
  io.dma.ready := io.out.ready && Mux(locked, selected === 0.U, true.B)
  io.cpu.ready := io.out.ready && Mux(locked, selected === 1.U, !io.dma.valid)
  io.link.ready := io.out.ready && Mux(locked, selected === 2.U, !io.dma.valid && !io.cpu.valid)

  when(!locked && io.out.valid && !io.out.ready) {
    locked := true.B
    selected := choice
  }.elsewhen(locked && io.out.fire) {
    locked := false.B
  }
}
