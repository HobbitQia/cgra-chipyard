package chipyard.socgen.cgra

import chisel3._
import chisel3.util._
import chipyard.example.{CGRACmdGenerated, CGRAParams}
import chipyard.socgen.link._
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams}
import org.chipsalliance.cde.config.{Config, Field}

object CgraLinkStatus {
  val BadConfig = 1
  val BadPacket = 2
  val DmaMismatch = 3
}

object CgraSymbolSource {
  val Width = 2
  val Slot = 0
  val Elements = 1
  val TileId = 2
}

case class CgraLinkParams(auto: AutoLinkParams, cgra: CGRAParams, packetCapacity: Int, symbolCapacity: Int = 8) {
  require(packetCapacity > 0)
  require(symbolCapacity > 0)

  val jobCount: Int = auto.stages.count(_.endpoint == "cgra")
  require(jobCount > 0)

  val packetCountWidth: Int = log2Ceil(packetCapacity + 1)
  val packetIndexWidth: Int = math.max(1, log2Ceil(packetCapacity))
  val packetAddressWidth: Int = math.max(1, log2Ceil(jobCount * packetCapacity))
  val jobIndexWidth: Int = math.max(1, log2Ceil(jobCount))
  val symbolIndexWidth: Int = math.max(1, log2Ceil(symbolCapacity))
  val symbolCountWidth: Int = log2Ceil(symbolCapacity + 1)
  val maxExpectedCompletions: BigInt = (BigInt(1) << packetCountWidth) - 1
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
  val job = UInt(32.W)
  val packetCount = UInt(32.W)
  val expectedCompletions = UInt(32.W)
  val symbolCount = UInt(32.W)
  val patchCount = UInt(32.W)
  val repeatCount = UInt(32.W)
  val writeback = new CgraWritebackConfig
}

class CgraSymbolConfig extends Bundle {
  val base = UInt(32.W)
  val stride = UInt(32.W)
  val source = UInt(CgraSymbolSource.Width.W)
}

class CgraPatchConfig extends Bundle {
  val packetIndex = UInt(32.W)
  val symbolIndex = UInt(32.W)
  val scale = UInt(32.W)
  val offset = UInt(32.W)
}

class CgraPatch(params: CgraLinkParams) extends Bundle {
  val symbolIndex = UInt(params.symbolIndexWidth.W)
  val scale = UInt(32.W)
  val offset = UInt(32.W)
}

class CgraLinkConfigAck(params: CgraLinkParams) extends Bundle {
  val job = UInt(32.W)
  val done = Bool()
  val status = UInt(AutoLinkStatus.Width.W)
  val detail = UInt(params.auto.detailWidth.W)
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
  val symbol = new AsyncBundle(new CgraSymbolConfig, crossing)
  val patch = new AsyncBundle(new CgraPatchConfig, crossing)
  val repeat = new AsyncBundle(UInt(32.W), crossing)
  val ack = Flipped(new AsyncBundle(new CgraLinkConfigAck(params), crossing))
}

/** Translates AutoLink copy and compute requests into CGRA traffic. */
class CgraLinkAdapter(params: CgraLinkParams) extends Module {
  val io = IO(new Bundle {
    val configIn = Flipped(Decoupled(new CgraLinkConfig(params)))
    val configAck = Decoupled(new CgraLinkConfigAck(params))
    val symbolIn = Flipped(Decoupled(new CgraSymbolConfig))
    val patchIn = Flipped(Decoupled(new CgraPatchConfig))
    val repeatIn = Flipped(Decoupled(UInt(32.W)))
    val packetIn = Flipped(Decoupled(UInt(params.cgra.intraPktWidth.W)))
    val captureActive = Output(Bool())
    val autoLink = new AutoEndpointIO(params.auto)
    val dmaRequest = Decoupled(new CgraLinkDmaRequest(params))
    val dmaCompletion = Flipped(Decoupled(new CgraLinkDmaCompletion(params)))
    val jobPacket = Decoupled(UInt(params.cgra.intraPktWidth.W))
    val resetRequest = Decoupled(Bool())
    val invalidateResident = Input(Bool())
    val computeResult = Flipped(Decoupled(UInt(params.auto.resultWidth.W)))
    val computeActive = Output(Bool())
    val writeback = Decoupled(new CgraWritebackRequest(params))
    val writebackDone = Flipped(Decoupled(UInt(AutoLinkStatus.Width.W)))
  })

  object ConfigState {
    val idle :: collectSymbols :: collectPatches :: collectRepeats :: collectPackets :: reportConfig :: Nil = Enum(6)
  }
  object ExecState {
    val idle :: issueDma :: waitDma :: reportCopy :: waitReset :: readPacket :: loadPacket :: sendPacket :: waitCompute :: issueWriteback :: waitWriteback :: reportCompute :: Nil = Enum(12)
  }

  val configState = RegInit(ConfigState.idle)
  val execState = RegInit(ExecState.idle)
  val config = Reg(new CgraLinkConfig(params))
  val copy = Reg(new AutoCopyRequest(params.auto))
  val packets = SyncReadMem(
    params.jobCount * params.packetCapacity,
    UInt(params.cgra.intraPktWidth.W))
  val patches = SyncReadMem(params.jobCount * params.packetCapacity, new CgraPatch(params))
  val patchValid = RegInit(VecInit(Seq.fill(params.jobCount)(0.U(params.packetCapacity.W))))
  // Masks retain original packet indices so relocations use the same cache address.
  val repeatMask = RegInit(VecInit(Seq.fill(params.jobCount)(0.U(params.packetCapacity.W))))
  val launchMask = RegInit(VecInit(Seq.fill(params.jobCount)(0.U(params.packetCapacity.W))))
  val symbols = Reg(Vec(params.jobCount, Vec(params.symbolCapacity, new CgraSymbolConfig)))
  val elements = RegInit(VecInit(Seq.fill(params.jobCount)(0.U(params.auto.lengthWidth.W))))
  val copyJobs = VecInit((0 until params.jobCount).map(job => params.auto.dependencies.exists { dependency =>
    val stage = params.auto.stage(dependency.destination)
    dependency.copy.nonEmpty && stage.endpoint == "cgra" && stage.job == job
  }.B))
  val jobValid = RegInit(VecInit(Seq.fill(params.jobCount)(false.B)))
  val jobPacketCount = Reg(Vec(params.jobCount, UInt(params.packetCountWidth.W)))
  val jobExpectedCompletions = Reg(Vec(params.jobCount, UInt(params.packetCountWidth.W)))
  val jobWriteback = Reg(Vec(params.jobCount, new CgraWritebackConfig))
  val configIndex = RegInit(0.U(params.packetCountWidth.W))
  val symbolIndex = RegInit(0.U(params.symbolCountWidth.W))
  val patchIndex = RegInit(0.U(params.packetCountWidth.W))
  val repeatIndex = RegInit(0.U(params.packetCountWidth.W))
  val configLaunchCount = RegInit(0.U(params.packetCountWidth.W))
  val configSawLaunch = RegInit(false.B)
  val configSawConfig = RegInit(false.B)
  val configFailed = RegInit(false.B)
  val replayIndex = RegInit(0.U(params.packetCountWidth.W))
  val replayPacket = Reg(UInt(params.cgra.intraPktWidth.W))
  val replayMask = Reg(UInt(params.packetCapacity.W))
  val rearming = RegInit(false.B)
  val residentValid = RegInit(false.B)
  val residentJob = Reg(UInt(params.auto.jobWidth.W))
  val priorJobComplete = RegInit(false.B)
  val expectedCompletions = Reg(UInt(params.packetCountWidth.W))
  val completed = RegInit(0.U(params.packetCountWidth.W))
  val configDone = RegInit(false.B)
  val configStatus = RegInit(AutoLinkStatus.Success)
  val configDetail = RegInit(0.U(params.auto.detailWidth.W))
  val copyDetail = RegInit(0.U(params.auto.detailWidth.W))
  val resultData = RegInit(0.U(params.auto.resultWidth.W))
  val resultStatus = RegInit(AutoLinkStatus.Success)
  val resultDetail = RegInit(0.U(params.auto.detailWidth.W))
  val publicationArmed = RegInit(false.B)
  val publicationValid = RegInit(false.B)
  val publicationStatus = RegInit(AutoLinkStatus.Success)
  val publicationDetail = RegInit(0.U(params.auto.detailWidth.W))
  val publicationData = RegInit(0.U(params.auto.resultWidth.W))
  val publicationJob = RegInit(0.U(params.auto.jobWidth.W))
  val computeJob = RegInit(0.U(params.auto.jobWidth.W))
  val computeTile = Reg(new AutoTile(params.auto.lengthWidth))
  val slot = RegInit(0.U(params.auto.slotWidth.W))

  val configJobValid = io.configIn.bits.job < params.jobCount.U
  val configValid = configJobValid && io.configIn.bits.packetCount =/= 0.U &&
    io.configIn.bits.packetCount <= params.packetCapacity.U &&
    io.configIn.bits.expectedCompletions =/= 0.U &&
    io.configIn.bits.expectedCompletions <= params.maxExpectedCompletions.U &&
    io.configIn.bits.symbolCount <= params.symbolCapacity.U &&
    io.configIn.bits.patchCount <= io.configIn.bits.packetCount &&
    io.configIn.bits.repeatCount <= io.configIn.bits.packetCount &&
    (io.configIn.bits.patchCount === 0.U || io.configIn.bits.symbolCount =/= 0.U)
  val packetCommand = io.packetIn.bits(
    params.cgra.packetLayout.cmdLsb + params.cgra.cmdWidth - 1,
    params.cgra.packetLayout.cmdLsb)
  val packetIsLaunch = packetCommand === CGRACmdGenerated.CMD_LAUNCH.U
  val packetIsConfig =
    (packetCommand >= CGRACmdGenerated.CMD_CONFIG.U &&
      packetCommand <= CGRACmdGenerated.CMD_CONFIG_CTRL_LOWER_BOUND.U) ||
      (packetCommand >= CGRACmdGenerated.CMD_CONFIG_STREAMING_LD_START_ADDR.U &&
        packetCommand <= CGRACmdGenerated.CMD_CONFIG_STREAMING_LD_END_ADDR.U) ||
      (packetCommand >= CGRACmdGenerated.CMD_CONFIG_LOOP_LOWER.U &&
        packetCommand <= CGRACmdGenerated.CMD_CONFIG_LOOP_STEP.U) ||
      (packetCommand >= CGRACmdGenerated.CMD_LC_CONFIG_LOWER.U &&
        packetCommand <= CGRACmdGenerated.CMD_LC_CONFIG_PARENT.U) ||
      packetCommand === CGRACmdGenerated.CMD_CONST.U ||
      packetCommand === CGRACmdGenerated.CMD_RECORD_PHI_ADDR.U ||
      packetCommand === CGRACmdGenerated.CMD_CONFIG_GEP_STRIDE.U
  val packetInOrder = !configSawLaunch || packetIsLaunch
  val captureAddress = (config.job * params.packetCapacity.U + configIndex)(
    params.packetAddressWidth - 1,
    0)
  val replayAddress = (computeJob * params.packetCapacity.U + replayIndex)(
    params.packetAddressWidth - 1,
    0)
  val packetRead = packets.read(replayAddress, execState === ExecState.readPacket)
  val patchRead = patches.read(replayAddress, execState === ExecState.readPacket)
  val computeJobInRange = io.autoLink.requestCompute.bits.job < params.jobCount.U
  def selected[T <: Data](values: Vec[T], job: UInt): T = {
    if (params.jobCount == 1) values.head else values(job(params.jobIndexWidth - 1, 0))
  }
  val computeJobValid = computeJobInRange && selected(
    jobValid,
    io.autoLink.requestCompute.bits.job)
  val autoDmaTag = 0.U(params.cgra.dma.tagWidth.W)
  val completionFire = io.computeResult.fire
  val completedNext = completed + completionFire
  val finalResultData = Mux(completionFire, io.computeResult.bits, resultData)

  io.configIn.ready := configState === ConfigState.idle && execState === ExecState.idle
  io.packetIn.ready := configState === ConfigState.collectPackets
  io.symbolIn.ready := configState === ConfigState.collectSymbols
  io.patchIn.ready := configState === ConfigState.collectPatches
  io.repeatIn.ready := configState === ConfigState.collectRepeats
  io.captureActive := configState =/= ConfigState.idle || io.configIn.valid
  io.configAck.valid := configState === ConfigState.reportConfig
  io.configAck.bits.job := config.job
  io.configAck.bits.done := configDone
  io.configAck.bits.status := configStatus
  io.configAck.bits.detail := configDetail

  io.autoLink.watchOutput.ready := !publicationArmed && !publicationValid
  io.autoLink.reportOutput.valid := publicationValid
  io.autoLink.reportOutput.bits.stage := 0.U
  io.autoLink.reportOutput.bits.job := publicationJob
  io.autoLink.reportOutput.bits.status := publicationStatus
  io.autoLink.reportOutput.bits.detail := publicationDetail
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
  io.autoLink.reportCompute.bits.status := resultStatus
  io.autoLink.reportCompute.bits.detail := resultDetail
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
  io.resetRequest.valid := execState === ExecState.waitReset
  io.resetRequest.bits := true.B
  io.computeResult.ready := execState === ExecState.readPacket ||
    execState === ExecState.loadPacket || execState === ExecState.sendPacket ||
    execState === ExecState.waitCompute
  io.computeActive := io.computeResult.ready
  io.writeback.valid := execState === ExecState.issueWriteback
  io.writeback.bits.config := selected(jobWriteback, computeJob)
  io.writeback.bits.tile := computeTile
  io.writeback.bits.slot := slot
  io.writebackDone.ready := execState === ExecState.waitWriteback

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
    symbolIndex := 0.U
    patchIndex := 0.U
    repeatIndex := 0.U
    configLaunchCount := 0.U
    configSawLaunch := false.B
    configSawConfig := false.B
    configFailed := false.B
    configDone := !configValid
    when(configJobValid) {
      selected(jobValid, io.configIn.bits.job) := false.B
      selected(patchValid, io.configIn.bits.job) := 0.U
      selected(repeatMask, io.configIn.bits.job) := 0.U
      selected(launchMask, io.configIn.bits.job) := 0.U
      selected(elements, io.configIn.bits.job) := 0.U
      when(residentJob === io.configIn.bits.job) { residentValid := false.B }
    }
    when(configValid) {
      configStatus := AutoLinkStatus.Success
      configDetail := 0.U
    }.otherwise {
      configStatus := AutoLinkStatus.SinkFailure
      configDetail := CgraLinkStatus.BadConfig.U
    }
    configState := ConfigState.reportConfig
  }
  when(io.symbolIn.fire) {
    selected(symbols, config.job)(symbolIndex(params.symbolIndexWidth - 1, 0)) := io.symbolIn.bits
    when(io.symbolIn.bits.source > CgraSymbolSource.TileId.U ||
      (io.symbolIn.bits.source === CgraSymbolSource.Elements.U && !selected(copyJobs, config.job))) {
      configFailed := true.B
    }
    when(symbolIndex + 1.U === config.symbolCount) {
      configState := Mux(config.patchCount =/= 0.U, ConfigState.collectPatches,
        Mux(config.repeatCount =/= 0.U, ConfigState.collectRepeats, ConfigState.collectPackets))
    }.otherwise {
      symbolIndex := symbolIndex + 1.U
    }
  }
  when(io.patchIn.fire) {
    val patch = io.patchIn.bits
    val index = patch.packetIndex(params.packetIndexWidth - 1, 0)
    val inRange = patch.packetIndex < config.packetCount && patch.symbolIndex < config.symbolCount
    val valid = inRange && !selected(patchValid, config.job)(index)
    val address = (config.job * params.packetCapacity.U + patch.packetIndex)(params.packetAddressWidth - 1, 0)
    when(valid) {
      val entry = Wire(new CgraPatch(params))
      entry.symbolIndex := patch.symbolIndex
      entry.scale := patch.scale
      entry.offset := patch.offset
      patches.write(address, entry)
      selected(patchValid, config.job) := selected(patchValid, config.job) |
        UIntToOH(index, params.packetCapacity)
    }.otherwise {
      configFailed := true.B
    }
    when(patchIndex + 1.U === config.patchCount) {
      configState := Mux(config.repeatCount =/= 0.U, ConfigState.collectRepeats, ConfigState.collectPackets)
    }.otherwise {
      patchIndex := patchIndex + 1.U
    }
  }
  when(io.repeatIn.fire) {
    val index = io.repeatIn.bits(params.packetIndexWidth - 1, 0)
    when(io.repeatIn.bits < config.packetCount && !selected(repeatMask, config.job)(index)) {
      selected(repeatMask, config.job) := selected(repeatMask, config.job) |
        UIntToOH(index, params.packetCapacity)
    }.otherwise {
      configFailed := true.B
    }
    when(repeatIndex + 1.U === config.repeatCount) {
      configState := ConfigState.collectPackets
    }.otherwise {
      repeatIndex := repeatIndex + 1.U
    }
  }
  when(io.packetIn.fire) {
    when(packetInOrder && !configFailed) {
      packets.write(captureAddress, io.packetIn.bits)
    }
    configSawLaunch := configSawLaunch || packetIsLaunch
    configSawConfig := configSawConfig || packetIsConfig
    when(packetIsLaunch) {
      configLaunchCount := configLaunchCount + 1.U
      selected(launchMask, config.job) := selected(launchMask, config.job) |
        UIntToOH(configIndex, params.packetCapacity)
    }
    val needsRepeat = packetIsLaunch || selected(patchValid, config.job)(configIndex(params.packetIndexWidth - 1, 0))
    val repeatValid = config.repeatCount === 0.U || !needsRepeat ||
      selected(repeatMask, config.job)(configIndex(params.packetIndexWidth - 1, 0))
    when(!packetInOrder || !repeatValid) {
      configFailed := true.B
    }
    when(configIndex + 1.U === config.packetCount) {
      val launchCount = configLaunchCount + packetIsLaunch
      val hasConfig = configSawConfig || packetIsConfig
      val complete = !configFailed && packetInOrder && repeatValid && launchCount =/= 0.U &&
        hasConfig
      when(complete) {
        selected(jobValid, config.job) := true.B
        selected(jobPacketCount, config.job) := config.packetCount(params.packetCountWidth - 1, 0)
        selected(jobExpectedCompletions, config.job) :=
          config.expectedCompletions(params.packetCountWidth - 1, 0)
        selected(jobWriteback, config.job) := config.writeback
        configStatus := AutoLinkStatus.Success
        configDetail := 0.U
      }.otherwise {
        configStatus := AutoLinkStatus.SinkFailure
        configDetail := CgraLinkStatus.BadPacket.U
      }
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
      Mux(config.symbolCount =/= 0.U, ConfigState.collectSymbols,
        Mux(config.patchCount =/= 0.U, ConfigState.collectPatches,
          Mux(config.repeatCount =/= 0.U, ConfigState.collectRepeats, ConfigState.collectPackets))))
  }

  when(io.autoLink.requestCopy.fire) {
    copy := io.autoLink.requestCopy.bits
    val job = io.autoLink.requestCopy.bits.job
    val valid = job < params.jobCount.U && selected(jobValid, job)
    copyDetail := Mux(valid, 0.U, CgraLinkStatus.BadConfig.U)
    execState := Mux(valid, ExecState.issueDma, ExecState.reportCopy)
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
    resultStatus := AutoLinkStatus.Success
    resultDetail := 0.U
    priorJobComplete := true.B
    residentValid := selected(repeatMask, computeJob).orR
    residentJob := computeJob
    when(publicationArmed) {
      publicationArmed := false.B
      publicationValid := true.B
      publicationStatus := AutoLinkStatus.Success
      publicationDetail := 0.U
      publicationData := data
    }
    execState := Mux(selected(jobWriteback, computeJob).enabled,
      ExecState.issueWriteback, ExecState.reportCompute)
  }

  when(io.autoLink.requestCompute.fire) {
    computeJob := io.autoLink.requestCompute.bits.job
    computeTile := io.autoLink.requestCompute.bits.tile
    slot := io.autoLink.requestCompute.bits.slot
    when(io.autoLink.requestCompute.bits.start) {
      when(computeJobValid) {
        val job = io.autoLink.requestCompute.bits.job
        val reuse = residentValid && residentJob === job && !io.invalidateResident
        val fullMask = ((1.U((params.packetCapacity + 1).W) << selected(jobPacketCount, job)) - 1.U)(params.packetCapacity - 1, 0)
        val mask = Mux(reuse, selected(launchMask, job), fullMask)
        replayMask := mask
        replayIndex := PriorityEncoder(mask)
        rearming := reuse
        expectedCompletions := selected(
          jobExpectedCompletions,
          io.autoLink.requestCompute.bits.job)
        completed := 0.U
        resultData := 0.U
        resultStatus := AutoLinkStatus.Success
        resultDetail := 0.U
        when(priorJobComplete && !reuse) {
          execState := ExecState.waitReset
        }.otherwise {
          execState := ExecState.readPacket
        }
      }.otherwise {
        resultStatus := AutoLinkStatus.SinkFailure
        resultDetail := CgraLinkStatus.BadConfig.U
        resultData := 0.U
        when(publicationArmed) {
          publicationArmed := false.B
          publicationValid := true.B
          publicationStatus := AutoLinkStatus.SourceFailure
          publicationDetail := CgraLinkStatus.BadConfig.U
          publicationData := 0.U
        }
        execState := ExecState.reportCompute
      }
    }.otherwise {
      publicationArmed := false.B
      publicationValid := false.B
    }
  }
  when(io.resetRequest.fire) {
    replayIndex := 0.U
    execState := ExecState.readPacket
  }
  when(execState === ExecState.readPacket) {
    execState := ExecState.loadPacket
  }
  when(execState === ExecState.loadPacket) {
    val symbol = selected(symbols, computeJob)(patchRead.symbolIndex(params.symbolIndexWidth - 1, 0))
    val index = Mux(symbol.source === CgraSymbolSource.TileId.U, computeTile.id, slot)
    val value = Mux(symbol.source === CgraSymbolSource.Elements.U,
      selected(elements, computeJob), symbol.base + index * symbol.stride)
    val payload = Wire(UInt(params.cgra.dataPayloadWidth.W))
    payload := value * patchRead.scale + patchRead.offset
    val lsb = params.cgra.packetLayout.dataPayloadLsb
    val mask = (((BigInt(1) << params.cgra.dataPayloadWidth) - 1) << lsb).U(params.cgra.intraPktWidth.W)
    val patched = Mux(selected(patchValid, computeJob)(replayIndex(params.packetIndexWidth - 1, 0)),
      (packetRead & ~mask) | (payload << lsb), packetRead)
    val commandLsb = params.cgra.packetLayout.cmdLsb
    val commandMask = (((BigInt(1) << params.cgra.cmdWidth) - 1) << commandLsb).U(params.cgra.intraPktWidth.W)
    // Captured launches supply the exact kernel targets and routing for REARM.
    replayPacket := Mux(rearming,
      (packetRead & ~commandMask) | (CGRACmdGenerated.CMD_REARM.U << commandLsb), patched)
    execState := ExecState.sendPacket
  }
  when(execState === ExecState.sendPacket && io.jobPacket.fire) {
    val remaining = replayMask & ~UIntToOH(replayIndex, params.packetCapacity)
    replayMask := remaining
    when(!remaining.orR) {
      when(rearming) {
        rearming := false.B
        replayMask := selected(repeatMask, computeJob)
        replayIndex := PriorityEncoder(selected(repeatMask, computeJob))
        execState := ExecState.readPacket
      }.elsewhen(completedNext === expectedCompletions) {
        finishCompute(finalResultData)
      }.otherwise {
        execState := ExecState.waitCompute
      }
    }.otherwise {
      replayIndex := PriorityEncoder(remaining)
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
  when(io.writeback.fire) {
    execState := ExecState.waitWriteback
  }
  when(io.writebackDone.fire) {
    resultStatus := io.writebackDone.bits
    execState := ExecState.reportCompute
  }
  when(io.invalidateResident) { residentValid := false.B }
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
