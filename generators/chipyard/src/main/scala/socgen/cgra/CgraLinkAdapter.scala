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

case class CgraLinkParams(auto: AutoLinkParams, cgra: CGRAParams, packetCapacity: Int) {
  require(packetCapacity > 0)

  val jobCount: Int = auto.stages.count(_.endpoint == "cgra")
  require(jobCount > 0)

  val packetCountWidth: Int = log2Ceil(packetCapacity + 1)
  val packetAddressWidth: Int = math.max(1, log2Ceil(jobCount * packetCapacity))
  val jobIndexWidth: Int = math.max(1, log2Ceil(jobCount))
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
}

class CgraLinkConfigAck(params: CgraLinkParams) extends Bundle {
  val status = UInt(AutoLinkStatus.Width.W)
  val detail = UInt(params.auto.detailWidth.W)
}

class CgraLinkDmaRequest(params: CgraLinkParams) extends Bundle {
  val sourceAddress = UInt(params.cgra.dma.dramAddrWidth.W)
  val spmWordAddress = UInt(params.cgra.dma.spmAddrWidth.W)
  val bytes = UInt(params.auto.lengthWidth.W)
  val dmaTag = UInt(params.cgra.dma.tagWidth.W)
}

class CgraLinkDmaCompletion(params: CgraLinkParams) extends Bundle {
  val dmaTag = UInt(params.cgra.dma.tagWidth.W)
}

class CgraLinkConfigAsync(params: CgraLinkParams) extends Bundle {
  private val crossing = AsyncQueueParams.singleton()
  val config = new AsyncBundle(new CgraLinkConfig(params), crossing)
  val ack = Flipped(new AsyncBundle(new CgraLinkConfigAck(params), crossing))
}

/** Translates AutoLink copy and compute requests into CGRA traffic. */
class CgraLinkAdapter(params: CgraLinkParams) extends Module {
  val io = IO(new Bundle {
    val configIn = Flipped(Decoupled(new CgraLinkConfig(params)))
    val configAck = Decoupled(new CgraLinkConfigAck(params))
    val packetIn = Flipped(Decoupled(UInt(params.cgra.intraPktWidth.W)))
    val autoLink = new AutoEndpointIO(params.auto)
    val dmaRequest = Decoupled(new CgraLinkDmaRequest(params))
    val dmaCompletion = Flipped(Decoupled(new CgraLinkDmaCompletion(params)))
    val jobPacket = Decoupled(UInt(params.cgra.intraPktWidth.W))
    val resetRequest = Decoupled(Bool())
    val computeResult = Flipped(Decoupled(UInt(params.auto.resultWidth.W)))
    val computeActive = Output(Bool())
  })

  object ConfigState {
    val idle :: collectPackets :: reportConfig :: Nil = Enum(3)
  }
  object ExecState {
    val idle :: issueDma :: waitDma :: reportCopy :: waitReset :: readPacket :: loadPacket :: sendPacket :: waitCompute :: reportCompute :: Nil = Enum(10)
  }

  val configState = RegInit(ConfigState.idle)
  val execState = RegInit(ExecState.idle)
  val config = Reg(new CgraLinkConfig(params))
  val copy = Reg(new AutoCopyRequest(params.auto))
  val packets = SyncReadMem(
    params.jobCount * params.packetCapacity,
    UInt(params.cgra.intraPktWidth.W))
  val jobValid = RegInit(VecInit(Seq.fill(params.jobCount)(false.B)))
  val jobPacketCount = Reg(Vec(params.jobCount, UInt(params.packetCountWidth.W)))
  val jobExpectedCompletions = Reg(Vec(params.jobCount, UInt(params.packetCountWidth.W)))
  val configIndex = RegInit(0.U(params.packetCountWidth.W))
  val configLaunchCount = RegInit(0.U(params.packetCountWidth.W))
  val configSawLaunch = RegInit(false.B)
  val configSawConfig = RegInit(false.B)
  val configFailed = RegInit(false.B)
  val replayIndex = RegInit(0.U(params.packetCountWidth.W))
  val replayPacket = Reg(UInt(params.cgra.intraPktWidth.W))
  val priorJobComplete = RegInit(false.B)
  val expectedCompletions = Reg(UInt(params.packetCountWidth.W))
  val completed = RegInit(0.U(params.packetCountWidth.W))
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

  val configJobValid = io.configIn.bits.job < params.jobCount.U
  val configValid = configJobValid && io.configIn.bits.packetCount =/= 0.U &&
    io.configIn.bits.packetCount <= params.packetCapacity.U &&
    io.configIn.bits.expectedCompletions =/= 0.U &&
    io.configIn.bits.expectedCompletions <= params.maxExpectedCompletions.U
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
  io.configAck.valid := configState === ConfigState.reportConfig
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
  io.dmaRequest.bits.bytes := copy.bytes
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
    configLaunchCount := 0.U
    configSawLaunch := false.B
    configSawConfig := false.B
    configFailed := false.B
    when(configValid) {
      selected(jobValid, io.configIn.bits.job) := false.B
      configStatus := AutoLinkStatus.Success
      configDetail := 0.U
      configState := ConfigState.collectPackets
    }.otherwise {
      configStatus := AutoLinkStatus.SinkFailure
      configDetail := CgraLinkStatus.BadConfig.U
      configState := ConfigState.reportConfig
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
    }
    when(!packetInOrder) {
      configFailed := true.B
    }
    when(configIndex + 1.U === config.packetCount) {
      val launchCount = configLaunchCount + packetIsLaunch
      val hasConfig = configSawConfig || packetIsConfig
      val complete = !configFailed && packetInOrder && launchCount =/= 0.U &&
        hasConfig
      when(complete) {
        selected(jobValid, config.job) := true.B
        selected(jobPacketCount, config.job) := config.packetCount(params.packetCountWidth - 1, 0)
        selected(jobExpectedCompletions, config.job) :=
          config.expectedCompletions(params.packetCountWidth - 1, 0)
        configStatus := AutoLinkStatus.Success
        configDetail := 0.U
      }.otherwise {
        configStatus := AutoLinkStatus.SinkFailure
        configDetail := CgraLinkStatus.BadPacket.U
      }
      configState := ConfigState.reportConfig
    }.otherwise {
      configIndex := configIndex + 1.U
    }
  }
  when(io.configAck.fire) {
    configState := ConfigState.idle
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
    when(publicationArmed) {
      publicationArmed := false.B
      publicationValid := true.B
      publicationStatus := AutoLinkStatus.Success
      publicationDetail := 0.U
      publicationData := data
    }
    execState := ExecState.reportCompute
  }

  when(io.autoLink.requestCompute.fire) {
    computeJob := io.autoLink.requestCompute.bits.job
    when(io.autoLink.requestCompute.bits.start) {
      when(computeJobValid) {
        replayIndex := 0.U
        expectedCompletions := selected(
          jobExpectedCompletions,
          io.autoLink.requestCompute.bits.job)
        completed := 0.U
        resultData := 0.U
        resultStatus := AutoLinkStatus.Success
        resultDetail := 0.U
        when(priorJobComplete) {
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
    replayPacket := packetRead
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
