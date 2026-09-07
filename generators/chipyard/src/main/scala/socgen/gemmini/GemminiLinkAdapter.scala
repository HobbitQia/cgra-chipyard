package chipyard.socgen.gemmini

import chisel3._
import chisel3.util._
import chipyard.socgen.link._
import freechips.rocketchip.tile.RoCCCommand
import freechips.rocketchip.tilelink.TLMessages
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams}
import org.chipsalliance.cde.config.Parameters

object GemminiLinkStatus {
  val BadAddress = 1
  val BadBeat = 2
  val BadOrder = 3
  val Denied = 4
  val Corrupt = 5
  val BadConfig = 6
}

case class GemminiLinkParams(auto: AutoLinkParams, beatBytes: Int, commandCapacity: Int, maxInflight: Int = 1) {
  require(isPow2(beatBytes))
  require(commandCapacity > 0)
  require(maxInflight > 0)

  val jobCount: Int = auto.stages.count(_.endpoint == "gemmini")
  require(jobCount > 0)

  val commandCountWidth: Int = log2Ceil(commandCapacity + 1)
  val commandAddressWidth: Int = math.max(1, log2Ceil(jobCount * commandCapacity))
  val jobIndexWidth: Int = math.max(1, log2Ceil(jobCount))
  val publicationBytes: Int = auto.dependencies.flatMap { dependency =>
    dependency.source.filter(index => auto.stage(index).endpoint == "gemmini")
      .flatMap(_ => dependency.copy.map(_.bytes))
  }.foldLeft(1)(math.max)
}

class GemminiLinkWrite(params: GemminiLinkParams) extends Bundle {
  val address = UInt(64.W)
  val source = UInt(16.W)
  val size = UInt(8.W)
  val opcode = UInt(3.W)
  val mask = UInt(params.beatBytes.W)
}

class GemminiLinkAck extends Bundle {
  val source = UInt(16.W)
  val size = UInt(8.W)
  val denied = Bool()
  val corrupt = Bool()
}

class GemminiLinkEvent(params: GemminiLinkParams) extends Bundle {
  val isAck = Bool()
  val local = Bool()
  val write = new GemminiLinkWrite(params)
  val ack = new GemminiLinkAck
}

class GemminiPublicationEntry(params: GemminiLinkParams) extends Bundle {
  val valid = Bool()
  val local = Bool()
  val source = UInt(16.W)
  val bytes = UInt(log2Ceil(params.beatBytes + 1).W)
  val size = UInt(8.W)
  val error = UInt(params.auto.detailWidth.W)
}

class GemminiLinkConfig(params: GemminiLinkParams) extends Bundle {
  val job = UInt(32.W)
  val commandCount = UInt(32.W)
}

class GemminiLinkConfigAck(params: GemminiLinkParams) extends Bundle {
  val done = Bool()
  val status = UInt(AutoLinkStatus.Width.W)
  val detail = UInt(params.auto.detailWidth.W)
}

class GemminiLinkConfigAsync(params: GemminiLinkParams) extends Bundle {
  private val crossing = AsyncQueueParams.singleton()
  val config = new AsyncBundle(new GemminiLinkConfig(params), crossing)
  val ack = Flipped(new AsyncBundle(new GemminiLinkConfigAck(params), crossing))
}

class GemminiLinkObserveAsync(params: GemminiLinkParams) extends Bundle {
  private val crossing = AsyncQueueParams.singleton()
  val event = new AsyncBundle(new GemminiLinkEvent(params), crossing)
}

/** Captures native CPU commands and replays them after AutoLink dependencies complete. */
class GemminiLinkAdapter(params: GemminiLinkParams)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val configIn = Flipped(Decoupled(new GemminiLinkConfig(params)))
    val configAck = Decoupled(new GemminiLinkConfigAck(params))
    val cpuCommand = Flipped(Decoupled(new RoCCCommand))
    val command = Decoupled(new RoCCCommand)
    val nativeBusy = Input(Bool())
    val event = Flipped(Decoupled(new GemminiLinkEvent(params)))
    val autoLink = new AutoEndpointIO(params.auto)
    val autoBusy = Output(Bool())
  })

  object ConfigState {
    val idle :: reportConfig :: collect :: Nil = Enum(3)
  }
  object ExecState {
    val idle :: reportCopy :: waitCompute :: issue :: waitComplete :: reportCompute :: Nil = Enum(6)
  }

  val configState = RegInit(ConfigState.idle)
  val execState = RegInit(ExecState.idle)
  val config = Reg(new GemminiLinkConfig(params))
  val commands = Reg(Vec(
    params.jobCount * params.commandCapacity,
    new RoCCCommand))
  val jobValid = RegInit(VecInit(Seq.fill(params.jobCount)(false.B)))
  val jobCommandCount = Reg(Vec(
    params.jobCount,
    UInt(params.commandCountWidth.W)))
  val configIndex = RegInit(0.U(params.commandCountWidth.W))
  val commandIndex = RegInit(0.U(params.commandCountWidth.W))
  val configDone = RegInit(false.B)
  val configStatus = RegInit(AutoLinkStatus.Success)
  val configDetail = RegInit(0.U(params.auto.detailWidth.W))
  val copyTask = Reg(UInt(params.auto.dependencyWidth.W))
  val copyStatus = RegInit(AutoLinkStatus.Success)
  val copyDetail = RegInit(0.U(params.auto.detailWidth.W))
  val computeResult = Reg(new AutoEvent(params.auto))
  val computeJob = RegInit(0.U(params.auto.jobWidth.W))
  val watch = Reg(new AutoWatch(params.auto))
  val armed = RegInit(false.B)
  val acknowledgedBytes = RegInit(0.U(params.auto.lengthWidth.W))
  val pending = RegInit(VecInit(Seq.fill(params.maxInflight)(
    0.U.asTypeOf(new GemminiPublicationEntry(params)))))
  val coverage = RegInit(0.U(params.publicationBytes.W))
  val outstanding = VecInit(pending.map(_.valid))
  val producedValid = RegInit(false.B)
  val producedDetail = RegInit(0.U(params.auto.detailWidth.W))
  val outputPending = RegInit(false.B)

  val configJobValid = io.configIn.bits.job < params.jobCount.U
  val configValid = configJobValid && io.configIn.bits.commandCount =/= 0.U &&
    io.configIn.bits.commandCount <= params.commandCapacity.U
  val capture = configState === ConfigState.collect
  val replay = execState === ExecState.issue
  val captureAddress = (config.job * params.commandCapacity.U + configIndex)(
    params.commandAddressWidth - 1,
    0)
  val replayAddress = (computeJob * params.commandCapacity.U + commandIndex)(
    params.commandAddressWidth - 1,
    0)
  val requestJobInRange = io.autoLink.requestCompute.bits.job < params.jobCount.U
  def selected[T <: Data](values: Vec[T], job: UInt): T = {
    if (params.jobCount == 1) values.head else values(job(params.jobIndexWidth - 1, 0))
  }
  val requestJobValid = requestJobInRange && selected(
    jobValid,
    io.autoLink.requestCompute.bits.job)

  io.configIn.ready := configState === ConfigState.idle && execState === ExecState.idle
  io.configAck.valid := configState === ConfigState.reportConfig
  io.configAck.bits.done := configDone
  io.configAck.bits.status := configStatus
  io.configAck.bits.detail := configDetail

  // CPU commands outside capture may execute before the automatic job completes.
  io.cpuCommand.ready := Mux(capture, true.B, !replay && io.command.ready)
  io.command.valid := Mux(replay, true.B, io.cpuCommand.valid && !capture)
  io.command.bits := Mux(
    replay,
    commands(replayAddress),
    io.cpuCommand.bits)
  io.event.ready := true.B
  io.autoBusy := replay || execState === ExecState.waitComplete ||
    execState === ExecState.reportCompute

  io.autoLink.watchOutput.ready := !armed && !producedValid && !outstanding.asUInt.orR
  io.autoLink.reportOutput.valid := producedValid
  io.autoLink.reportOutput.bits.stage := 0.U
  io.autoLink.reportOutput.bits.job := watch.job
  io.autoLink.reportOutput.bits.status := Mux(
    producedDetail === 0.U,
    AutoLinkStatus.Success,
    AutoLinkStatus.SourceFailure)
  io.autoLink.reportOutput.bits.detail := producedDetail
  io.autoLink.reportOutput.bits.data := 0.U
  io.autoLink.requestCopy.ready :=
    (execState === ExecState.idle || execState === ExecState.waitCompute) &&
      configState === ConfigState.idle && !io.configIn.valid
  io.autoLink.reportCopy.valid := execState === ExecState.reportCopy
  io.autoLink.reportCopy.bits.task := copyTask
  io.autoLink.reportCopy.bits.status := copyStatus
  io.autoLink.reportCopy.bits.detail := copyDetail
  io.autoLink.requestCompute.ready :=
    (execState === ExecState.idle || execState === ExecState.waitCompute) &&
      configState === ConfigState.idle && !io.configIn.valid
  io.autoLink.reportCompute.valid := execState === ExecState.reportCompute
  io.autoLink.reportCompute.bits := computeResult

  def finish(detail: UInt): Unit = {
    producedValid := true.B
    producedDetail := detail
    outputPending := false.B
    computeResult := 0.U.asTypeOf(new AutoEvent(params.auto))
    computeResult.job := watch.job
    computeResult.status := Mux(
      detail === 0.U,
      AutoLinkStatus.Success,
      AutoLinkStatus.SourceFailure)
    computeResult.detail := detail
  }

  when(io.configIn.fire) {
    config := io.configIn.bits
    configIndex := 0.U
    configDone := !configValid
    when(configJobValid) {
      selected(jobValid, io.configIn.bits.job) := false.B
    }
    when(configValid) {
      configStatus := AutoLinkStatus.Success
      configDetail := 0.U
    }.otherwise {
      configStatus := AutoLinkStatus.SinkFailure
      configDetail := GemminiLinkStatus.BadConfig.U
    }
    configState := ConfigState.reportConfig
  }
  when(io.configAck.fire) {
    configState := Mux(
      configDone,
      ConfigState.idle,
      ConfigState.collect)
  }
  when(capture && io.cpuCommand.fire) {
    commands(captureAddress) := io.cpuCommand.bits
    when(configIndex + 1.U === config.commandCount) {
      selected(jobValid, config.job) := true.B
      selected(jobCommandCount, config.job) := config.commandCount(params.commandCountWidth - 1, 0)
      configDone := true.B
      configState := ConfigState.reportConfig
    }.otherwise {
      configIndex := configIndex + 1.U
    }
  }

  when(io.autoLink.requestCopy.fire) {
    copyTask := io.autoLink.requestCopy.bits.task
    val job = io.autoLink.requestCopy.bits.job
    val valid = job < params.jobCount.U && selected(jobValid, job)
    copyStatus := Mux(
      valid,
      AutoLinkStatus.Success,
      AutoLinkStatus.SinkFailure)
    copyDetail := Mux(valid, 0.U, GemminiLinkStatus.BadConfig.U)
    execState := ExecState.reportCopy
  }
  when(io.autoLink.reportCopy.fire) {
    execState := ExecState.waitCompute
  }
  when(io.autoLink.requestCompute.fire) {
    computeJob := io.autoLink.requestCompute.bits.job
    computeResult := 0.U.asTypeOf(new AutoEvent(params.auto))
    computeResult.job := io.autoLink.requestCompute.bits.job
    computeResult.status := AutoLinkStatus.Success
    when(io.autoLink.requestCompute.bits.start) {
      when(requestJobValid) {
        commandIndex := 0.U
        execState := ExecState.issue
      }.otherwise {
        computeResult.status := AutoLinkStatus.SinkFailure
        computeResult.detail := GemminiLinkStatus.BadConfig.U
        outputPending := false.B
        when(armed) {
          producedValid := true.B
          producedDetail := GemminiLinkStatus.BadConfig.U
        }
        execState := ExecState.reportCompute
      }
    }.otherwise {
      execState := ExecState.idle
      armed := false.B
      producedValid := false.B
      outputPending := false.B
    }
  }
  when(replay && io.command.fire) {
    when(commandIndex + 1.U === selected(jobCommandCount, computeJob)) {
      execState := ExecState.waitComplete
    }.otherwise {
      commandIndex := commandIndex + 1.U
    }
  }
  when(execState === ExecState.waitComplete && !io.nativeBusy && !outputPending) {
    execState := ExecState.reportCompute
  }
  when(io.autoLink.reportCompute.fire) {
    execState := ExecState.idle
  }

  when(io.autoLink.watchOutput.fire) {
    watch := io.autoLink.watchOutput.bits
    outputPending := true.B
    armed := true.B
    acknowledgedBytes := 0.U
    coverage := 0.U
  }

  when(armed && !producedValid && io.event.fire && !io.event.bits.isAck) {
    val write = io.event.bits.write
    val firstLane = PriorityEncoder(write.mask)
    val byteCount = PopCount(write.mask)
    val beatAddress = write.address & (~(params.beatBytes - 1).U(64.W))
    val firstAddress = beatAddress + firstLane
    val offset = firstAddress - watch.address
    val addressValid = firstAddress >= watch.address && offset < watch.bytes
    val requestBytes = MuxLookup(write.size, 0.U(log2Ceil(params.beatBytes + 1).W))(
      (0 to log2Ceil(params.beatBytes)).map(size => size.U -> (1 << size).U))
    val laneOffset = write.address & (params.beatBytes - 1).U
    val requestMask = VecInit((0 until params.beatBytes).map(lane =>
      lane.U >= laneOffset && lane.U < laneOffset + requestBytes)).asUInt
    val shiftedMask = write.mask >> firstLane
    val contiguous = write.mask.orR && (shiftedMask & (shiftedMask +& 1.U)) === 0.U
    val requestValid = requestBytes =/= 0.U && (write.address & (requestBytes - 1.U)) === 0.U
    val opcodeValid = (write.opcode === TLMessages.PutFullData && write.mask === requestMask) ||
      write.opcode === TLMessages.PutPartialData
    val shapeValid = requestValid && opcodeValid && contiguous && (write.mask & ~requestMask) === 0.U
    val endOffset = offset +& byteCount
    val withinPublication = endOffset <= watch.bytes
    val written = VecInit((0 until params.publicationBytes).map(byte =>
      byte.U >= offset && byte.U < endOffset)).asUInt
    val overlap = (written & coverage).orR
    val matches = VecInit(pending.map(entry => entry.valid &&
      entry.local === io.event.bits.local && entry.source === write.source))
    val free = VecInit(pending.map(entry => !entry.valid))
    val slot = PriorityEncoder(free.asUInt)
    val error = Mux(!addressValid, GemminiLinkStatus.BadAddress.U,
      Mux(!shapeValid, GemminiLinkStatus.BadBeat.U,
        Mux(!withinPublication || overlap, GemminiLinkStatus.BadOrder.U, 0.U)))

    when(matches.asUInt.orR) {
      pending(PriorityEncoder(matches.asUInt)).error := GemminiLinkStatus.BadOrder.U
    }.elsewhen(!free.asUInt.orR) {
      finish(GemminiLinkStatus.BadOrder.U)
    }.otherwise {
      pending(slot).valid := true.B
      pending(slot).local := io.event.bits.local
      pending(slot).source := write.source
      pending(slot).bytes := byteCount
      pending(slot).size := write.size
      pending(slot).error := error
      when(error === 0.U) {
        coverage := coverage | written
      }
    }
  }

  when(io.event.fire && io.event.bits.isAck) {
    val ack = io.event.bits.ack
    val matches = VecInit(pending.map(entry => entry.valid &&
      entry.local === io.event.bits.local && entry.source === ack.source))
    val slot = PriorityEncoder(matches.asUInt)
    val entry = pending(slot)
    when(matches.asUInt.orR) {
      pending(slot).valid := false.B
    }
    when(armed && !producedValid) {
      val detail = Mux(
        ack.denied,
        GemminiLinkStatus.Denied.U,
        Mux(
          ack.corrupt,
          GemminiLinkStatus.Corrupt.U,
          Mux(ack.size =/= entry.size, GemminiLinkStatus.BadBeat.U, entry.error)))
      val nextBytes = acknowledgedBytes + entry.bytes
      when(!matches.asUInt.orR) {
        finish(GemminiLinkStatus.BadOrder.U)
      }.elsewhen(detail =/= 0.U) {
        finish(detail)
      }.otherwise {
        acknowledgedBytes := nextBytes
        when(nextBytes === watch.bytes && PopCount(outstanding) === 1.U) {
          finish(0.U)
        }
      }
    }
  }

  when(io.autoLink.reportOutput.fire) {
    armed := false.B
    producedValid := false.B
  }
}
