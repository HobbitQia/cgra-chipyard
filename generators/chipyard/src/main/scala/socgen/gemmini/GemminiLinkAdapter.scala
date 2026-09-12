package chipyard.socgen.gemmini

import chisel3._
import chisel3.util._
import chipyard.socgen.link._
import freechips.rocketchip.tile.RoCCCommand
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams}
import org.chipsalliance.cde.config.Parameters

object GemminiLinkStatus {
  val BadAddress = 1
  val BadBeat = 2
  val BadOrder = 3
  val Denied = 4
  val Corrupt = 5
}

case class GemminiLinkParams(auto: AutoLinkParams, beatBytes: Int, commandCapacity: Int, maxInflight: Int = 1, patchCapacity: Int = 32) {
  require(isPow2(beatBytes))
  require(commandCapacity > 0)
  require(maxInflight > 0)
  require(isPow2(patchCapacity) && patchCapacity > 1)

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

class GemminiPublicationControl(params: GemminiLinkParams) extends Bundle {
  val enable = Bool()
  val watch = new AutoWatch(params.auto)
}

class GemminiPublicationReply(params: GemminiLinkParams) extends Bundle {
  val result = Bool()
  val detail = UInt(params.auto.detailWidth.W)
}

class GemminiLinkConfig(params: GemminiLinkParams) extends Bundle {
  val job = UInt(32.W)
  val commandCount = UInt(32.W)
  val patchCount = UInt(32.W)
  val window = new GemminiWindow
}

class GemminiLinkConfigAck(params: GemminiLinkParams) extends Bundle {
  val done = Bool()
  val status = UInt(AutoLinkStatus.Width.W)
  val detail = UInt(params.auto.detailWidth.W)
}

class GemminiLinkConfigAsync(params: GemminiLinkParams) extends Bundle {
  private val crossing = AsyncQueueParams.singleton()
  val config = new AsyncBundle(new GemminiLinkConfig(params), crossing)
  val patch = new AsyncBundle(new GemminiPatchEntry, crossing)
  val ack = Flipped(new AsyncBundle(new GemminiLinkConfigAck(params), crossing))
}

class GemminiLinkObserveAsync(params: GemminiLinkParams) extends Bundle {
  private val crossing = AsyncQueueParams.singleton()
  val control = Flipped(new AsyncBundle(new GemminiPublicationControl(params), crossing))
  val reply = new AsyncBundle(new GemminiPublicationReply(params), crossing)
}

/** Captures native CPU commands and replays them after AutoLink dependencies complete. */
class GemminiLinkAdapter(params: GemminiLinkParams)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val configIn = Flipped(Decoupled(new GemminiLinkConfig(params)))
    val configAck = Decoupled(new GemminiLinkConfigAck(params))
    val patchIn = Flipped(Decoupled(new GemminiPatchEntry))
    val cpuCommand = Flipped(Decoupled(new RoCCCommand))
    val command = Decoupled(new RoCCCommand)
    val nativeBusy = Input(Bool())
    val publication = Decoupled(new GemminiPublicationControl(params))
    val publicationReply = Flipped(Decoupled(new GemminiPublicationReply(params)))
    val autoLink = new AutoEndpointIO(params.auto)
    val autoBusy = Output(Bool())
  })

  object ConfigState {
    val idle :: reportConfig :: collectPatches :: collect :: Nil = Enum(4)
  }
  object ExecState {
    val idle :: reportCopy :: waitCompute :: bind :: issue :: waitComplete :: reportCompute :: Nil = Enum(7)
  }

  val configState = RegInit(ConfigState.idle)
  val execState = RegInit(ExecState.idle)
  val config = Reg(new GemminiLinkConfig(params))
  val commands = Reg(Vec(
    params.jobCount * params.commandCapacity,
    new RoCCCommand))
  val jobCommandCount = Reg(Vec(
    params.jobCount,
    UInt(params.commandCountWidth.W)))
  val configIndex = RegInit(0.U(params.commandCountWidth.W))
  val commandIndex = RegInit(0.U(params.commandCountWidth.W))
  val configDone = RegInit(false.B)
  val copyTask = Reg(UInt(params.auto.dependencyWidth.W))
  val computeResult = Reg(new AutoEvent(params.auto))
  val computeJob = RegInit(0.U(params.auto.jobWidth.W))
  val watch = Reg(new AutoWatch(params.auto))
  val armed = RegInit(false.B)
  val publicationSend = RegInit(false.B)
  val publicationPending = RegInit(false.B)
  val publicationEnable = RegInit(false.B)
  val producedValid = RegInit(false.B)
  val producedDetail = RegInit(0.U(params.auto.detailWidth.W))
  val outputPending = RegInit(false.B)

  val capture = configState === ConfigState.collect
  val replay = execState === ExecState.issue
  val captureAddress = (config.job * params.commandCapacity.U + configIndex)(
    params.commandAddressWidth - 1,
    0)
  val replayAddress = (computeJob * params.commandCapacity.U + commandIndex)(
    params.commandAddressWidth - 1,
    0)
  def selected[T <: Data](values: Vec[T], job: UInt): T = {
    if (params.jobCount == 1) values.head else values(job(params.jobIndexWidth - 1, 0))
  }
  val binding = Module(new GemminiPatch(params))
  binding.io.begin.valid := io.configIn.fire
  binding.io.begin.bits := io.configIn.bits
  binding.io.patch.valid := io.patchIn.valid && configState === ConfigState.collectPatches
  binding.io.patch.bits := io.patchIn.bits
  io.patchIn.ready := binding.io.patch.ready && configState === ConfigState.collectPatches
  binding.io.copy.valid := io.autoLink.requestCopy.fire
  binding.io.copy.bits := io.autoLink.requestCopy.bits
  binding.io.request := io.autoLink.requestCompute.bits
  binding.io.start := io.autoLink.requestCompute.fire && io.autoLink.requestCompute.bits.start
  binding.io.watch := watch
  binding.io.job := computeJob
  binding.io.index := commandIndex
  binding.io.command := commands(replayAddress)

  io.configIn.ready := configState === ConfigState.idle && execState === ExecState.idle
  io.configAck.valid := configState === ConfigState.reportConfig
  io.configAck.bits.done := configDone
  io.configAck.bits.status := AutoLinkStatus.Success
  io.configAck.bits.detail := 0.U

  // CPU commands outside capture may execute before the automatic job completes.
  val armWait = publicationPending || io.autoLink.watchOutput.valid
  val configWait = (configState =/= ConfigState.idle && !capture) || execState === ExecState.bind
  io.cpuCommand.ready := Mux(capture, true.B, !replay && !armWait && !configWait && io.command.ready)
  io.command.valid := Mux(replay, true.B, io.cpuCommand.valid && !capture && !armWait && !configWait)
  io.command.bits := Mux(
    replay,
    binding.io.patched,
    io.cpuCommand.bits)
  io.publication.valid := publicationSend
  io.publication.bits.enable := publicationEnable
  io.publication.bits.watch := watch
  io.publicationReply.ready := !producedValid || !publicationEnable || !io.publicationReply.bits.result
  io.autoBusy := publicationPending || execState === ExecState.bind || replay || execState === ExecState.waitComplete ||
    execState === ExecState.reportCompute

  io.autoLink.watchOutput.ready := !armed && !producedValid && !publicationPending
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
  io.autoLink.reportCopy.bits.status := AutoLinkStatus.Success
  io.autoLink.reportCopy.bits.detail := 0.U
  io.autoLink.requestCompute.ready :=
    (execState === ExecState.idle || execState === ExecState.waitCompute) &&
      configState === ConfigState.idle && !io.configIn.valid && !io.autoLink.watchOutput.fire &&
      !io.autoLink.requestCopy.fire && !publicationPending
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
    configDone := false.B
    configState := ConfigState.reportConfig
  }
  when(io.configAck.fire) {
    configState := Mux(
      configDone,
      ConfigState.idle,
      Mux(config.patchCount =/= 0.U, ConfigState.collectPatches, ConfigState.collect))
  }
  when(configState === ConfigState.collectPatches && binding.io.configured) {
    configState := ConfigState.collect
  }
  when(capture && io.cpuCommand.fire) {
    commands(captureAddress) := io.cpuCommand.bits
    when(configIndex + 1.U === config.commandCount) {
      selected(jobCommandCount, config.job) := config.commandCount(params.commandCountWidth - 1, 0)
      configDone := true.B
      configState := ConfigState.reportConfig
    }.otherwise {
      configIndex := configIndex + 1.U
    }
  }
  when(io.autoLink.requestCopy.fire) {
    copyTask := io.autoLink.requestCopy.bits.task
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
      commandIndex := 0.U
      execState := ExecState.bind
    }.otherwise {
      execState := ExecState.idle
      armed := false.B
      producedValid := false.B
      outputPending := false.B
      publicationSend := true.B
      publicationPending := true.B
      publicationEnable := false.B
    }
  }
  when(execState === ExecState.bind && binding.io.ready) {
    execState := ExecState.issue
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
    publicationSend := true.B
    publicationPending := true.B
    publicationEnable := true.B
  }
  when(io.publication.fire) {
    publicationSend := false.B
  }
  when(io.publicationReply.fire) {
    when(io.publicationReply.bits.result) {
      val abort = io.autoLink.requestCompute.fire && !io.autoLink.requestCompute.bits.start
      when(publicationEnable && !abort) {
        finish(io.publicationReply.bits.detail)
      }
    }.otherwise {
      publicationPending := false.B
    }
  }

  when(io.autoLink.reportOutput.fire) {
    armed := false.B
    producedValid := false.B
  }
}
