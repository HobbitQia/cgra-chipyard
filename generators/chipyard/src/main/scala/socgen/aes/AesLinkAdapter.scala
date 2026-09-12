package chipyard.socgen.aes

import chisel3._
import chisel3.util._
import chipyard.socgen.link._

case class AesLinkParams(auto: AutoLinkParams) {
  val jobCount: Int = auto.stages.count(_.endpoint == "aes")
  require(jobCount > 0)

  val jobIndexWidth: Int = math.max(1, log2Ceil(jobCount))
}

class AesLinkConfig extends Bundle {
  val job = UInt(32.W)
  val start = Bool()
  val descriptor = new _root_.aes.AesJob
}

class AesLinkAdapter(params: AesLinkParams) extends Module {
  val io = IO(new Bundle {
    val autoLink = new AutoEndpointIO(params.auto)
    val configIn = Flipped(Decoupled(new AesLinkConfig))
    val configStatus = Output(UInt(AutoLinkStatus.Width.W))
    val configDetail = Output(UInt(params.auto.detailWidth.W))
    val job = Decoupled(new _root_.aes.AesJob)
    val inputReadDone = Flipped(Decoupled(Bool()))
    val jobDone = Flipped(Decoupled(Bool()))
  })

  object Role {
    val idle :: root :: downstream :: Nil = Enum(3)
  }

  val role = RegInit(Role.idle)
  val jobs = Reg(Vec(params.jobCount, new _root_.aes.AesJob))
  val rootJob = Reg(UInt(params.auto.jobWidth.W))
  val rootPending = RegInit(false.B)
  val watch = Reg(new AutoWatch(params.auto))
  val watchArmed = RegInit(false.B)
  val outputValid = RegInit(false.B)
  val copy = Reg(new AutoCopyRequest(params.auto))
  val readDone = RegInit(false.B)
  val copyReported = RegInit(false.B)
  val done = RegInit(false.B)
  val computeAccepted = RegInit(false.B)

  def selected[T <: Data](values: Vec[T], job: UInt): T = {
    if (params.jobCount == 1) values.head else values(job(params.jobIndexWidth - 1, 0))
  }

  val idle = role === Role.idle && !rootPending && !outputValid
  val requestJob = io.autoLink.requestCopy.bits.job
  val downstreamLaunch = io.autoLink.requestCopy.valid && idle

  // Do not overwrite a descriptor while its job is waiting for the AES port.
  io.configIn.ready := idle && !io.autoLink.requestCopy.valid &&
    !io.autoLink.requestCompute.valid &&
    (!io.configIn.bits.start || watchArmed)
  io.configStatus := AutoLinkStatus.Success
  io.configDetail := 0.U

  io.autoLink.watchOutput.ready := !watchArmed && !outputValid
  io.autoLink.reportOutput.valid := outputValid
  io.autoLink.reportOutput.bits.stage := 0.U
  io.autoLink.reportOutput.bits.job := watch.job
  io.autoLink.reportOutput.bits.status := AutoLinkStatus.Success
  io.autoLink.reportOutput.bits.detail := 0.U
  io.autoLink.reportOutput.bits.data := 0.U

  val downstreamJob = Wire(new _root_.aes.AesJob)
  downstreamJob := selected(jobs, requestJob)
  downstreamJob.source.ip := io.autoLink.requestCopy.bits.sourceAddress
  downstreamJob.source.isize := io.autoLink.requestCopy.bits.bytes

  io.job.valid := rootPending || downstreamLaunch
  io.job.bits := Mux(rootPending, selected(jobs, rootJob), downstreamJob)
  io.autoLink.requestCopy.ready := idle && io.job.ready

  io.inputReadDone.ready := role === Role.root || (role === Role.downstream && !readDone)
  io.jobDone.ready := (role === Role.root && !outputValid) ||
    (role === Role.downstream && !done)

  io.autoLink.reportCopy.valid := role === Role.downstream && readDone && !copyReported
  io.autoLink.reportCopy.bits.task := copy.task
  io.autoLink.reportCopy.bits.status := AutoLinkStatus.Success
  io.autoLink.reportCopy.bits.detail := 0.U

  io.autoLink.requestCompute.ready := Mux(
    io.autoLink.requestCompute.bits.start,
    role === Role.downstream && copyReported && !computeAccepted,
    idle)
  io.autoLink.reportCompute.valid := role === Role.downstream && computeAccepted && done
  io.autoLink.reportCompute.bits.stage := 0.U
  io.autoLink.reportCompute.bits.job := copy.job
  io.autoLink.reportCompute.bits.status := AutoLinkStatus.Success
  io.autoLink.reportCompute.bits.detail := 0.U
  io.autoLink.reportCompute.bits.data := 0.U

  when(io.configIn.fire) {
    selected(jobs, io.configIn.bits.job) := io.configIn.bits.descriptor
    when(io.configIn.bits.start) {
      rootJob := io.configIn.bits.job
      rootPending := true.B
    }
  }
  when(io.autoLink.watchOutput.fire) {
    watch := io.autoLink.watchOutput.bits
    watchArmed := true.B
  }
  when(io.job.fire && rootPending) {
    role := Role.root
    rootPending := false.B
  }
  when(io.autoLink.requestCopy.fire) {
    role := Role.downstream
    copy := io.autoLink.requestCopy.bits
    readDone := false.B
    copyReported := false.B
    done := false.B
    computeAccepted := false.B
  }
  when(io.inputReadDone.fire) {
    when(role === Role.downstream) {
      readDone := true.B
    }
  }
  when(io.jobDone.fire) {
    when(role === Role.root) {
      outputValid := true.B
    }.otherwise {
      done := true.B
    }
  }
  when(io.autoLink.reportOutput.fire) {
    role := Role.idle
    watchArmed := false.B
    outputValid := false.B
  }
  when(io.autoLink.reportCopy.fire) {
    copyReported := true.B
  }
  when(io.autoLink.requestCompute.fire) {
    when(io.autoLink.requestCompute.bits.start) {
      computeAccepted := true.B
    }.otherwise {
      role := Role.idle
      watchArmed := false.B
      readDone := false.B
      copyReported := false.B
      done := false.B
      computeAccepted := false.B
    }
  }
  when(io.autoLink.reportCompute.fire) {
    role := Role.idle
    readDone := false.B
    copyReported := false.B
    done := false.B
    computeAccepted := false.B
  }
}
