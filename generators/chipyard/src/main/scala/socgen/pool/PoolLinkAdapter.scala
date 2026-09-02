package chipyard.socgen.pool

import chisel3._
import chisel3.util._
import chipyard.socgen.link._

case class PoolLinkParams(auto: AutoLinkParams)

class PoolLinkAdapter(params: PoolParams, link: Option[PoolLinkParams]) extends Module {
  val io = IO(new Bundle {
    val configuredJob = Input(new PoolJob(params))
    val manualStart = Flipped(Decoupled(new PoolJob(params)))
    val manualDone = Decoupled(new PoolEvent)
    val job = Decoupled(new PoolJob(params))
    val inputDone = Flipped(Decoupled(new PoolEvent))
    val jobDone = Flipped(Decoupled(new PoolEvent))
    val autoLink = link.map(value => new AutoEndpointIO(value.auto))
    val active = Output(Bool())
  })

  object Role {
    val idle :: manual :: automatic :: Nil = Enum(3)
  }
  object AutoState {
    val idle :: running :: reportCopy :: waitCompute :: reportCompute :: Nil = Enum(5)
  }

  val role = RegInit(Role.idle)
  val autoState = RegInit(AutoState.idle)
  val manualResult = Reg(new PoolEvent)
  val manualResultValid = RegInit(false.B)
  val copyTask = link.map(value => Reg(UInt(value.auto.taskWidth.W)))
  val copyStatus = RegInit(PoolStatus.Success)
  val doneStatus = RegInit(PoolStatus.Success)
  val doneSeen = RegInit(false.B)
  val activeJob = Reg(new PoolJob(params))
  val watch = link.map(value => Reg(new AutoWatch(value.auto)))
  val publicationArmed = RegInit(false.B)
  val publicationValid = RegInit(false.B)
  val publicationStatus = RegInit(PoolStatus.Success)

  val idle = role === Role.idle && !manualResultValid
  val autoRequest = link.map(_.auto).map { auto =>
    val port = io.autoLink.get
    val expectedBytes = io.configuredJob.inputHeight * io.configuredJob.inputWidth *
      io.configuredJob.channels * params.elementBytes.U
    val lengthValid = port.requestCopy.bits.bytes === expectedBytes
    val autoJob = Wire(new PoolJob(params))
    autoJob := io.configuredJob
    autoJob.source := port.requestCopy.bits.sourceAddress

    port.watchOutput.ready := !publicationArmed && !publicationValid
    port.reportOutput.valid := publicationValid
    port.reportOutput.bits.status := Mux(
      publicationStatus === PoolStatus.Success,
      AutoLinkStatus.Success,
      AutoLinkStatus.SourceFailure)
    port.reportOutput.bits.detail := publicationStatus
    port.reportOutput.bits.data := 0.U

    port.requestCopy.ready := idle && Mux(lengthValid, io.job.ready, true.B) &&
      !io.manualStart.valid
    port.reportCopy.valid := autoState === AutoState.reportCopy
    port.reportCopy.bits.task := copyTask.get
    port.reportCopy.bits.status := Mux(
      copyStatus === PoolStatus.Success,
      AutoLinkStatus.Success,
      AutoLinkStatus.SinkFailure)
    port.reportCopy.bits.detail := copyStatus
    port.requestCompute.ready := autoState === AutoState.waitCompute
    port.reportCompute.valid := autoState === AutoState.reportCompute
    port.reportCompute.bits.status := Mux(
      doneStatus === PoolStatus.Success,
      AutoLinkStatus.Success,
      AutoLinkStatus.SinkFailure)
    port.reportCompute.bits.detail := doneStatus
    port.reportCompute.bits.data := 0.U

    when(port.watchOutput.fire) {
      watch.get := port.watchOutput.bits
      publicationArmed := true.B
    }
    when(port.reportOutput.fire) {
      publicationArmed := false.B
      publicationValid := false.B
    }
    when(port.requestCopy.fire) {
      role := Role.automatic
      copyTask.get := port.requestCopy.bits.task
      copyStatus := Mux(lengthValid, PoolStatus.Success, PoolStatus.BadLength)
      doneStatus := PoolStatus.Success
      doneSeen := false.B
      autoState := Mux(lengthValid, AutoState.running, AutoState.reportCopy)
    }
    when(port.reportCopy.fire) {
      autoState := AutoState.waitCompute
    }
    when(port.requestCompute.fire) {
      when(port.requestCompute.bits.start) {
        autoState := Mux(doneSeen, AutoState.reportCompute, AutoState.waitCompute)
      }.otherwise {
        role := Role.idle
        autoState := AutoState.idle
      }
    }
    when(port.reportCompute.fire) {
      role := Role.idle
      autoState := AutoState.idle
      doneSeen := false.B
    }
    (port.requestCopy.valid && idle && lengthValid && !io.manualStart.valid, autoJob)
  }

  val autoJobValid = autoRequest.map(_._1).getOrElse(false.B)
  val autoJob = autoRequest.map(_._2).getOrElse(0.U.asTypeOf(new PoolJob(params)))
  io.job.valid := autoJobValid || (io.manualStart.valid && idle)
  io.job.bits := Mux(autoJobValid, autoJob, io.manualStart.bits)
  io.manualStart.ready := io.job.ready && idle && !autoJobValid
  io.manualDone.valid := manualResultValid
  io.manualDone.bits := manualResult
  io.inputDone.ready := role =/= Role.idle
  io.jobDone.ready := role =/= Role.idle
  io.active := role =/= Role.idle || manualResultValid

  when(io.manualStart.fire) {
    role := Role.manual
  }
  when(io.job.fire) {
    activeJob := io.job.bits
  }
  when(io.inputDone.fire && role === Role.automatic) {
    copyStatus := io.inputDone.bits.status
    autoState := AutoState.reportCopy
  }
  when(io.jobDone.fire) {
    link.foreach { _ =>
      when(publicationArmed) {
        val paddedHeight = activeJob.inputHeight + (activeJob.padHeight << 1)
        val paddedWidth = activeJob.inputWidth + (activeJob.padWidth << 1)
        val outputHeight = (paddedHeight - activeJob.kernelHeight) / activeJob.strideHeight + 1.U
        val outputWidth = (paddedWidth - activeJob.kernelWidth) / activeJob.strideWidth + 1.U
        val outputBytes = outputHeight * outputWidth * activeJob.channels * params.elementBytes.U
        val addressValid = activeJob.destination === watch.get.address
        val lengthValid = outputBytes === watch.get.bytes
        publicationStatus := Mux(
          io.jobDone.bits.status =/= PoolStatus.Success,
          io.jobDone.bits.status,
          Mux(!addressValid, PoolStatus.BadAddress,
            Mux(!lengthValid, PoolStatus.BadLength, PoolStatus.Success)))
        publicationValid := true.B
      }
    }
    when(role === Role.manual) {
      manualResult := io.jobDone.bits
      manualResultValid := true.B
    }.elsewhen(role === Role.automatic) {
      doneStatus := io.jobDone.bits.status
      doneSeen := true.B
      when(autoState === AutoState.waitCompute) {
        autoState := AutoState.reportCompute
      }
    }
  }
  when(io.manualDone.fire) {
    manualResultValid := false.B
    role := Role.idle
  }
}
