package chipyard.socgen.pool

import chisel3._
import chisel3.util._
import chipyard.socgen.link._

case class PoolLinkParams(auto: AutoLinkParams)

class PoolLinkAdapter(params: PoolParams, link: Option[PoolLinkParams]) extends Module {
  val io = IO(new Bundle {
    val configuredJob = Input(new PoolJob(params))
    val job = Decoupled(new PoolJob(params))
    val inputDone = Flipped(Decoupled(new PoolEvent))
    val jobDone = Flipped(Decoupled(new PoolEvent))
    val autoLink = link.map(value => new AutoEndpointIO(value.auto))
    val active = Output(Bool())
  })

  object AutoState {
    val idle :: running :: reportCopy :: waitCompute :: reportCompute :: Nil = Enum(5)
  }

  val autoState = RegInit(AutoState.idle)
  val copyTask = link.map(value => Reg(UInt(value.auto.taskWidth.W)))
  val copyStatus = RegInit(PoolStatus.Success)
  val doneStatus = RegInit(PoolStatus.Success)
  val doneSeen = RegInit(false.B)
  val activeJob = Reg(new PoolJob(params))
  val watch = link.map(value => Reg(new AutoWatch(value.auto)))
  val publicationArmed = RegInit(false.B)
  val publicationValid = RegInit(false.B)
  val publicationStatus = RegInit(PoolStatus.Success)

  val idle = autoState === AutoState.idle
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

    port.requestCopy.ready := idle && Mux(lengthValid, io.job.ready, true.B)
    port.reportCopy.valid := autoState === AutoState.reportCopy
    port.reportCopy.bits.task := copyTask.get
    port.reportCopy.bits.status := Mux(
      copyStatus === PoolStatus.Success,
      AutoLinkStatus.Success,
      AutoLinkStatus.SinkFailure)
    port.reportCopy.bits.detail := copyStatus
    port.requestCompute.ready := Mux(
      port.requestCompute.bits.start,
      autoState === AutoState.waitCompute,
      idle || autoState === AutoState.waitCompute)
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
        autoState := AutoState.idle
      }
    }
    when(port.reportCompute.fire) {
      autoState := AutoState.idle
      doneSeen := false.B
    }
    (port.requestCopy.valid && idle && lengthValid, autoJob)
  }

  val autoJobValid = autoRequest.map(_._1).getOrElse(false.B)
  val autoJob = autoRequest.map(_._2).getOrElse(0.U.asTypeOf(new PoolJob(params)))
  val requestPending = link.map { _ =>
    io.autoLink.get.requestCopy.valid || io.autoLink.get.requestCompute.valid
  }.getOrElse(false.B)
  io.job.valid := autoJobValid
  io.job.bits := autoJob
  io.inputDone.ready := !idle
  io.jobDone.ready := !idle
  io.active := !idle || requestPending

  when(io.job.fire) {
    activeJob := io.job.bits
  }
  when(io.inputDone.fire) {
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
    doneStatus := io.jobDone.bits.status
    doneSeen := true.B
    when(autoState === AutoState.waitCompute) {
      autoState := AutoState.reportCompute
    }
  }
}
