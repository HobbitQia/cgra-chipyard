package chipyard.socgen.pool

import chisel3._
import chisel3.util._
import chipyard.socgen.link._

case class PoolLinkParams(auto: AutoLinkParams)

class PoolTileBinding(params: PoolParams, auto: AutoLinkParams) extends Module {
  val io = IO(new Bundle {
    val configured = Input(new PoolJob(params))
    val request = Input(new AutoCopyRequest(auto))
    val job = Output(new PoolJob(params))
  })

  val configured = io.configured
  val tile = io.request.tile
  val source = io.request.sourceTile
  val paddedWidth = configured.inputWidth +& configured.padWidth +& configured.padRight
  val outputWidth = (paddedWidth - configured.kernelWidth) / configured.strideWidth + 1.U

  def padding(origin: UInt, count: UInt, stride: UInt, kernel: UInt, pad: UInt, limit: UInt): (UInt, UInt) = {
    val first = (origin * stride).zext - pad.zext
    val end = first + ((count - 1.U) * stride +& kernel).zext
    val before = Mux(first < 0.S, -first, 0.S).asUInt
    val after = Mux(end > limit.zext, end - limit.zext, 0.S).asUInt
    (before, after)
  }

  val (top, bottom) = padding(tile.row, tile.rows,
    configured.strideHeight, configured.kernelHeight, configured.padHeight, configured.inputHeight)
  val (left, right) = padding(tile.column, tile.columns,
    configured.strideWidth, configured.kernelWidth, configured.padWidth, configured.inputWidth)
  val pixelBytes = configured.channels * params.elementBytes.U
  val rowBytes = outputWidth * pixelBytes
  val rowStride = Mux(configured.outputStride === 0.U, rowBytes, configured.outputStride)
  val destination = configured.destination +& tile.row * rowStride +& tile.column * pixelBytes

  io.job := configured
  io.job.source := io.request.sourceAddress
  io.job.destination := destination
  io.job.outputStride := rowStride
  io.job.inputHeight := source.rows
  io.job.inputWidth := source.columns
  io.job.padHeight := top
  io.job.padWidth := left
  io.job.padBottom := bottom
  io.job.padRight := right
}

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
    val idle :: running :: reportCopy :: waitCompute :: waitDone :: reportCompute :: Nil = Enum(6)
  }

  val autoState = RegInit(AutoState.idle)
  val copyTask = link.map(value => Reg(UInt(value.auto.dependencyWidth.W)))
  val copyStatus = RegInit(PoolStatus.Success)
  val doneStatus = RegInit(PoolStatus.Success)
  val doneSeen = RegInit(false.B)
  val watch = link.map(value => Reg(new AutoWatch(value.auto)))
  val publicationArmed = RegInit(false.B)
  val publicationValid = RegInit(false.B)
  val publicationStatus = RegInit(PoolStatus.Success)
  val computeJob = link.map(value => Reg(UInt(value.auto.jobWidth.W)))

  val idle = autoState === AutoState.idle
  val autoRequest = link.map(_.auto).map { auto =>
    val port = io.autoLink.get
    val binding = Module(new PoolTileBinding(params, auto))
    binding.io.configured := io.configuredJob
    binding.io.request := port.requestCopy.bits
    val autoJob = Wire(new PoolJob(params))
    autoJob := Mux(io.configuredJob.tiled, binding.io.job, io.configuredJob)
    autoJob.source := port.requestCopy.bits.sourceAddress

    port.watchOutput.ready := !publicationArmed && !publicationValid
    port.reportOutput.valid := publicationValid
    port.reportOutput.bits.stage := 0.U
    port.reportOutput.bits.job := watch.get.job
    port.reportOutput.bits.status := Mux(
      publicationStatus === PoolStatus.Success,
      AutoLinkStatus.Success,
      AutoLinkStatus.SourceFailure)
    port.reportOutput.bits.detail := publicationStatus
    port.reportOutput.bits.data := 0.U

    port.requestCopy.ready := idle && io.job.ready
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
    port.reportCompute.bits.stage := 0.U
    port.reportCompute.bits.job := computeJob.get
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
      copyStatus := PoolStatus.Success
      doneStatus := PoolStatus.Success
      doneSeen := false.B
      autoState := AutoState.running
    }
    when(port.reportCopy.fire) {
      autoState := AutoState.waitCompute
    }
    when(port.requestCompute.fire) {
      computeJob.get := port.requestCompute.bits.job
      when(port.requestCompute.bits.start) {
        autoState := Mux(doneSeen || io.jobDone.fire, AutoState.reportCompute, AutoState.waitDone)
      }.otherwise {
        autoState := AutoState.idle
        publicationArmed := false.B
        publicationValid := false.B
      }
    }
    when(port.reportCompute.fire) {
      autoState := AutoState.idle
      doneSeen := false.B
    }
    (port.requestCopy.valid && idle, autoJob)
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

  when(io.inputDone.fire) {
    copyStatus := io.inputDone.bits.status
    autoState := AutoState.reportCopy
  }
  when(io.jobDone.fire) {
    link.foreach { _ =>
      when(publicationArmed) {
        publicationStatus := io.jobDone.bits.status
        publicationValid := true.B
      }
    }
    doneStatus := io.jobDone.bits.status
    doneSeen := true.B
    when(autoState === AutoState.waitDone) {
      autoState := AutoState.reportCompute
    }
  }
}
