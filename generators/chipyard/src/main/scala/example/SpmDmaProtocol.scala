package chipyard.example

import chisel3._
import chisel3.util._

object SpmDmaMode {
  val Width = 1
  val Auto = 0.U(Width.W)
  val Manual = 1.U(Width.W)
}

object SpmDmaStage {
  val Width = 2
  val Producer = 0.U(Width.W)
  val Transfer = 1.U(Width.W)
  val Consumer = 2.U(Width.W)
}

object SpmDmaStatus {
  val Width = 4
  val Success = 0.U(Width.W)
  val InvalidJob = 1.U(Width.W)
  val InvalidSlot = 2.U(Width.W)
  val InvalidLength = 3.U(Width.W)
  val IdentityMismatch = 4.U(Width.W)
  val StageFailure = 5.U(Width.W)
}

case class SpmDmaParams(
  slotCount: Int,
  slotSizeBytes: Int,
  beatBytes: Int,
  jobIdWidth: Int = 32,
  lengthWidth: Int = 32,
  detailWidth: Int = 8,
  resultWidth: Int = 32) {
  require(slotCount > 0)
  require(slotSizeBytes > 0)
  require(isPow2(beatBytes))
  require(slotSizeBytes % beatBytes == 0)

  val slotWidth: Int = math.max(1, log2Ceil(slotCount))
}

class SpmDmaJob(params: SpmDmaParams) extends Bundle {
  val jobId = UInt(params.jobIdWidth.W)
  val slot = UInt(params.slotWidth.W)
  val bytes = UInt(params.lengthWidth.W)
  val mode = UInt(SpmDmaMode.Width.W)
}

class SpmDmaCommand(params: SpmDmaParams) extends Bundle {
  val jobId = UInt(params.jobIdWidth.W)
  val slot = UInt(params.slotWidth.W)
  val bytes = UInt(params.lengthWidth.W)
}

class SpmDmaResult(params: SpmDmaParams) extends Bundle {
  val jobId = UInt(params.jobIdWidth.W)
  val slot = UInt(params.slotWidth.W)
  val bytes = UInt(params.lengthWidth.W)
  val stage = UInt(SpmDmaStage.Width.W)
  val status = UInt(SpmDmaStatus.Width.W)
  val detail = UInt(params.detailWidth.W)
  val data = UInt(params.resultWidth.W)
}

/** Orders producer publication, SPM transfer, and consumer execution. IP
  * adapters only execute stage commands and return stage results.
  */
class SpmDmaController(params: SpmDmaParams) extends Module {
  val io = IO(new Bundle {
    val jobIn = Flipped(Decoupled(new SpmDmaJob(params)))
    val stepIn = Flipped(Decoupled(new SpmDmaCommand(params)))
    val producerStart = Decoupled(new SpmDmaCommand(params))
    val producerDone = Flipped(Decoupled(new SpmDmaResult(params)))
    val transferStart = Decoupled(new SpmDmaCommand(params))
    val transferDone = Flipped(Decoupled(new SpmDmaResult(params)))
    val consumerStart = Decoupled(new SpmDmaCommand(params))
    val consumerDone = Flipped(Decoupled(new SpmDmaResult(params)))
    val resultOut = Decoupled(new SpmDmaResult(params))
    val active = Output(Bool())
  })

  val Seq(idle, waitProducer, waitTransferStep, sendTransfer, waitTransfer, waitConsumerStep, sendConsumer, waitConsumer) = Enum(8)
  val state = RegInit(idle)
  val job = Reg(new SpmDmaJob(params))
  val resultValid = RegInit(false.B)
  val result = Reg(new SpmDmaResult(params))

  def setCommand(out: DecoupledIO[SpmDmaCommand]): Unit = {
    out.bits.jobId := job.jobId
    out.bits.slot := job.slot
    out.bits.bytes := job.bytes
  }

  def sameJob(candidate: SpmDmaResult): Bool = {
    candidate.jobId === job.jobId &&
      candidate.slot === job.slot &&
      candidate.bytes === job.bytes
  }

  def finish(stage: UInt, status: UInt, detail: UInt, data: UInt): Unit = {
    state := idle
    resultValid := true.B
    result.jobId := job.jobId
    result.slot := job.slot
    result.bytes := job.bytes
    result.stage := stage
    result.status := status
    result.detail := detail
    result.data := data
  }

  val jobValid = io.jobIn.bits.jobId =/= 0.U &&
    io.jobIn.bits.slot < params.slotCount.U &&
    io.jobIn.bits.bytes =/= 0.U &&
    io.jobIn.bits.bytes <= params.slotSizeBytes.U &&
    (io.jobIn.bits.bytes & (params.beatBytes - 1).U) === 0.U

  io.producerStart.valid := state === idle && !resultValid && io.jobIn.valid && jobValid
  io.producerStart.bits.jobId := io.jobIn.bits.jobId
  io.producerStart.bits.slot := io.jobIn.bits.slot
  io.producerStart.bits.bytes := io.jobIn.bits.bytes
  io.jobIn.ready := state === idle && !resultValid && Mux(jobValid, io.producerStart.ready, true.B)

  io.transferStart.valid := state === sendTransfer
  setCommand(io.transferStart)
  io.consumerStart.valid := state === sendConsumer
  setCommand(io.consumerStart)

  val waitingForStep = state === waitTransferStep || state === waitConsumerStep
  val stepMatches = io.stepIn.bits.jobId === job.jobId &&
    io.stepIn.bits.slot === job.slot && io.stepIn.bits.bytes === job.bytes
  io.stepIn.ready := waitingForStep

  io.producerDone.ready := state === waitProducer
  io.transferDone.ready := state === waitTransfer
  io.consumerDone.ready := state === waitConsumer
  io.resultOut.valid := resultValid
  io.resultOut.bits := result
  io.active := state =/= idle || resultValid

  when(io.jobIn.fire) {
    job := io.jobIn.bits
    when(jobValid) {
      state := waitProducer
    }.otherwise {
      resultValid := true.B
      result.jobId := io.jobIn.bits.jobId
      result.slot := io.jobIn.bits.slot
      result.bytes := io.jobIn.bits.bytes
      result.stage := SpmDmaStage.Producer
      result.status := Mux(
        io.jobIn.bits.jobId === 0.U,
        SpmDmaStatus.InvalidJob,
        Mux(io.jobIn.bits.slot >= params.slotCount.U, SpmDmaStatus.InvalidSlot, SpmDmaStatus.InvalidLength))
      result.detail := 0.U
      result.data := 0.U
    }
  }

  when(io.producerDone.fire) {
    when(!sameJob(io.producerDone.bits)) {
      finish(SpmDmaStage.Producer, SpmDmaStatus.IdentityMismatch, 0.U, 0.U)
    }.elsewhen(io.producerDone.bits.status =/= SpmDmaStatus.Success) {
      finish(SpmDmaStage.Producer, SpmDmaStatus.StageFailure, io.producerDone.bits.detail, 0.U)
    }.otherwise {
      state := Mux(job.mode === SpmDmaMode.Auto, sendTransfer, waitTransferStep)
    }
  }

  when(io.stepIn.fire) {
    when(!stepMatches) {
      finish(Mux(state === waitTransferStep, SpmDmaStage.Transfer, SpmDmaStage.Consumer), SpmDmaStatus.IdentityMismatch, 0.U, 0.U)
    }.elsewhen(state === waitTransferStep) {
      state := sendTransfer
    }.otherwise {
      state := sendConsumer
    }
  }

  when(io.transferStart.fire) {
    state := waitTransfer
  }

  when(io.transferDone.fire) {
    when(!sameJob(io.transferDone.bits)) {
      finish(SpmDmaStage.Transfer, SpmDmaStatus.IdentityMismatch, 0.U, 0.U)
    }.elsewhen(io.transferDone.bits.status =/= SpmDmaStatus.Success) {
      finish(SpmDmaStage.Transfer, SpmDmaStatus.StageFailure, io.transferDone.bits.detail, 0.U)
    }.otherwise {
      state := Mux(job.mode === SpmDmaMode.Auto, sendConsumer, waitConsumerStep)
    }
  }

  when(io.consumerStart.fire) {
    state := waitConsumer
  }

  when(io.consumerDone.fire) {
    when(!sameJob(io.consumerDone.bits)) {
      finish(SpmDmaStage.Consumer, SpmDmaStatus.IdentityMismatch, 0.U, 0.U)
    }.elsewhen(io.consumerDone.bits.status =/= SpmDmaStatus.Success) {
      finish(SpmDmaStage.Consumer, SpmDmaStatus.StageFailure, io.consumerDone.bits.detail, io.consumerDone.bits.data)
    }.otherwise {
      finish(SpmDmaStage.Consumer, SpmDmaStatus.Success, io.consumerDone.bits.detail, io.consumerDone.bits.data)
    }
  }

  when(io.resultOut.fire) {
    resultValid := false.B
    state := idle
  }
}
