package chipyard.example

import chisel3._
import chisel3.util._

class CgraTransferPullSubmission extends Bundle {
  val jobId = UInt(32.W)
  val slot = UInt(32.W)
  val bytes = UInt(32.W)
  val spmWordAddress = UInt(32.W)
  val dmaTag = UInt(32.W)
}

class CgraTransferLaunchSubmission extends Bundle {
  val jobId = UInt(32.W)
  val slot = UInt(32.W)
  val bytes = UInt(32.W)
  val spmWordAddress = UInt(32.W)
  val dmaTag = UInt(32.W)
  val packetCount = UInt(32.W)
}

class CgraTransferControlProtocolError extends Bundle {
  val jobId = UInt(32.W)
  val slot = UInt(32.W)
  val requestedBytes = UInt(32.W)
  val actualBytes = UInt(32.W)
  val spmWordAddress = UInt(32.W)
  val dmaTag = UInt(32.W)
  val operation = UInt(32.W)
  val reason = UInt(32.W)
}

/** Bounded typed queues behind the production MMIO register contract. The
  * block carries control metadata and complete launch packets only; it has no
  * payload-data port and never waits for DMA or compute execution.
  */
class CgraTransferControlQueues(params: CgraComputeLaunchGateParams)
    extends Module {
  private val consumerParams = CgraConsumerPullAdapterParams.production
  require(params.spmAddressWidth == consumerParams.spmAddressWidth)
  require(params.dmaTagWidth == consumerParams.dmaTagWidth)

  val io = IO(new Bundle {
    val descriptorSubmit = Flipped(
      Decoupled(new CgraTransferPullSubmission))
    val descriptorOut = Decoupled(
      new CgraConsumerPullDescriptor(consumerParams))
    val launchHeaderSubmit = Flipped(
      Decoupled(new CgraTransferLaunchSubmission))
    val launchHeaderOut = Decoupled(new CgraLaunchSequenceHeader(params))
    val launchPacketSubmit = Flipped(
      Decoupled(new CgraLaunchPacket(params)))
    val launchPacketOut = Decoupled(new CgraLaunchPacket(params))

    val launchResultIn = Flipped(Decoupled(new CgraLaunchResult(params)))
    val launchResultOut = Decoupled(new CgraLaunchResult(params))
    val launchErrorIn = Flipped(
      Decoupled(new CgraLaunchProtocolError(params)))
    val launchErrorOut = Decoupled(
      new CgraTransferControlProtocolError)
    val computeCompletionIn = Flipped(
      Decoupled(new CgraComputeCompletion(params)))
    val computeCompletionOut = Decoupled(new CgraComputeCompletion(params))
    val computeErrorIn = Flipped(Decoupled(
      new CgraComputeCompletionProtocolError(params)))
    val computeErrorOut = Decoupled(
      new CgraComputeCompletionProtocolError(params))
  })

  private def connectQueue[T <: Data](
    input: DecoupledIO[T],
    output: DecoupledIO[T],
    entries: Int): Unit = {
    val queue = Module(new Queue(chiselTypeOf(input.bits), entries))
    queue.io.enq <> input
    output <> queue.io.deq
  }

  private def upperBitsZero(value: UInt, retainedWidth: Int): Bool = {
    require(retainedWidth > 0 && retainedWidth <= value.getWidth)
    if (retainedWidth == value.getWidth) true.B
    else !value(value.getWidth - 1, retainedWidth).orR
  }

  val descriptorQueue = Module(new Queue(
    new CgraConsumerPullDescriptor(consumerParams), 1))
  val descriptorFieldsValid =
    upperBitsZero(io.descriptorSubmit.bits.spmWordAddress,
      consumerParams.spmAddressWidth) &&
      upperBitsZero(io.descriptorSubmit.bits.dmaTag,
        consumerParams.dmaTagWidth)
  descriptorQueue.io.enq.valid := io.descriptorSubmit.valid &&
    descriptorFieldsValid
  descriptorQueue.io.enq.bits.jobId := io.descriptorSubmit.bits.jobId
  descriptorQueue.io.enq.bits.slot := io.descriptorSubmit.bits.slot
  descriptorQueue.io.enq.bits.bytes := io.descriptorSubmit.bits.bytes
  descriptorQueue.io.enq.bits.spmWordAddress :=
    io.descriptorSubmit.bits.spmWordAddress(
      consumerParams.spmAddressWidth - 1, 0)
  descriptorQueue.io.enq.bits.dmaTag :=
    io.descriptorSubmit.bits.dmaTag(consumerParams.dmaTagWidth - 1, 0)
  io.descriptorOut <> descriptorQueue.io.deq

  val launchHeaderQueue = Module(new Queue(
    new CgraLaunchSequenceHeader(params), 1))
  val launchHeaderFieldsValid =
    upperBitsZero(io.launchHeaderSubmit.bits.spmWordAddress,
      params.spmAddressWidth) &&
      upperBitsZero(io.launchHeaderSubmit.bits.dmaTag,
        params.dmaTagWidth) &&
      io.launchHeaderSubmit.bits.packetCount =/= 0.U &&
      io.launchHeaderSubmit.bits.packetCount <= params.packetCapacity.U
  launchHeaderQueue.io.enq.valid := io.launchHeaderSubmit.valid &&
    launchHeaderFieldsValid
  launchHeaderQueue.io.enq.bits.jobId := io.launchHeaderSubmit.bits.jobId
  launchHeaderQueue.io.enq.bits.slot := io.launchHeaderSubmit.bits.slot
  launchHeaderQueue.io.enq.bits.bytes := io.launchHeaderSubmit.bits.bytes
  launchHeaderQueue.io.enq.bits.spmWordAddress :=
    io.launchHeaderSubmit.bits.spmWordAddress(params.spmAddressWidth - 1, 0)
  launchHeaderQueue.io.enq.bits.dmaTag :=
    io.launchHeaderSubmit.bits.dmaTag(params.dmaTagWidth - 1, 0)
  launchHeaderQueue.io.enq.bits.packetCount :=
    io.launchHeaderSubmit.bits.packetCount(params.packetCountWidth - 1, 0)
  io.launchHeaderOut <> launchHeaderQueue.io.deq

  val controlErrorArbiter = Module(new Arbiter(
    new CgraTransferControlProtocolError, 3))

  controlErrorArbiter.io.in(0).valid := io.launchErrorIn.valid
  controlErrorArbiter.io.in(0).bits.jobId := io.launchErrorIn.bits.jobId
  controlErrorArbiter.io.in(0).bits.slot := io.launchErrorIn.bits.slot
  controlErrorArbiter.io.in(0).bits.requestedBytes :=
    io.launchErrorIn.bits.requestedBytes
  controlErrorArbiter.io.in(0).bits.actualBytes :=
    io.launchErrorIn.bits.actualBytes
  controlErrorArbiter.io.in(0).bits.spmWordAddress :=
    io.launchErrorIn.bits.spmWordAddress
  controlErrorArbiter.io.in(0).bits.dmaTag := io.launchErrorIn.bits.dmaTag
  controlErrorArbiter.io.in(0).bits.operation :=
    io.launchErrorIn.bits.operation
  controlErrorArbiter.io.in(0).bits.reason := io.launchErrorIn.bits.reason
  io.launchErrorIn.ready := controlErrorArbiter.io.in(0).ready

  val descriptorSubmitErrorQueue = Module(new Queue(
    new CgraTransferControlProtocolError, 1))
  descriptorSubmitErrorQueue.io.enq.valid := io.descriptorSubmit.valid &&
    !descriptorFieldsValid
  descriptorSubmitErrorQueue.io.enq.bits.jobId :=
    io.descriptorSubmit.bits.jobId
  descriptorSubmitErrorQueue.io.enq.bits.slot := io.descriptorSubmit.bits.slot
  descriptorSubmitErrorQueue.io.enq.bits.requestedBytes :=
    io.descriptorSubmit.bits.bytes
  descriptorSubmitErrorQueue.io.enq.bits.actualBytes := 0.U
  descriptorSubmitErrorQueue.io.enq.bits.spmWordAddress :=
    io.descriptorSubmit.bits.spmWordAddress
  descriptorSubmitErrorQueue.io.enq.bits.dmaTag :=
    io.descriptorSubmit.bits.dmaTag
  descriptorSubmitErrorQueue.io.enq.bits.operation :=
    CgraLaunchError.Operation.Pull
  descriptorSubmitErrorQueue.io.enq.bits.reason :=
    CgraLaunchError.Reason.FieldOutOfRange
  controlErrorArbiter.io.in(1) <> descriptorSubmitErrorQueue.io.deq

  val launchSubmitErrorQueue = Module(new Queue(
    new CgraTransferControlProtocolError, 1))
  launchSubmitErrorQueue.io.enq.valid := io.launchHeaderSubmit.valid &&
    !launchHeaderFieldsValid
  launchSubmitErrorQueue.io.enq.bits.jobId :=
    io.launchHeaderSubmit.bits.jobId
  launchSubmitErrorQueue.io.enq.bits.slot := io.launchHeaderSubmit.bits.slot
  launchSubmitErrorQueue.io.enq.bits.requestedBytes :=
    io.launchHeaderSubmit.bits.bytes
  launchSubmitErrorQueue.io.enq.bits.actualBytes := 0.U
  launchSubmitErrorQueue.io.enq.bits.spmWordAddress :=
    io.launchHeaderSubmit.bits.spmWordAddress
  launchSubmitErrorQueue.io.enq.bits.dmaTag :=
    io.launchHeaderSubmit.bits.dmaTag
  launchSubmitErrorQueue.io.enq.bits.operation :=
    CgraLaunchError.Operation.Header
  launchSubmitErrorQueue.io.enq.bits.reason :=
    CgraLaunchError.Reason.FieldOutOfRange
  controlErrorArbiter.io.in(2) <> launchSubmitErrorQueue.io.deq

  io.descriptorSubmit.ready := Mux(
    descriptorFieldsValid,
    descriptorQueue.io.enq.ready,
    descriptorSubmitErrorQueue.io.enq.ready)
  io.launchHeaderSubmit.ready := Mux(
    launchHeaderFieldsValid,
    launchHeaderQueue.io.enq.ready,
    launchSubmitErrorQueue.io.enq.ready)

  val controlErrorQueue = Module(new Queue(
    new CgraTransferControlProtocolError, 2))
  controlErrorQueue.io.enq <> controlErrorArbiter.io.out
  io.launchErrorOut <> controlErrorQueue.io.deq

  connectQueue(io.launchPacketSubmit, io.launchPacketOut, 2)
  connectQueue(io.launchResultIn, io.launchResultOut, 2)
  connectQueue(io.computeCompletionIn, io.computeCompletionOut, 2)
  connectQueue(io.computeErrorIn, io.computeErrorOut, 2)
}
