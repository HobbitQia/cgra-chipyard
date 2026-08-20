package chipyard.example

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CgraConsumerEndpointHarness extends Module {
  private val consumerParams = CgraConsumerPullAdapterParams.production
  private val endpointParams = SpmTransferEndpointParams.production

  val io = IO(new Bundle {
    val descriptorIn = Flipped(
      Decoupled(new CgraConsumerPullDescriptor(consumerParams)))
    val producerRequestOut = Decoupled(new SpmTransferRequest)
    val producerReadyIn = Flipped(Decoupled(new SpmTransferReady))
    val dmaCommandOut = Decoupled(new CgraAutomaticDmaCommand(consumerParams))
    val dmaReadStartIn = Flipped(
      Decoupled(new CgraAutomaticDmaEvent(consumerParams)))
    val dmaDoneIn = Flipped(
      Decoupled(new CgraAutomaticDmaEvent(consumerParams)))
    val completionOut = Decoupled(new CgraConsumerCompletion(consumerParams))
    val consumerErrorOut = Decoupled(
      new CgraConsumerProtocolError(consumerParams))
    val slots = Output(Vec(endpointParams.slotCount, new SpmTransferSlotDebug))
  })

  val endpoint = Module(new SpmTransferEndpoint(endpointParams))
  val consumer = Module(new CgraConsumerPullAdapter(consumerParams))

  consumer.io.descriptorIn <> io.descriptorIn
  consumer.io.requestOut <> endpoint.io.requestIn
  io.producerRequestOut <> endpoint.io.requestOut
  endpoint.io.readyIn <> io.producerReadyIn
  consumer.io.readyIn <> endpoint.io.readyOut
  endpoint.io.readStartIn <> consumer.io.readStartOut
  endpoint.io.releaseIn <> consumer.io.releaseOut
  io.dmaCommandOut <> consumer.io.dmaCommandOut
  consumer.io.dmaReadStartIn <> io.dmaReadStartIn
  consumer.io.dmaDoneIn <> io.dmaDoneIn
  io.completionOut <> consumer.io.completionOut
  io.consumerErrorOut <> consumer.io.errorOut
  endpoint.io.errorOut.ready := true.B
  io.slots := endpoint.io.slots
}

class CgraConsumerPullAdapterSpec extends AnyFlatSpec
    with ChiselScalatestTester {
  private val params = CgraConsumerPullAdapterParams.production

  private def initialize(dut: CgraConsumerEndpointHarness): Unit = {
    dut.io.descriptorIn.valid.poke(false.B)
    dut.io.producerRequestOut.ready.poke(false.B)
    dut.io.producerReadyIn.valid.poke(false.B)
    dut.io.dmaCommandOut.ready.poke(false.B)
    dut.io.dmaReadStartIn.valid.poke(false.B)
    dut.io.dmaDoneIn.valid.poke(false.B)
    dut.io.completionOut.ready.poke(false.B)
    dut.io.consumerErrorOut.ready.poke(false.B)
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
    dut.clock.step()
  }

  private def waitFor(
    dut: CgraConsumerEndpointHarness,
    signal: Bool,
    cycles: Int = 30): Unit = {
    var remaining = cycles
    while (signal.peek().litValue == 0 && remaining > 0) {
      dut.clock.step()
      remaining -= 1
    }
    assert(signal.peek().litValue == 1,
      s"signal did not assert within $cycles cycles")
  }

  private def sendDescriptor(
    dut: CgraConsumerEndpointHarness,
    jobId: BigInt,
    slot: BigInt,
    bytes: BigInt,
    spmWordAddress: BigInt,
    tag: BigInt): Unit = {
    dut.io.descriptorIn.bits.jobId.poke(jobId.U)
    dut.io.descriptorIn.bits.slot.poke(slot.U)
    dut.io.descriptorIn.bits.bytes.poke(bytes.U)
    dut.io.descriptorIn.bits.spmWordAddress.poke(spmWordAddress.U)
    dut.io.descriptorIn.bits.dmaTag.poke(tag.U)
    dut.io.descriptorIn.valid.poke(true.B)
    waitFor(dut, dut.io.descriptorIn.ready)
    dut.clock.step()
    dut.io.descriptorIn.valid.poke(false.B)
  }

  private def forwardRequest(
    dut: CgraConsumerEndpointHarness,
    jobId: BigInt,
    slot: BigInt,
    bytes: BigInt): Unit = {
    waitFor(dut, dut.io.producerRequestOut.valid)
    dut.io.producerRequestOut.bits.jobId.expect(jobId.U)
    dut.io.producerRequestOut.bits.slot.expect(slot.U)
    dut.io.producerRequestOut.bits.maxBytes.expect(bytes.U)
    dut.io.producerRequestOut.ready.poke(true.B)
    dut.clock.step()
    dut.io.producerRequestOut.ready.poke(false.B)
  }

  private def sendProducerReady(
    dut: CgraConsumerEndpointHarness,
    jobId: BigInt,
    slot: BigInt,
    actualBytes: BigInt,
    status: BigInt): Unit = {
    dut.io.producerReadyIn.bits.jobId.poke(jobId.U)
    dut.io.producerReadyIn.bits.slot.poke(slot.U)
    dut.io.producerReadyIn.bits.actualBytes.poke(actualBytes.U)
    dut.io.producerReadyIn.bits.status.poke(status.U)
    dut.io.producerReadyIn.valid.poke(true.B)
    waitFor(dut, dut.io.producerReadyIn.ready)
    dut.clock.step()
    dut.io.producerReadyIn.valid.poke(false.B)
  }

  private def acceptDmaCommand(
    dut: CgraConsumerEndpointHarness,
    jobId: BigInt,
    slot: BigInt,
    bytes: BigInt,
    spmWordAddress: BigInt,
    tag: BigInt): Unit = {
    waitFor(dut, dut.io.dmaCommandOut.valid)
    dut.io.dmaCommandOut.bits.jobId.expect(jobId.U)
    dut.io.dmaCommandOut.bits.slot.expect(slot.U)
    dut.io.dmaCommandOut.bits.sourceAddress.expect(
      params.slotBases(slot.toInt).U)
    dut.io.dmaCommandOut.bits.spmWordAddress.expect(spmWordAddress.U)
    dut.io.dmaCommandOut.bits.bytes.expect(bytes.U)
    dut.io.dmaCommandOut.bits.dmaTag.expect(tag.U)
    for (_ <- 0 until 3) {
      dut.io.dmaCommandOut.valid.expect(true.B)
      dut.io.dmaCommandOut.bits.jobId.expect(jobId.U)
      dut.io.dmaCommandOut.bits.sourceAddress.expect(
        params.slotBases(slot.toInt).U)
      dut.clock.step()
    }
    dut.io.dmaCommandOut.ready.poke(true.B)
    dut.clock.step()
    dut.io.dmaCommandOut.ready.poke(false.B)
  }

  private def sendDmaEvent(
    dut: CgraConsumerEndpointHarness,
    readStart: Boolean,
    jobId: BigInt,
    slot: BigInt,
    tag: BigInt): Unit = {
    val channel = if (readStart) dut.io.dmaReadStartIn else dut.io.dmaDoneIn
    channel.bits.jobId.poke(jobId.U)
    channel.bits.slot.poke(slot.U)
    channel.bits.dmaTag.poke(tag.U)
    channel.valid.poke(true.B)
    waitFor(dut, channel.ready)
    dut.clock.step()
    channel.valid.poke(false.B)
  }

  private def expectCompletion(
    dut: CgraConsumerEndpointHarness,
    jobId: BigInt,
    slot: BigInt,
    bytes: BigInt,
    tag: BigInt,
    consumerStatus: UInt,
    producerStatus: BigInt): Unit = {
    waitFor(dut, dut.io.completionOut.valid)
    dut.io.completionOut.bits.jobId.expect(jobId.U)
    dut.io.completionOut.bits.slot.expect(slot.U)
    dut.io.completionOut.bits.actualBytes.expect(bytes.U)
    dut.io.completionOut.bits.dmaTag.expect(tag.U)
    dut.io.completionOut.bits.consumerStatus.expect(consumerStatus)
    dut.io.completionOut.bits.producerStatus.expect(producerStatus.U)
    dut.io.completionOut.ready.poke(true.B)
    dut.clock.step()
    dut.io.completionOut.ready.poke(false.B)
  }

  behavior of "CgraConsumerPullAdapter"

  it should "pull 64 and 128 bytes from generated slot bases and release only after read start and DMA done" in {
    test(new CgraConsumerEndpointHarness) { dut =>
      import SpmTransferProtocol.SlotState
      initialize(dut)

      sendDescriptor(dut, 0x51, 0, 64, 0, 0x21)
      dut.io.dmaCommandOut.valid.expect(false.B)
      forwardRequest(dut, 0x51, 0, 64)
      dut.io.dmaCommandOut.valid.expect(false.B)
      sendProducerReady(dut, 0x51, 0, 64, 0)
      acceptDmaCommand(dut, 0x51, 0, 64, 0, 0x21)

      dut.io.slots(0).state.expect(SlotState.Ready)
      sendDmaEvent(dut, readStart = false, 0x51, 0, 0x21)
      dut.clock.step(2)
      dut.io.slots(0).state.expect(SlotState.Ready)
      dut.io.completionOut.valid.expect(false.B)
      sendDmaEvent(dut, readStart = true, 0x51, 0, 0x21)
      waitFor(dut, dut.io.completionOut.valid)
      dut.io.slots(0).state.expect(SlotState.Free)

      // Keep completion 0 stalled while slot 1 independently completes. The
      // two-entry completion queue preserves both exactly once.
      for (_ <- 0 until 3) {
        dut.io.completionOut.bits.jobId.expect(0x51.U)
        dut.io.completionOut.bits.actualBytes.expect(64.U)
        dut.clock.step()
      }
      sendDescriptor(dut, 0x52, 1, 128, 32, 0x22)
      forwardRequest(dut, 0x52, 1, 128)
      sendProducerReady(dut, 0x52, 1, 128, 0)
      acceptDmaCommand(dut, 0x52, 1, 128, 32, 0x22)
      sendDmaEvent(dut, readStart = true, 0x52, 1, 0x22)
      sendDmaEvent(dut, readStart = false, 0x52, 1, 0x22)
      dut.clock.step(3)
      dut.io.slots(1).state.expect(SlotState.Free)

      expectCompletion(
        dut, 0x51, 0, 64, 0x21, CgraConsumerStatus.Success, 0)
      expectCompletion(
        dut, 0x52, 1, 128, 0x22, CgraConsumerStatus.Success, 0)
      dut.io.completionOut.valid.expect(false.B)
    }
  }

  it should "direct-release a failed producer READY without issuing DMA" in {
    test(new CgraConsumerEndpointHarness) { dut =>
      import SpmTransferProtocol.SlotState
      initialize(dut)
      sendDescriptor(dut, 0x61, 1, 64, 0, 0x31)
      forwardRequest(dut, 0x61, 1, 64)
      sendProducerReady(dut, 0x61, 1, 0, 7)
      dut.io.dmaCommandOut.valid.expect(false.B)
      waitFor(dut, dut.io.completionOut.valid)
      dut.io.slots(1).state.expect(SlotState.Free)
      expectCompletion(
        dut, 0x61, 1, 0, 0x31, CgraConsumerStatus.ProducerFailure, 7)
    }
  }

  it should "reject malformed descriptors before endpoint reservation" in {
    test(new CgraConsumerEndpointHarness) { dut =>
      initialize(dut)
      val malformed = Seq(
        (BigInt(0), BigInt(0), BigInt(64), BigInt(0),
          CgraConsumerStatus.InvalidJob),
        (BigInt(1), BigInt(2), BigInt(64), BigInt(0),
          CgraConsumerStatus.InvalidSlot),
        (BigInt(2), BigInt(0), BigInt(0), BigInt(0),
          CgraConsumerStatus.InvalidLength),
        (BigInt(3), BigInt(0), BigInt(16), BigInt(0),
          CgraConsumerStatus.InvalidLength),
        (BigInt(4), BigInt(0), BigInt(1024), BigInt(0),
          CgraConsumerStatus.InvalidLength),
        (BigInt(5), BigInt(0), BigInt(64), BigInt(120),
          CgraConsumerStatus.SpmRange))

      malformed.zipWithIndex.foreach {
        case ((jobId, slot, bytes, spm, status), index) =>
          sendDescriptor(dut, jobId, slot, bytes, spm, 0x40 + index)
          dut.io.producerRequestOut.valid.expect(false.B)
          dut.io.dmaCommandOut.valid.expect(false.B)
          expectCompletion(
            dut, jobId, slot, 0, 0x40 + index, status, 0)
      }
      dut.io.slots(0).state.expect(SpmTransferProtocol.SlotState.Free)
      dut.io.slots(1).state.expect(SpmTransferProtocol.SlotState.Free)
    }
  }

  it should "retain the active descriptor and reject wrong or duplicate DMA events" in {
    test(new CgraConsumerEndpointHarness) { dut =>
      initialize(dut)
      sendDescriptor(dut, 0x71, 0, 64, 0, 0x51)
      // A second descriptor is backpressured while the first is active.
      dut.io.descriptorIn.bits.jobId.poke(0x72.U)
      dut.io.descriptorIn.bits.slot.poke(1.U)
      dut.io.descriptorIn.bits.bytes.poke(64.U)
      dut.io.descriptorIn.bits.spmWordAddress.poke(16.U)
      dut.io.descriptorIn.bits.dmaTag.poke(0x52.U)
      dut.io.descriptorIn.valid.poke(true.B)
      dut.io.descriptorIn.ready.expect(false.B)
      dut.clock.step(2)
      dut.io.descriptorIn.valid.poke(false.B)

      forwardRequest(dut, 0x71, 0, 64)
      sendProducerReady(dut, 0x71, 0, 64, 0)
      acceptDmaCommand(dut, 0x71, 0, 64, 0, 0x51)

      sendDmaEvent(dut, readStart = true, 0x71, 0, 0x50)
      waitFor(dut, dut.io.consumerErrorOut.valid)
      dut.io.consumerErrorOut.bits.operation.expect(
        CgraConsumerError.Operation.ReadStart)
      dut.io.consumerErrorOut.bits.reason.expect(
        CgraConsumerError.Reason.TagMismatch)
      dut.io.consumerErrorOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.consumerErrorOut.ready.poke(false.B)
      dut.io.completionOut.valid.expect(false.B)

      sendDmaEvent(dut, readStart = true, 0x71, 0, 0x51)
      sendDmaEvent(dut, readStart = true, 0x71, 0, 0x51)
      waitFor(dut, dut.io.consumerErrorOut.valid)
      dut.io.consumerErrorOut.bits.reason.expect(
        CgraConsumerError.Reason.DuplicateEvent)
      dut.io.consumerErrorOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.consumerErrorOut.ready.poke(false.B)
      sendDmaEvent(dut, readStart = false, 0x71, 1, 0x51)
      waitFor(dut, dut.io.consumerErrorOut.valid)
      dut.io.consumerErrorOut.bits.reason.expect(
        CgraConsumerError.Reason.IdentityMismatch)
      dut.io.consumerErrorOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.consumerErrorOut.ready.poke(false.B)
      sendDmaEvent(dut, readStart = false, 0x71, 0, 0x51)
      expectCompletion(
        dut, 0x71, 0, 64, 0x51, CgraConsumerStatus.Success, 0)
    }
  }

  it should "reject wrong producer READY identity before DMA and recover losslessly" in {
    test(new CgraConsumerPullAdapter(params)) { dut =>
      dut.io.descriptorIn.valid.poke(false.B)
      dut.io.requestOut.ready.poke(false.B)
      dut.io.readyIn.valid.poke(false.B)
      dut.io.readStartOut.ready.poke(true.B)
      dut.io.releaseOut.ready.poke(true.B)
      dut.io.dmaCommandOut.ready.poke(false.B)
      dut.io.dmaReadStartIn.valid.poke(false.B)
      dut.io.dmaDoneIn.valid.poke(false.B)
      dut.io.completionOut.ready.poke(false.B)
      dut.io.errorOut.ready.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)

      dut.io.descriptorIn.bits.jobId.poke(0x91.U)
      dut.io.descriptorIn.bits.slot.poke(1.U)
      dut.io.descriptorIn.bits.bytes.poke(64.U)
      dut.io.descriptorIn.bits.spmWordAddress.poke(16.U)
      dut.io.descriptorIn.bits.dmaTag.poke(0x73.U)
      dut.io.descriptorIn.valid.poke(true.B)
      dut.io.descriptorIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.descriptorIn.valid.poke(false.B)
      dut.io.requestOut.valid.expect(true.B)
      dut.io.requestOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.requestOut.ready.poke(false.B)

      def rejectReady(jobId: BigInt, slot: BigInt): Unit = {
        dut.io.readyIn.bits.jobId.poke(jobId.U)
        dut.io.readyIn.bits.slot.poke(slot.U)
        dut.io.readyIn.bits.actualBytes.poke(64.U)
        dut.io.readyIn.bits.status.poke(0.U)
        dut.io.readyIn.valid.poke(true.B)
        dut.io.readyIn.ready.expect(true.B)
        dut.clock.step()
        dut.io.readyIn.valid.poke(false.B)
        dut.io.errorOut.valid.expect(true.B)
        dut.io.errorOut.bits.operation.expect(
          CgraConsumerError.Operation.Ready)
        dut.io.errorOut.bits.reason.expect(
          CgraConsumerError.Reason.IdentityMismatch)
        dut.io.dmaCommandOut.valid.expect(false.B)
        dut.io.releaseOut.valid.expect(false.B)
        dut.io.errorOut.ready.poke(true.B)
        dut.clock.step()
        dut.io.errorOut.ready.poke(false.B)
      }

      rejectReady(0x90, 1)
      rejectReady(0x91, 0)

      dut.io.readyIn.bits.jobId.poke(0x91.U)
      dut.io.readyIn.bits.slot.poke(1.U)
      dut.io.readyIn.bits.actualBytes.poke(64.U)
      dut.io.readyIn.bits.status.poke(0.U)
      dut.io.readyIn.valid.poke(true.B)
      dut.io.readyIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.readyIn.valid.poke(false.B)
      dut.io.dmaCommandOut.valid.expect(true.B)
      dut.io.dmaCommandOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.dmaCommandOut.ready.poke(false.B)

      dut.io.dmaDoneIn.bits.jobId.poke(0x91.U)
      dut.io.dmaDoneIn.bits.slot.poke(1.U)
      dut.io.dmaDoneIn.bits.dmaTag.poke(0x72.U)
      dut.io.dmaDoneIn.valid.poke(true.B)
      dut.io.dmaDoneIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.dmaDoneIn.valid.poke(false.B)
      dut.io.errorOut.valid.expect(true.B)
      dut.io.errorOut.bits.reason.expect(CgraConsumerError.Reason.TagMismatch)
      dut.io.releaseOut.valid.expect(false.B)
      dut.io.errorOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.errorOut.ready.poke(false.B)

      dut.io.dmaReadStartIn.bits.jobId.poke(0x91.U)
      dut.io.dmaReadStartIn.bits.slot.poke(1.U)
      dut.io.dmaReadStartIn.bits.dmaTag.poke(0x73.U)
      dut.io.dmaReadStartIn.valid.poke(true.B)
      dut.clock.step()
      dut.io.dmaReadStartIn.valid.poke(false.B)
      dut.io.dmaDoneIn.bits.dmaTag.poke(0x73.U)
      dut.io.dmaDoneIn.valid.poke(true.B)
      dut.clock.step()
      dut.io.dmaDoneIn.valid.poke(false.B)

      var remaining = 20
      while (dut.io.completionOut.valid.peek().litValue == 0 && remaining > 0) {
        dut.clock.step()
        remaining -= 1
      }
      assert(remaining > 0)
      dut.io.completionOut.bits.jobId.expect(0x91.U)
      dut.io.completionOut.bits.slot.expect(1.U)
      dut.io.completionOut.bits.actualBytes.expect(64.U)
      dut.io.completionOut.bits.dmaTag.expect(0x73.U)
      dut.io.completionOut.bits.consumerStatus.expect(
        CgraConsumerStatus.Success)
      dut.io.completionOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.completionOut.valid.expect(false.B)
    }
  }
}
