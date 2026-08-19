package chipyard.example

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SpmTransferEndpointSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val params = SpmTransferEndpointParams.production

  private def initialize(dut: SpmTransferEndpoint): Unit = {
    dut.io.requestIn.valid.poke(false.B)
    dut.io.readyIn.valid.poke(false.B)
    dut.io.readStartIn.valid.poke(false.B)
    dut.io.releaseIn.valid.poke(false.B)
    dut.io.requestOut.ready.poke(false.B)
    dut.io.readyOut.ready.poke(false.B)
    dut.io.errorOut.ready.poke(false.B)
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
    dut.clock.step()
  }

  private def waitFor(dut: SpmTransferEndpoint, signal: Bool, cycles: Int = 20): Unit = {
    var remaining = cycles
    while (signal.peek().litValue == 0 && remaining > 0) {
      dut.clock.step()
      remaining -= 1
    }
    assert(signal.peek().litValue == 1, s"signal did not assert within $cycles cycles")
  }

  private def sendRequest(
    dut: SpmTransferEndpoint,
    jobId: BigInt,
    slot: BigInt,
    maxBytes: BigInt): Unit = {
    dut.io.requestIn.bits.jobId.poke(jobId.U)
    dut.io.requestIn.bits.slot.poke(slot.U)
    dut.io.requestIn.bits.maxBytes.poke(maxBytes.U)
    dut.io.requestIn.valid.poke(true.B)
    waitFor(dut, dut.io.requestIn.ready)
    dut.clock.step()
    dut.io.requestIn.valid.poke(false.B)
  }

  private def sendReady(
    dut: SpmTransferEndpoint,
    jobId: BigInt,
    slot: BigInt,
    actualBytes: BigInt,
    status: BigInt): Unit = {
    dut.io.readyIn.bits.jobId.poke(jobId.U)
    dut.io.readyIn.bits.slot.poke(slot.U)
    dut.io.readyIn.bits.actualBytes.poke(actualBytes.U)
    dut.io.readyIn.bits.status.poke(status.U)
    dut.io.readyIn.valid.poke(true.B)
    waitFor(dut, dut.io.readyIn.ready)
    dut.clock.step()
    dut.io.readyIn.valid.poke(false.B)
  }

  private def sendReadStart(
    dut: SpmTransferEndpoint,
    jobId: BigInt,
    slot: BigInt): Unit = {
    dut.io.readStartIn.bits.jobId.poke(jobId.U)
    dut.io.readStartIn.bits.slot.poke(slot.U)
    dut.io.readStartIn.valid.poke(true.B)
    waitFor(dut, dut.io.readStartIn.ready)
    dut.clock.step()
    dut.io.readStartIn.valid.poke(false.B)
  }

  private def sendRelease(
    dut: SpmTransferEndpoint,
    jobId: BigInt,
    slot: BigInt): Unit = {
    dut.io.releaseIn.bits.jobId.poke(jobId.U)
    dut.io.releaseIn.bits.slot.poke(slot.U)
    dut.io.releaseIn.valid.poke(true.B)
    waitFor(dut, dut.io.releaseIn.ready)
    dut.clock.step()
    dut.io.releaseIn.valid.poke(false.B)
  }

  private def expectError(
    dut: SpmTransferEndpoint,
    jobId: BigInt,
    slot: BigInt,
    operation: UInt,
    reason: UInt): Unit = {
    waitFor(dut, dut.io.errorOut.valid)
    dut.io.errorOut.bits.jobId.expect(jobId.U)
    dut.io.errorOut.bits.slot.expect(slot.U)
    dut.io.errorOut.bits.operation.expect(operation)
    dut.io.errorOut.bits.reason.expect(reason)
    dut.io.errorOut.ready.poke(true.B)
    dut.clock.step()
    dut.io.errorOut.ready.poke(false.B)
  }

  private def consumeRequest(
    dut: SpmTransferEndpoint,
    jobId: BigInt,
    slot: BigInt,
    maxBytes: BigInt): Unit = {
    waitFor(dut, dut.io.requestOut.valid)
    dut.io.requestOut.bits.jobId.expect(jobId.U)
    dut.io.requestOut.bits.slot.expect(slot.U)
    dut.io.requestOut.bits.maxBytes.expect(maxBytes.U)
    dut.io.requestOut.ready.poke(true.B)
    dut.clock.step()
    dut.io.requestOut.ready.poke(false.B)
  }

  private def consumeReady(
    dut: SpmTransferEndpoint,
    jobId: BigInt,
    slot: BigInt,
    actualBytes: BigInt,
    status: BigInt): Unit = {
    waitFor(dut, dut.io.readyOut.valid)
    dut.io.readyOut.bits.jobId.expect(jobId.U)
    dut.io.readyOut.bits.slot.expect(slot.U)
    dut.io.readyOut.bits.actualBytes.expect(actualBytes.U)
    dut.io.readyOut.bits.status.expect(status.U)
    dut.io.readyOut.ready.poke(true.B)
    dut.clock.step()
    dut.io.readyOut.ready.poke(false.B)
  }

  behavior of "SpmTransferEndpoint"

  it should "run both slots independently and hold shared outputs under backpressure" in {
    test(new SpmTransferEndpoint(params)) { dut =>
      import SpmTransferProtocol.SlotState

      initialize(dut)
      dut.io.slots(0).state.expect(SlotState.Free)
      dut.io.slots(1).state.expect(SlotState.Free)

      sendRequest(dut, 1, 0, 128)
      waitFor(dut, dut.io.requestOut.valid)
      dut.io.requestOut.bits.jobId.expect(1.U)
      dut.io.requestOut.bits.slot.expect(0.U)
      dut.io.requestOut.bits.maxBytes.expect(128.U)
      for (_ <- 0 until 3) {
        dut.io.requestOut.valid.expect(true.B)
        dut.io.requestOut.bits.jobId.expect(1.U)
        dut.io.requestOut.bits.slot.expect(0.U)
        dut.io.requestOut.bits.maxBytes.expect(128.U)
        dut.clock.step()
      }

      // Slot 1 can reserve while slot 0's producer request is stalled.
      sendRequest(dut, 2, 1, 1024)
      dut.io.slots(0).state.expect(SlotState.Producing)
      dut.io.slots(1).state.expect(SlotState.Producing)
      dut.io.slots(0).requestForwarded.expect(false.B)
      dut.io.slots(1).requestPending.expect(true.B)

      consumeRequest(dut, 1, 0, 128)
      consumeRequest(dut, 2, 1, 1024)
      dut.io.slots(0).requestForwarded.expect(true.B)
      dut.io.slots(1).requestForwarded.expect(true.B)

      sendReady(dut, 2, 1, 1024, 0)
      waitFor(dut, dut.io.readyOut.valid)
      dut.io.readyOut.bits.jobId.expect(2.U)
      for (_ <- 0 until 3) {
        dut.io.readyOut.valid.expect(true.B)
        dut.io.readyOut.bits.jobId.expect(2.U)
        dut.io.readyOut.bits.slot.expect(1.U)
        dut.io.readyOut.bits.actualBytes.expect(1024.U)
        dut.io.readyOut.bits.status.expect(0.U)
        dut.clock.step()
      }

      // Slot 0's READY is accepted and retained while slot 1 is stalled.
      sendReady(dut, 1, 0, 128, 0)
      dut.io.slots(0).state.expect(SlotState.Ready)
      dut.io.slots(0).readyPending.expect(true.B)
      consumeReady(dut, 2, 1, 1024, 0)
      consumeReady(dut, 1, 0, 128, 0)

      sendReadStart(dut, 2, 1)
      sendReadStart(dut, 1, 0)
      dut.io.slots(0).state.expect(SlotState.Reading)
      dut.io.slots(1).state.expect(SlotState.Reading)

      sendRelease(dut, 1, 0)
      sendRelease(dut, 2, 1)
      for (slot <- 0 until params.slotCount) {
        dut.io.slots(slot).state.expect(SlotState.Free)
        dut.io.slots(slot).jobId.expect(0.U)
        dut.io.slots(slot).maxBytes.expect(0.U)
        dut.io.slots(slot).actualBytes.expect(0.U)
        dut.io.slots(slot).status.expect(0.U)
      }
      dut.clock.step(3)
      dut.io.requestOut.valid.expect(false.B)
      dut.io.readyOut.valid.expect(false.B)
    }
  }

  it should "reject invalid requests without changing slot ownership" in {
    test(new SpmTransferEndpoint(params)) { dut =>
      import SpmTransferProtocol.{ErrorReason, Operation, SlotState}

      initialize(dut)
      sendRequest(dut, 1, 2, 16)
      expectError(dut, 1, 2, Operation.Request, ErrorReason.InvalidSlot)
      sendRequest(dut, 0, 0, 16)
      expectError(dut, 0, 0, Operation.Request, ErrorReason.InvalidJob)
      sendRequest(dut, 2, 0, 0)
      expectError(dut, 2, 0, Operation.Request, ErrorReason.InvalidLength)
      sendRequest(dut, 2, 0, 18)
      expectError(dut, 2, 0, Operation.Request, ErrorReason.InvalidLength)
      sendRequest(dut, 2, 0, 1040)
      expectError(dut, 2, 0, Operation.Request, ErrorReason.InvalidLength)
      dut.io.slots(0).state.expect(SlotState.Free)
      dut.io.slots(1).state.expect(SlotState.Free)

      sendRequest(dut, 10, 0, 128)
      sendRequest(dut, 11, 0, 128)
      expectError(dut, 11, 0, Operation.Request, ErrorReason.SlotBusy)
      sendRequest(dut, 10, 1, 128)
      expectError(dut, 10, 1, Operation.Request, ErrorReason.DuplicateJob)
      dut.io.slots(0).jobId.expect(10.U)
      dut.io.slots(0).state.expect(SlotState.Producing)
      dut.io.slots(1).state.expect(SlotState.Free)
    }
  }

  it should "retain stalled protocol errors while legal work continues" in {
    test(new SpmTransferEndpoint(params)) { dut =>
      import SpmTransferProtocol.{ErrorReason, Operation, SlotState}

      initialize(dut)
      sendRequest(dut, 20, 2, 16)
      waitFor(dut, dut.io.errorOut.valid)
      dut.io.errorOut.bits.reason.expect(ErrorReason.InvalidSlot)

      dut.io.readyIn.bits.jobId.poke(21.U)
      dut.io.readyIn.bits.slot.poke(2.U)
      dut.io.readyIn.bits.actualBytes.poke(16.U)
      dut.io.readyIn.bits.status.poke(0.U)
      dut.io.readyIn.valid.poke(true.B)
      for (_ <- 0 until 3) {
        dut.io.readyIn.ready.expect(false.B)
        dut.io.errorOut.valid.expect(true.B)
        dut.io.errorOut.bits.jobId.expect(20.U)
        dut.io.errorOut.bits.operation.expect(Operation.Request)
        dut.io.errorOut.bits.reason.expect(ErrorReason.InvalidSlot)
        dut.clock.step()
      }

      // An unrelated valid request is not coupled to the stalled error path.
      dut.io.requestIn.bits.jobId.poke(22.U)
      dut.io.requestIn.bits.slot.poke(1.U)
      dut.io.requestIn.bits.maxBytes.poke(128.U)
      dut.io.requestIn.valid.poke(true.B)
      dut.io.requestIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.requestIn.valid.poke(false.B)
      dut.io.slots(1).state.expect(SlotState.Producing)

      // Dequeuing the first error atomically admits and retains the second.
      dut.io.errorOut.ready.poke(true.B)
      dut.io.readyIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.errorOut.ready.poke(false.B)
      dut.io.readyIn.valid.poke(false.B)
      dut.io.errorOut.valid.expect(true.B)
      dut.io.errorOut.bits.jobId.expect(21.U)
      dut.io.errorOut.bits.slot.expect(2.U)
      dut.io.errorOut.bits.operation.expect(Operation.Ready)
      dut.io.errorOut.bits.reason.expect(ErrorReason.InvalidSlot)
      dut.io.errorOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.errorOut.ready.poke(false.B)
      dut.io.errorOut.valid.expect(false.B)
    }
  }

  it should "drain simultaneous invalid operations exactly once without starvation" in {
    test(new SpmTransferEndpoint(params)) { dut =>
      import SpmTransferProtocol.{ErrorReason, Operation}

      initialize(dut)
      dut.io.requestIn.bits.jobId.poke(101.U)
      dut.io.requestIn.bits.slot.poke(2.U)
      dut.io.requestIn.bits.maxBytes.poke(16.U)
      dut.io.readyIn.bits.jobId.poke(102.U)
      dut.io.readyIn.bits.slot.poke(2.U)
      dut.io.readyIn.bits.actualBytes.poke(16.U)
      dut.io.readyIn.bits.status.poke(0.U)
      dut.io.readStartIn.bits.jobId.poke(103.U)
      dut.io.readStartIn.bits.slot.poke(2.U)
      dut.io.releaseIn.bits.jobId.poke(104.U)
      dut.io.releaseIn.bits.slot.poke(2.U)
      dut.io.requestIn.valid.poke(true.B)
      dut.io.readyIn.valid.poke(true.B)
      dut.io.readStartIn.valid.poke(true.B)
      dut.io.releaseIn.valid.poke(true.B)
      dut.io.errorOut.ready.poke(true.B)

      var accepted = Set.empty[Int]
      var observed = Vector.empty[(BigInt, BigInt, BigInt)]
      var cycles = 0
      while ((accepted.size < 4 || observed.size < 4) && cycles < 12) {
        if (dut.io.errorOut.valid.peek().litValue == 1) {
          observed :+= (
            dut.io.errorOut.bits.jobId.peek().litValue,
            dut.io.errorOut.bits.operation.peek().litValue,
            dut.io.errorOut.bits.reason.peek().litValue)
        }
        val handshakes = Seq(
          dut.io.requestIn.valid.peek().litValue == 1 &&
            dut.io.requestIn.ready.peek().litValue == 1,
          dut.io.readyIn.valid.peek().litValue == 1 &&
            dut.io.readyIn.ready.peek().litValue == 1,
          dut.io.readStartIn.valid.peek().litValue == 1 &&
            dut.io.readStartIn.ready.peek().litValue == 1,
          dut.io.releaseIn.valid.peek().litValue == 1 &&
            dut.io.releaseIn.ready.peek().litValue == 1).zipWithIndex.collect {
          case (fire, index) if fire => index
        }
        assert(handshakes.size <= 1)
        dut.clock.step()
        handshakes.foreach {
          case 0 => dut.io.requestIn.valid.poke(false.B)
          case 1 => dut.io.readyIn.valid.poke(false.B)
          case 2 => dut.io.readStartIn.valid.poke(false.B)
          case 3 => dut.io.releaseIn.valid.poke(false.B)
        }
        accepted ++= handshakes
        cycles += 1
      }

      assert(accepted == Set(0, 1, 2, 3))
      assert(observed.size == 4)
      assert(observed.toSet == Set(
        (BigInt(101), Operation.Request.litValue, ErrorReason.InvalidSlot.litValue),
        (BigInt(102), Operation.Ready.litValue, ErrorReason.InvalidSlot.litValue),
        (BigInt(103), Operation.ReadStart.litValue, ErrorReason.InvalidSlot.litValue),
        (BigInt(104), Operation.Release.litValue, ErrorReason.InvalidSlot.litValue)))
      dut.io.errorOut.valid.expect(false.B)

      // Keep replenishing invalid requests. Each other held operation must
      // still handshake within one four-input round-robin drain.
      dut.io.requestIn.bits.jobId.poke(200.U)
      dut.io.requestIn.valid.poke(true.B)
      dut.io.readyIn.bits.jobId.poke(201.U)
      dut.io.readyIn.valid.poke(true.B)
      dut.io.readStartIn.bits.jobId.poke(202.U)
      dut.io.readStartIn.valid.poke(true.B)
      dut.io.releaseIn.bits.jobId.poke(203.U)
      dut.io.releaseIn.valid.poke(true.B)

      var pendingOthers = Set(1, 2, 3)
      var requestJob = BigInt(200)
      var requestHandshakes = 0
      var fairnessCycles = 0
      while ((pendingOthers.nonEmpty || requestHandshakes == 0) && fairnessCycles < 8) {
        val requestFire = dut.io.requestIn.valid.peek().litValue == 1 &&
          dut.io.requestIn.ready.peek().litValue == 1
        val readyFire = dut.io.readyIn.valid.peek().litValue == 1 &&
          dut.io.readyIn.ready.peek().litValue == 1
        val readStartFire = dut.io.readStartIn.valid.peek().litValue == 1 &&
          dut.io.readStartIn.ready.peek().litValue == 1
        val releaseFire = dut.io.releaseIn.valid.peek().litValue == 1 &&
          dut.io.releaseIn.ready.peek().litValue == 1
        assert(Seq(requestFire, readyFire, readStartFire, releaseFire).count(identity) <= 1)
        dut.clock.step()
        if (requestFire) {
          requestHandshakes += 1
          requestJob += 1
          dut.io.requestIn.bits.jobId.poke(requestJob.U)
        }
        if (readyFire) {
          pendingOthers -= 1
          dut.io.readyIn.valid.poke(false.B)
        }
        if (readStartFire) {
          pendingOthers -= 2
          dut.io.readStartIn.valid.poke(false.B)
        }
        if (releaseFire) {
          pendingOthers -= 3
          dut.io.releaseIn.valid.poke(false.B)
        }
        fairnessCycles += 1
      }
      dut.io.requestIn.valid.poke(false.B)
      assert(pendingOthers.isEmpty)
      assert(fairnessCycles <= 4)
      assert(requestHandshakes >= 1)
    }
  }

  it should "reject mismatched and out-of-order successful-transfer events" in {
    test(new SpmTransferEndpoint(params)) { dut =>
      import SpmTransferProtocol.{ErrorReason, Operation, SlotState}

      initialize(dut)
      sendRequest(dut, 30, 0, 128)
      sendReady(dut, 30, 0, 128, 0)
      expectError(dut, 30, 0, Operation.Ready, ErrorReason.StateOrderMismatch)
      consumeRequest(dut, 30, 0, 128)

      sendReady(dut, 31, 0, 128, 0)
      expectError(dut, 31, 0, Operation.Ready, ErrorReason.JobMismatch)
      sendReady(dut, 30, 0, 144, 0)
      expectError(
        dut,
        30,
        0,
        Operation.Ready,
        ErrorReason.ActualLengthExceedsRequest)
      sendReady(dut, 30, 0, 0, 0)
      expectError(dut, 30, 0, Operation.Ready, ErrorReason.InvalidLength)
      sendReady(dut, 30, 0, 17, 0)
      expectError(dut, 30, 0, Operation.Ready, ErrorReason.InvalidLength)

      sendReady(dut, 30, 0, 128, 0)
      sendReady(dut, 30, 0, 128, 0)
      expectError(dut, 30, 0, Operation.Ready, ErrorReason.StateOrderMismatch)
      consumeReady(dut, 30, 0, 128, 0)

      sendRelease(dut, 30, 0)
      expectError(
        dut,
        30,
        0,
        Operation.Release,
        ErrorReason.StateOrderMismatch)
      dut.io.slots(0).state.expect(SlotState.Ready)

      sendReadStart(dut, 31, 0)
      expectError(dut, 31, 0, Operation.ReadStart, ErrorReason.JobMismatch)
      sendReadStart(dut, 30, 0)
      dut.io.slots(0).state.expect(SlotState.Reading)
      sendReadStart(dut, 30, 0)
      expectError(
        dut,
        30,
        0,
        Operation.ReadStart,
        ErrorReason.StateOrderMismatch)

      sendRelease(dut, 31, 0)
      expectError(dut, 31, 0, Operation.Release, ErrorReason.JobMismatch)
      sendRelease(dut, 30, 0)
      dut.io.slots(0).state.expect(SlotState.Free)
      sendRelease(dut, 30, 0)
      expectError(dut, 30, 0, Operation.Release, ErrorReason.StateOrderMismatch)
    }
  }

  it should "forward producer failure status and release without a read" in {
    test(new SpmTransferEndpoint(params)) { dut =>
      import SpmTransferProtocol.{ErrorReason, Operation, SlotState}

      initialize(dut)
      sendRequest(dut, 40, 1, 1024)
      consumeRequest(dut, 40, 1, 1024)
      sendReady(dut, 40, 1, 0, 7)
      dut.io.slots(1).state.expect(SlotState.Ready)
      dut.io.slots(1).actualBytes.expect(0.U)
      dut.io.slots(1).status.expect(7.U)

      sendRelease(dut, 40, 1)
      expectError(
        dut,
        40,
        1,
        Operation.Release,
        ErrorReason.StateOrderMismatch)
      dut.io.slots(1).state.expect(SlotState.Ready)
      consumeReady(dut, 40, 1, 0, 7)

      sendReadStart(dut, 40, 1)
      expectError(
        dut,
        40,
        1,
        Operation.ReadStart,
        ErrorReason.StateOrderMismatch)
      sendRelease(dut, 40, 1)
      dut.io.slots(1).state.expect(SlotState.Free)
      dut.io.slots(1).jobId.expect(0.U)
      dut.io.slots(1).maxBytes.expect(0.U)
      dut.io.slots(1).actualBytes.expect(0.U)
      dut.io.slots(1).status.expect(0.U)
    }
  }

  it should "support one-byte beats and explicit non-power-of-two slot bounds" in {
    val genericParams = SpmTransferEndpointParams(
      slotCount = 3,
      slotSizeBytes = 16,
      beatBytes = 1)
    test(new SpmTransferEndpoint(genericParams)) { dut =>
      import SpmTransferProtocol.{ErrorReason, Operation, SlotState}

      initialize(dut)
      sendRequest(dut, 50, 2, 1)
      dut.io.slots(2).state.expect(SlotState.Producing)
      consumeRequest(dut, 50, 2, 1)
      sendReady(dut, 50, 2, 1, 0)
      consumeReady(dut, 50, 2, 1, 0)
      sendReadStart(dut, 50, 2)
      sendRelease(dut, 50, 2)
      dut.io.slots(2).state.expect(SlotState.Free)

      sendRequest(dut, 51, 3, 1)
      expectError(dut, 51, 3, Operation.Request, ErrorReason.InvalidSlot)
      for (slot <- 0 until genericParams.slotCount) {
        dut.io.slots(slot).state.expect(SlotState.Free)
      }
    }
  }
}
