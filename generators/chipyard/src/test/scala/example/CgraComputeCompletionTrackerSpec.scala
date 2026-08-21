package chipyard.example

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CgraComputeCompletionTrackerSpec extends AnyFlatSpec
    with ChiselScalatestTester {
  private val params = CgraComputeLaunchGateParams.production

  private def initialize(dut: CgraComputeCompletionTracker): Unit = {
    dut.io.reservationIn.valid.poke(false.B)
    dut.io.launchAcceptedIn.valid.poke(false.B)
    dut.io.launchResultIn.valid.poke(false.B)
    dut.io.launchResultOut.ready.poke(false.B)
    dut.io.completeIn.valid.poke(false.B)
    dut.io.completionOut.ready.poke(false.B)
    dut.io.errorOut.ready.poke(false.B)
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
    dut.clock.step()
  }

  private def waitReady(dut: CgraComputeCompletionTracker, ready: Bool): Unit = {
    var cycles = 40
    while (!ready.peek().litToBoolean && cycles > 0) {
      dut.clock.step()
      cycles -= 1
    }
    assert(cycles > 0)
  }

  private def reserve(
    dut: CgraComputeCompletionTracker,
    job: BigInt,
    slot: BigInt,
    bytes: BigInt,
    spm: BigInt,
    tag: BigInt,
    packets: BigInt): Unit = {
    dut.io.reservationIn.bits.jobId.poke(job.U)
    dut.io.reservationIn.bits.slot.poke(slot.U)
    dut.io.reservationIn.bits.bytes.poke(bytes.U)
    dut.io.reservationIn.bits.spmWordAddress.poke(spm.U)
    dut.io.reservationIn.bits.dmaTag.poke(tag.U)
    dut.io.reservationIn.bits.packetCount.poke(packets.U)
    dut.io.reservationIn.valid.poke(true.B)
    waitReady(dut, dut.io.reservationIn.ready)
    dut.clock.step()
    dut.io.reservationIn.valid.poke(false.B)
  }

  private def launchResult(
    dut: CgraComputeCompletionTracker,
    job: BigInt,
    slot: BigInt,
    bytes: BigInt,
    actual: BigInt,
    spm: BigInt,
    tag: BigInt,
    packets: BigInt,
    status: BigInt): Unit = {
    dut.io.launchResultIn.bits.jobId.poke(job.U)
    dut.io.launchResultIn.bits.slot.poke(slot.U)
    dut.io.launchResultIn.bits.requestedBytes.poke(bytes.U)
    dut.io.launchResultIn.bits.actualBytes.poke(actual.U)
    dut.io.launchResultIn.bits.spmWordAddress.poke(spm.U)
    dut.io.launchResultIn.bits.dmaTag.poke(tag.U)
    dut.io.launchResultIn.bits.packetCount.poke(packets.U)
    dut.io.launchResultIn.bits.status.poke(status.U)
    dut.io.launchResultIn.valid.poke(true.B)
    waitReady(dut, dut.io.launchResultIn.ready)
    dut.clock.step()
    dut.io.launchResultIn.valid.poke(false.B)
  }

  private def launchAccepted(
    dut: CgraComputeCompletionTracker,
    job: BigInt,
    slot: BigInt,
    bytes: BigInt,
    spm: BigInt,
    tag: BigInt,
    packets: BigInt): Unit = {
    dut.io.launchAcceptedIn.bits.jobId.poke(job.U)
    dut.io.launchAcceptedIn.bits.slot.poke(slot.U)
    dut.io.launchAcceptedIn.bits.requestedBytes.poke(bytes.U)
    dut.io.launchAcceptedIn.bits.actualBytes.poke(bytes.U)
    dut.io.launchAcceptedIn.bits.spmWordAddress.poke(spm.U)
    dut.io.launchAcceptedIn.bits.dmaTag.poke(tag.U)
    dut.io.launchAcceptedIn.bits.packetCount.poke(packets.U)
    dut.io.launchAcceptedIn.bits.status.poke(
      CgraLaunchStatus.LaunchAccepted)
    dut.io.launchAcceptedIn.valid.poke(true.B)
    dut.clock.step()
    dut.io.launchAcceptedIn.valid.poke(false.B)
  }

  private def complete(
    dut: CgraComputeCompletionTracker,
    data: BigInt): Unit = {
    dut.io.completeIn.bits.poke(data.U)
    dut.io.completeIn.valid.poke(true.B)
    waitReady(dut, dut.io.completeIn.ready)
    dut.clock.step()
    dut.io.completeIn.valid.poke(false.B)
  }

  behavior of "CgraComputeCompletionTracker"

  it should "correlate only a real post-LaunchAccepted completion and reuse" in {
    test(new CgraComputeCompletionTracker(params)) { dut =>
      initialize(dut)
      reserve(dut, 0x701, 0, 128, 0, 0x71, 10)
      dut.io.active.expect(true.B)
      dut.io.launched.expect(false.B)

      complete(dut, 0xdead)
      dut.io.errorOut.valid.expect(true.B)
      dut.io.errorOut.bits.reason.expect(
        CgraComputeCompletionError.Reason.CompleteBeforeLaunch)
      dut.io.completionOut.valid.expect(false.B)
      val heldErrorJob = dut.io.errorOut.bits.jobId.peek().litValue
      val heldErrorReason = dut.io.errorOut.bits.reason.peek().litValue
      dut.clock.step(3)
      dut.io.errorOut.valid.expect(true.B)
      assert(dut.io.errorOut.bits.jobId.peek().litValue == heldErrorJob)
      assert(dut.io.errorOut.bits.reason.peek().litValue == heldErrorReason)
      dut.io.errorOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.errorOut.ready.poke(false.B)

      launchAccepted(dut, 0x701, 0, 128, 0, 0x71, 10)
      launchResult(dut, 0x701, 0, 128, 128, 0, 0x71, 10,
        CgraLaunchStatus.LaunchAccepted.litValue)
      dut.io.launched.expect(true.B)
      dut.io.launchResultOut.valid.expect(true.B)
      val heldLaunchJob = dut.io.launchResultOut.bits.jobId.peek().litValue
      val heldLaunchStatus = dut.io.launchResultOut.bits.status.peek().litValue
      dut.clock.step(3)
      assert(dut.io.launchResultOut.bits.jobId.peek().litValue == heldLaunchJob)
      assert(
        dut.io.launchResultOut.bits.status.peek().litValue == heldLaunchStatus)

      complete(dut, 0x12345678)
      dut.io.completionOut.valid.expect(true.B)
      dut.io.completionOut.bits.jobId.expect(0x701.U)
      dut.io.completionOut.bits.slot.expect(0.U)
      dut.io.completionOut.bits.requestedBytes.expect(128.U)
      dut.io.completionOut.bits.actualBytes.expect(128.U)
      dut.io.completionOut.bits.spmWordAddress.expect(0.U)
      dut.io.completionOut.bits.dmaTag.expect(0x71.U)
      dut.io.completionOut.bits.packetCount.expect(10.U)
      dut.io.completionOut.bits.completeData.expect(0x12345678.U)
      dut.io.completed.expect(true.B)

      dut.io.reservationIn.valid.poke(true.B)
      dut.io.reservationIn.ready.expect(false.B)
      dut.io.reservationIn.valid.poke(false.B)
      complete(dut, BigInt("87654321", 16))
      dut.io.errorOut.valid.expect(true.B)
      dut.io.errorOut.bits.reason.expect(
        CgraComputeCompletionError.Reason.DuplicateComplete)
      dut.io.errorOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.errorOut.ready.poke(false.B)

      dut.io.launchResultOut.ready.poke(true.B)
      dut.io.completionOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.launchResultOut.ready.poke(false.B)
      dut.io.completionOut.ready.poke(false.B)
      dut.io.active.expect(false.B)

      reserve(dut, 0x702, 1, 64, 32, 0x72, 1)
      launchAccepted(dut, 0x702, 1, 64, 32, 0x72, 1)
      launchResult(dut, 0x702, 1, 64, 64, 32, 0x72, 1,
        CgraLaunchStatus.LaunchAccepted.litValue)
      complete(dut, 0)
      dut.io.completionOut.bits.jobId.expect(0x702.U)
      dut.io.completionOut.bits.slot.expect(1.U)
    }
  }

  it should "reject a wrong launch identity and clear failed ownership" in {
    test(new CgraComputeCompletionTracker(params)) { dut =>
      initialize(dut)
      reserve(dut, 0x711, 0, 128, 0, 0x31, 10)
      launchResult(dut, 0x711, 1, 128, 128, 0, 0x31, 10,
        CgraLaunchStatus.LaunchAccepted.litValue)
      dut.io.errorOut.valid.expect(true.B)
      dut.io.errorOut.bits.operation.expect(
        CgraComputeCompletionError.Operation.LaunchResult)
      dut.io.errorOut.bits.reason.expect(
        CgraComputeCompletionError.Reason.IdentityMismatch)
      dut.io.active.expect(false.B)
      dut.io.completionOut.valid.expect(false.B)

      dut.io.errorOut.ready.poke(true.B)
      dut.io.launchResultOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.errorOut.ready.poke(false.B)
      dut.io.launchResultOut.ready.poke(false.B)

      reserve(dut, 0x712, 1, 64, 16, 0x32, 1)
      launchResult(dut, 0x712, 1, 64, 0, 16, 0x32, 1,
        CgraLaunchStatus.ProducerFailure.litValue)
      dut.io.active.expect(false.B)
      complete(dut, 0x55)
      dut.io.completionOut.valid.expect(false.B)
      dut.io.errorOut.valid.expect(false.B)
    }
  }

  it should "reject a duplicate matching launch result without forwarding it" in {
    test(new CgraComputeCompletionTracker(params)) { dut =>
      initialize(dut)
      reserve(dut, 0x721, 0, 128, 0, 0x41, 10)
      launchAccepted(dut, 0x721, 0, 128, 0, 0x41, 10)
      launchResult(dut, 0x721, 0, 128, 128, 0, 0x41, 10,
        CgraLaunchStatus.LaunchAccepted.litValue)
      dut.io.launchResultOut.valid.expect(true.B)
      dut.io.launchResultOut.bits.jobId.expect(0x721.U)

      launchResult(dut, 0x721, 0, 128, 128, 0, 0x41, 10,
        CgraLaunchStatus.LaunchAccepted.litValue)
      dut.io.errorOut.valid.expect(true.B)
      dut.io.errorOut.bits.operation.expect(
        CgraComputeCompletionError.Operation.LaunchResult)
      dut.io.errorOut.bits.reason.expect(
        CgraComputeCompletionError.Reason.DuplicateLaunchResult)
      dut.io.active.expect(true.B)
      dut.io.launched.expect(true.B)

      dut.io.launchResultOut.ready.poke(true.B)
      dut.io.errorOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.launchResultOut.ready.poke(false.B)
      dut.io.errorOut.ready.poke(false.B)
      dut.io.launchResultOut.valid.expect(false.B)
      dut.io.errorOut.valid.expect(false.B)

      complete(dut, 0x44)
      dut.io.completionOut.valid.expect(true.B)
      dut.io.completionOut.bits.jobId.expect(0x721.U)
    }
  }
}
