package chipyard.example

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CgraTransferControlSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val params = CgraComputeLaunchGateParams.production

  private def initialize(dut: CgraTransferControlQueues): Unit = {
    dut.io.descriptorSubmit.valid.poke(false.B)
    dut.io.descriptorOut.ready.poke(false.B)
    dut.io.launchHeaderSubmit.valid.poke(false.B)
    dut.io.launchHeaderOut.ready.poke(false.B)
    dut.io.launchPacketSubmit.valid.poke(false.B)
    dut.io.launchPacketOut.ready.poke(false.B)
    dut.io.launchResultIn.valid.poke(false.B)
    dut.io.launchResultOut.ready.poke(false.B)
    dut.io.launchErrorIn.valid.poke(false.B)
    dut.io.launchErrorOut.ready.poke(false.B)
    dut.io.computeCompletionIn.valid.poke(false.B)
    dut.io.computeCompletionOut.ready.poke(false.B)
    dut.io.computeErrorIn.valid.poke(false.B)
    dut.io.computeErrorOut.ready.poke(false.B)
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
  }

  behavior of "CgraTransferControlQueues"

  it should "bound submissions and retain typed events under backpressure" in {
    test(new CgraTransferControlQueues(params)) { dut =>
      initialize(dut)

      dut.io.descriptorSubmit.bits.jobId.poke(0x801.U)
      dut.io.descriptorSubmit.bits.slot.poke(1.U)
      dut.io.descriptorSubmit.bits.bytes.poke(128.U)
      dut.io.descriptorSubmit.bits.spmWordAddress.poke(32.U)
      dut.io.descriptorSubmit.bits.dmaTag.poke(0x41.U)
      dut.io.descriptorSubmit.valid.poke(true.B)
      dut.io.descriptorSubmit.ready.expect(true.B)
      dut.clock.step()
      dut.io.descriptorSubmit.ready.expect(false.B)
      dut.io.descriptorSubmit.valid.poke(false.B)
      dut.io.descriptorOut.valid.expect(true.B)
      val heldDescriptorJob = dut.io.descriptorOut.bits.jobId.peek().litValue
      val heldDescriptorTag = dut.io.descriptorOut.bits.dmaTag.peek().litValue
      dut.clock.step(3)
      assert(
        dut.io.descriptorOut.bits.jobId.peek().litValue == heldDescriptorJob)
      assert(
        dut.io.descriptorOut.bits.dmaTag.peek().litValue == heldDescriptorTag)
      dut.io.descriptorOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.descriptorOut.ready.poke(false.B)

      dut.io.computeCompletionIn.bits.jobId.poke(0x801.U)
      dut.io.computeCompletionIn.bits.slot.poke(1.U)
      dut.io.computeCompletionIn.bits.requestedBytes.poke(128.U)
      dut.io.computeCompletionIn.bits.actualBytes.poke(128.U)
      dut.io.computeCompletionIn.bits.spmWordAddress.poke(32.U)
      dut.io.computeCompletionIn.bits.dmaTag.poke(0x41.U)
      dut.io.computeCompletionIn.bits.packetCount.poke(10.U)
      dut.io.computeCompletionIn.bits.completeData.poke(0.U)
      dut.io.computeCompletionIn.bits.status.poke(
        CgraComputeCompletionStatus.Success)
      dut.io.computeCompletionIn.valid.poke(true.B)
      dut.io.computeCompletionIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.computeCompletionIn.valid.poke(false.B)
      dut.io.computeCompletionOut.valid.expect(true.B)
      val heldCompletionJob =
        dut.io.computeCompletionOut.bits.jobId.peek().litValue
      val heldCompletionData =
        dut.io.computeCompletionOut.bits.completeData.peek().litValue
      dut.clock.step(4)
      assert(dut.io.computeCompletionOut.bits.jobId.peek().litValue ==
        heldCompletionJob)
      assert(dut.io.computeCompletionOut.bits.completeData.peek().litValue ==
        heldCompletionData)
      dut.io.computeCompletionOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.computeCompletionOut.valid.expect(false.B)
    }
  }

  it should "bound headers, packets, results, and errors without reordering" in {
    test(new CgraTransferControlQueues(params)) { dut =>
      initialize(dut)

      dut.io.launchHeaderSubmit.bits.jobId.poke(0x811.U)
      dut.io.launchHeaderSubmit.bits.slot.poke(0.U)
      dut.io.launchHeaderSubmit.bits.bytes.poke(128.U)
      dut.io.launchHeaderSubmit.bits.spmWordAddress.poke(0.U)
      dut.io.launchHeaderSubmit.bits.dmaTag.poke(0x51.U)
      dut.io.launchHeaderSubmit.bits.packetCount.poke(10.U)
      dut.io.launchHeaderSubmit.valid.poke(true.B)
      dut.io.launchHeaderSubmit.ready.expect(true.B)
      dut.clock.step()
      dut.io.launchHeaderSubmit.ready.expect(false.B)
      dut.io.launchHeaderSubmit.valid.poke(false.B)
      dut.io.launchHeaderOut.valid.expect(true.B)
      dut.io.launchHeaderOut.bits.jobId.expect(0x811.U)
      dut.io.launchHeaderOut.bits.packetCount.expect(10.U)
      dut.clock.step(3)
      dut.io.launchHeaderOut.bits.jobId.expect(0x811.U)
      dut.io.launchHeaderOut.bits.packetCount.expect(10.U)
      dut.io.launchHeaderOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.launchHeaderOut.ready.poke(false.B)

      dut.io.launchPacketSubmit.bits.packet.poke(0x123.U)
      dut.io.launchPacketSubmit.valid.poke(true.B)
      dut.io.launchPacketSubmit.ready.expect(true.B)
      dut.clock.step()
      dut.io.launchPacketSubmit.bits.packet.poke(0x456.U)
      dut.io.launchPacketSubmit.ready.expect(true.B)
      dut.clock.step()
      dut.io.launchPacketSubmit.bits.packet.poke(0x789.U)
      dut.io.launchPacketSubmit.ready.expect(false.B)
      dut.io.launchPacketOut.valid.expect(true.B)
      dut.io.launchPacketOut.bits.packet.expect(0x123.U)
      dut.clock.step(3)
      dut.io.launchPacketOut.bits.packet.expect(0x123.U)
      dut.io.launchPacketOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.launchPacketOut.bits.packet.expect(0x456.U)
      dut.io.launchPacketSubmit.ready.expect(true.B)
      dut.clock.step()
      dut.io.launchPacketSubmit.valid.poke(false.B)
      dut.io.launchPacketOut.bits.packet.expect(0x789.U)
      dut.clock.step()
      dut.io.launchPacketOut.valid.expect(false.B)
      dut.io.launchPacketOut.ready.poke(false.B)

      dut.io.launchResultIn.bits.jobId.poke(0x821.U)
      dut.io.launchResultIn.bits.slot.poke(0.U)
      dut.io.launchResultIn.bits.requestedBytes.poke(128.U)
      dut.io.launchResultIn.bits.actualBytes.poke(128.U)
      dut.io.launchResultIn.bits.spmWordAddress.poke(0.U)
      dut.io.launchResultIn.bits.dmaTag.poke(0x61.U)
      dut.io.launchResultIn.bits.packetCount.poke(10.U)
      dut.io.launchResultIn.bits.status.poke(CgraLaunchStatus.LaunchAccepted)
      dut.io.launchResultIn.valid.poke(true.B)
      dut.io.launchResultIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.launchResultIn.bits.jobId.poke(0x822.U)
      dut.io.launchResultIn.bits.dmaTag.poke(0x62.U)
      dut.io.launchResultIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.launchResultIn.bits.jobId.poke(0x823.U)
      dut.io.launchResultIn.bits.dmaTag.poke(0x63.U)
      dut.io.launchResultIn.ready.expect(false.B)
      dut.io.launchResultOut.valid.expect(true.B)
      dut.io.launchResultOut.bits.jobId.expect(0x821.U)
      dut.clock.step(3)
      dut.io.launchResultOut.bits.jobId.expect(0x821.U)
      dut.io.launchResultOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.launchResultOut.bits.jobId.expect(0x822.U)
      dut.io.launchResultIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.launchResultIn.valid.poke(false.B)
      dut.io.launchResultOut.bits.jobId.expect(0x823.U)
      dut.clock.step()
      dut.io.launchResultOut.valid.expect(false.B)
      dut.io.launchResultOut.ready.poke(false.B)

      dut.io.launchErrorIn.bits.jobId.poke(0x831.U)
      dut.io.launchErrorIn.bits.slot.poke(1.U)
      dut.io.launchErrorIn.bits.requestedBytes.poke(128.U)
      dut.io.launchErrorIn.bits.actualBytes.poke(64.U)
      dut.io.launchErrorIn.bits.spmWordAddress.poke(32.U)
      dut.io.launchErrorIn.bits.dmaTag.poke(0x71.U)
      dut.io.launchErrorIn.bits.operation.poke(CgraLaunchError.Operation.Completion)
      dut.io.launchErrorIn.bits.reason.poke(CgraLaunchError.Reason.LengthMismatch)
      dut.io.launchErrorIn.valid.poke(true.B)
      dut.io.launchErrorIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.launchErrorIn.valid.poke(false.B)
      dut.io.launchErrorOut.valid.expect(true.B)
      dut.io.launchErrorOut.bits.jobId.expect(0x831.U)
      dut.io.launchErrorOut.bits.operation.expect(CgraLaunchError.Operation.Completion)
      dut.io.launchErrorOut.bits.reason.expect(CgraLaunchError.Reason.LengthMismatch)
      dut.clock.step(3)
      dut.io.launchErrorOut.bits.jobId.expect(0x831.U)
      dut.io.launchErrorOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.launchErrorOut.ready.poke(false.B)

      dut.io.computeErrorIn.bits.jobId.poke(0x841.U)
      dut.io.computeErrorIn.bits.slot.poke(0.U)
      dut.io.computeErrorIn.bits.requestedBytes.poke(128.U)
      dut.io.computeErrorIn.bits.actualBytes.poke(128.U)
      dut.io.computeErrorIn.bits.spmWordAddress.poke(0.U)
      dut.io.computeErrorIn.bits.dmaTag.poke(0x81.U)
      dut.io.computeErrorIn.bits.operation.poke(
        CgraComputeCompletionError.Operation.Complete)
      dut.io.computeErrorIn.bits.reason.poke(
        CgraComputeCompletionError.Reason.DuplicateComplete)
      dut.io.computeErrorIn.valid.poke(true.B)
      dut.io.computeErrorIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.computeErrorIn.valid.poke(false.B)
      dut.io.computeErrorOut.valid.expect(true.B)
      dut.io.computeErrorOut.bits.jobId.expect(0x841.U)
      dut.io.computeErrorOut.bits.operation.expect(
        CgraComputeCompletionError.Operation.Complete)
      dut.io.computeErrorOut.bits.reason.expect(
        CgraComputeCompletionError.Reason.DuplicateComplete)
      dut.clock.step(3)
      dut.io.computeErrorOut.bits.jobId.expect(0x841.U)
      dut.io.computeErrorOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.computeErrorOut.valid.expect(false.B)
    }
  }

  it should "reject full-width submission fields instead of truncating them" in {
    test(new CgraTransferControlQueues(params)) { dut =>
      initialize(dut)

      def submitInvalidPull(spm: BigInt, tag: BigInt): Unit = {
        dut.io.descriptorSubmit.bits.jobId.poke(0x851.U)
        dut.io.descriptorSubmit.bits.slot.poke(0.U)
        dut.io.descriptorSubmit.bits.bytes.poke(128.U)
        dut.io.descriptorSubmit.bits.spmWordAddress.poke(spm.U)
        dut.io.descriptorSubmit.bits.dmaTag.poke(tag.U)
        dut.io.descriptorSubmit.valid.poke(true.B)
        dut.io.descriptorSubmit.ready.expect(true.B)
        dut.clock.step()
        dut.io.descriptorSubmit.valid.poke(false.B)
        dut.io.descriptorOut.valid.expect(false.B)
        dut.clock.step()
        dut.io.launchErrorOut.valid.expect(true.B)
        dut.io.launchErrorOut.bits.operation.expect(
          CgraLaunchError.Operation.Pull)
        dut.io.launchErrorOut.bits.reason.expect(
          CgraLaunchError.Reason.FieldOutOfRange)
        dut.io.launchErrorOut.bits.spmWordAddress.expect(spm.U)
        dut.io.launchErrorOut.bits.dmaTag.expect(tag.U)
        dut.io.launchErrorOut.ready.poke(true.B)
        dut.clock.step()
        dut.io.launchErrorOut.ready.poke(false.B)
      }

      def submitInvalidHeader(
        spm: BigInt,
        tag: BigInt,
        packets: BigInt): Unit = {
        dut.io.launchHeaderSubmit.bits.jobId.poke(0x852.U)
        dut.io.launchHeaderSubmit.bits.slot.poke(0.U)
        dut.io.launchHeaderSubmit.bits.bytes.poke(128.U)
        dut.io.launchHeaderSubmit.bits.spmWordAddress.poke(spm.U)
        dut.io.launchHeaderSubmit.bits.dmaTag.poke(tag.U)
        dut.io.launchHeaderSubmit.bits.packetCount.poke(packets.U)
        dut.io.launchHeaderSubmit.valid.poke(true.B)
        dut.io.launchHeaderSubmit.ready.expect(true.B)
        dut.clock.step()
        dut.io.launchHeaderSubmit.valid.poke(false.B)
        dut.io.launchHeaderOut.valid.expect(false.B)
        dut.clock.step()
        dut.io.launchErrorOut.valid.expect(true.B)
        dut.io.launchErrorOut.bits.operation.expect(
          CgraLaunchError.Operation.Header)
        dut.io.launchErrorOut.bits.reason.expect(
          CgraLaunchError.Reason.FieldOutOfRange)
        dut.io.launchErrorOut.bits.spmWordAddress.expect(spm.U)
        dut.io.launchErrorOut.bits.dmaTag.expect(tag.U)
        dut.io.launchErrorOut.ready.poke(true.B)
        dut.clock.step()
        dut.io.launchErrorOut.ready.poke(false.B)
      }

      submitInvalidPull(128, 0x71)
      submitInvalidPull(0, 0x171)
      submitInvalidHeader(128, 0x71, 10)
      submitInvalidHeader(0, 0x171, 10)
      submitInvalidHeader(0, 0x71, 33)

      dut.io.descriptorSubmit.bits.jobId.poke(0x853.U)
      dut.io.descriptorSubmit.bits.slot.poke(0.U)
      dut.io.descriptorSubmit.bits.bytes.poke(128.U)
      dut.io.descriptorSubmit.bits.spmWordAddress.poke(0.U)
      dut.io.descriptorSubmit.bits.dmaTag.poke(0x71.U)
      dut.io.descriptorSubmit.valid.poke(true.B)
      dut.io.descriptorSubmit.ready.expect(true.B)
      dut.clock.step()
      dut.io.descriptorSubmit.valid.poke(false.B)
      dut.io.descriptorOut.valid.expect(true.B)
      dut.io.descriptorOut.bits.jobId.expect(0x853.U)

      dut.io.launchHeaderSubmit.bits.jobId.poke(0x853.U)
      dut.io.launchHeaderSubmit.bits.slot.poke(0.U)
      dut.io.launchHeaderSubmit.bits.bytes.poke(128.U)
      dut.io.launchHeaderSubmit.bits.spmWordAddress.poke(0.U)
      dut.io.launchHeaderSubmit.bits.dmaTag.poke(0x71.U)
      dut.io.launchHeaderSubmit.bits.packetCount.poke(10.U)
      dut.io.launchHeaderSubmit.valid.poke(true.B)
      dut.io.launchHeaderSubmit.ready.expect(true.B)
      dut.clock.step()
      dut.io.launchHeaderSubmit.valid.poke(false.B)
      dut.io.launchHeaderOut.valid.expect(true.B)
      dut.io.launchHeaderOut.bits.jobId.expect(0x853.U)
    }
  }

}
