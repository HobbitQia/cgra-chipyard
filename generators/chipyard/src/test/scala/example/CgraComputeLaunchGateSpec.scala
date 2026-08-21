package chipyard.example

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CgraComputeLaunchGateSpec extends AnyFlatSpec
    with ChiselScalatestTester {
  private val params = CgraComputeLaunchGateParams.production

  private def initialize(dut: CgraComputeLaunchGate): Unit = {
    dut.io.headerIn.valid.poke(false.B)
    dut.io.packetIn.valid.poke(false.B)
    dut.io.completionIn.valid.poke(false.B)
    dut.io.packetOut.ready.poke(false.B)
    dut.io.resultOut.ready.poke(false.B)
    dut.io.errorOut.ready.poke(false.B)
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
    dut.clock.step()
  }

  private def waitFor(
    dut: CgraComputeLaunchGate,
    signal: Bool,
    cycles: Int = 40): Unit = {
    var remaining = cycles
    while (signal.peek().litValue == 0 && remaining > 0) {
      dut.clock.step()
      remaining -= 1
    }
    assert(signal.peek().litValue == 1,
      s"signal did not assert within $cycles cycles")
  }

  private def makePacket(seed: BigInt, command: Int = params.launchCommand): BigInt = {
    val packetMask = (BigInt(1) << params.packetWidth) - 1
    val commandMask = ((BigInt(1) << params.packetCommandWidth) - 1) <<
      params.packetCommandLsb
    ((seed & packetMask & ~commandMask) |
      (BigInt(command) << params.packetCommandLsb)) & packetMask
  }

  private def sendHeader(
    dut: CgraComputeLaunchGate,
    jobId: BigInt,
    slot: BigInt,
    bytes: BigInt,
    spmWordAddress: BigInt,
    tag: BigInt,
    packetCount: BigInt): Unit = {
    dut.io.headerIn.bits.jobId.poke(jobId.U)
    dut.io.headerIn.bits.slot.poke(slot.U)
    dut.io.headerIn.bits.bytes.poke(bytes.U)
    dut.io.headerIn.bits.spmWordAddress.poke(spmWordAddress.U)
    dut.io.headerIn.bits.dmaTag.poke(tag.U)
    dut.io.headerIn.bits.packetCount.poke(packetCount.U)
    dut.io.headerIn.valid.poke(true.B)
    waitFor(dut, dut.io.headerIn.ready)
    dut.clock.step()
    dut.io.headerIn.valid.poke(false.B)
  }

  private def sendPacket(dut: CgraComputeLaunchGate, packet: BigInt): Unit = {
    dut.io.packetIn.bits.packet.poke(packet.U)
    dut.io.packetIn.valid.poke(true.B)
    waitFor(dut, dut.io.packetIn.ready)
    dut.clock.step()
    dut.io.packetIn.valid.poke(false.B)
  }

  private def sendCompletion(
    dut: CgraComputeLaunchGate,
    jobId: BigInt,
    slot: BigInt,
    requestedBytes: BigInt,
    actualBytes: BigInt,
    spmWordAddress: BigInt,
    tag: BigInt,
    consumerStatus: BigInt = 0,
    producerStatus: BigInt = 0): Unit = {
    dut.io.completionIn.bits.jobId.poke(jobId.U)
    dut.io.completionIn.bits.slot.poke(slot.U)
    dut.io.completionIn.bits.requestedBytes.poke(requestedBytes.U)
    dut.io.completionIn.bits.actualBytes.poke(actualBytes.U)
    dut.io.completionIn.bits.spmWordAddress.poke(spmWordAddress.U)
    dut.io.completionIn.bits.dmaTag.poke(tag.U)
    dut.io.completionIn.bits.consumerStatus.poke(consumerStatus.U)
    dut.io.completionIn.bits.producerStatus.poke(producerStatus.U)
    dut.io.completionIn.valid.poke(true.B)
    waitFor(dut, dut.io.completionIn.ready)
    dut.clock.step()
    dut.io.completionIn.valid.poke(false.B)
  }

  private def acceptResult(
    dut: CgraComputeLaunchGate,
    jobId: BigInt,
    slot: BigInt,
    requestedBytes: BigInt,
    actualBytes: BigInt,
    spmWordAddress: BigInt,
    tag: BigInt,
    packetCount: BigInt,
    status: UInt): Unit = {
    waitFor(dut, dut.io.resultOut.valid)
    dut.io.resultOut.bits.jobId.expect(jobId.U)
    dut.io.resultOut.bits.slot.expect(slot.U)
    dut.io.resultOut.bits.requestedBytes.expect(requestedBytes.U)
    dut.io.resultOut.bits.actualBytes.expect(actualBytes.U)
    dut.io.resultOut.bits.spmWordAddress.expect(spmWordAddress.U)
    dut.io.resultOut.bits.dmaTag.expect(tag.U)
    dut.io.resultOut.bits.packetCount.expect(packetCount.U)
    dut.io.resultOut.bits.status.expect(status)
    val heldJob = dut.io.resultOut.bits.jobId.peek().litValue
    val heldStatus = dut.io.resultOut.bits.status.peek().litValue
    dut.clock.step(3)
    dut.io.resultOut.valid.expect(true.B)
    assert(dut.io.resultOut.bits.jobId.peek().litValue == heldJob)
    assert(dut.io.resultOut.bits.status.peek().litValue == heldStatus)
    dut.io.resultOut.ready.poke(true.B)
    dut.clock.step()
    dut.io.resultOut.ready.poke(false.B)
  }

  private def acceptError(
    dut: CgraComputeLaunchGate,
    operation: UInt,
    reason: UInt,
    requestedBytes: Option[BigInt] = None,
    actualBytes: Option[BigInt] = None): Unit = {
    waitFor(dut, dut.io.errorOut.valid)
    dut.io.errorOut.bits.operation.expect(operation)
    dut.io.errorOut.bits.reason.expect(reason)
    requestedBytes.foreach { bytes =>
      dut.io.errorOut.bits.requestedBytes.expect(bytes.U)
    }
    actualBytes.foreach { bytes =>
      dut.io.errorOut.bits.actualBytes.expect(bytes.U)
    }
    val heldError = Seq(
      dut.io.errorOut.bits.jobId.peek().litValue,
      dut.io.errorOut.bits.slot.peek().litValue,
      dut.io.errorOut.bits.requestedBytes.peek().litValue,
      dut.io.errorOut.bits.actualBytes.peek().litValue,
      dut.io.errorOut.bits.spmWordAddress.peek().litValue,
      dut.io.errorOut.bits.dmaTag.peek().litValue,
      dut.io.errorOut.bits.operation.peek().litValue,
      dut.io.errorOut.bits.reason.peek().litValue)
    dut.clock.step(2)
    dut.io.errorOut.valid.expect(true.B)
    assert(Seq(
      dut.io.errorOut.bits.jobId.peek().litValue,
      dut.io.errorOut.bits.slot.peek().litValue,
      dut.io.errorOut.bits.requestedBytes.peek().litValue,
      dut.io.errorOut.bits.actualBytes.peek().litValue,
      dut.io.errorOut.bits.spmWordAddress.peek().litValue,
      dut.io.errorOut.bits.dmaTag.peek().litValue,
      dut.io.errorOut.bits.operation.peek().litValue,
      dut.io.errorOut.bits.reason.peek().litValue) == heldError)
    dut.io.errorOut.ready.poke(true.B)
    dut.clock.step()
    dut.io.errorOut.ready.poke(false.B)
  }

  private def collectPackets(
    dut: CgraComputeLaunchGate,
    expected: Seq[BigInt],
    stallFirst: Boolean = false): Unit = {
    waitFor(dut, dut.io.packetOut.valid)
    if (stallFirst) {
      val held = dut.io.packetOut.bits.packet.peek().litValue
      dut.clock.step(3)
      dut.io.packetOut.valid.expect(true.B)
      assert(dut.io.packetOut.bits.packet.peek().litValue == held)
    }
    dut.io.packetOut.ready.poke(true.B)
    expected.foreach { packet =>
      waitFor(dut, dut.io.packetOut.valid)
      dut.io.packetOut.bits.packet.expect(packet.U)
      dut.clock.step()
    }
    dut.io.packetOut.ready.poke(false.B)
  }

  behavior of "CgraComputeLaunchGate"

  it should "gate matching 64-byte and 128-byte sequences for both slots exactly once" in {
    test(new CgraComputeLaunchGate(params)) { dut =>
      initialize(dut)

      val slot0Packets = Seq(
        makePacket(BigInt("123456789abcdef", 16)),
        makePacket(BigInt("123456789abcdef012345", 16)))
      sendHeader(dut, 0x101, 0, 64, 0, 0x21, slot0Packets.size)
      slot0Packets.foreach(sendPacket(dut, _))
      dut.clock.step(3)
      dut.io.packetOut.valid.expect(false.B)
      sendCompletion(dut, 0x101, 0, 64, 64, 0, 0x21)
      collectPackets(dut, slot0Packets, stallFirst = true)
      acceptResult(dut, 0x101, 0, 64, 64, 0, 0x21, slot0Packets.size,
        CgraLaunchStatus.LaunchAccepted)

      val slot1Packets = Seq.tabulate(3) { index =>
        makePacket(BigInt("1f000000000000000000000000000000000000000", 16) + index)
      }
      // Completion-first is legal and retained until the complete sequence.
      sendCompletion(dut, 0x102, 1, 128, 128, 32, 0x22)
      // The independent async queues may present the next header and its first
      // packet together. Even after a prior sequence changed RR history, the
      // header must establish context before that packet is accepted.
      dut.io.headerIn.bits.jobId.poke(0x102.U)
      dut.io.headerIn.bits.slot.poke(1.U)
      dut.io.headerIn.bits.bytes.poke(128.U)
      dut.io.headerIn.bits.spmWordAddress.poke(32.U)
      dut.io.headerIn.bits.dmaTag.poke(0x22.U)
      dut.io.headerIn.bits.packetCount.poke(slot1Packets.size.U)
      dut.io.packetIn.bits.packet.poke(slot1Packets.head.U)
      dut.io.headerIn.valid.poke(true.B)
      dut.io.packetIn.valid.poke(true.B)
      dut.io.headerIn.ready.expect(true.B)
      dut.io.packetIn.ready.expect(false.B)
      dut.clock.step()
      dut.io.headerIn.valid.poke(false.B)
      dut.io.packetIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.packetIn.valid.poke(false.B)
      slot1Packets.drop(1).foreach(sendPacket(dut, _))
      collectPackets(dut, slot1Packets)
      acceptResult(dut, 0x102, 1, 128, 128, 32, 0x22,
        slot1Packets.size,
        CgraLaunchStatus.LaunchAccepted)
      dut.io.acceptedPacketCount.expect(5.U)
      dut.io.acceptedSequenceCount.expect(2.U)
      dut.io.packetOut.valid.expect(false.B)
    }
  }

  it should "reject mismatches, duplicates, and failed completions without launching" in {
    test(new CgraComputeLaunchGate(params)) { dut =>
      initialize(dut)
      val packets = Seq(makePacket(0x100), makePacket(0x200))
      sendHeader(dut, 0x201, 0, 64, 0, 0x31, packets.size)

      sendHeader(dut, 0x201, 0, 64, 0, 0x31, packets.size)
      acceptError(dut, CgraLaunchError.Operation.Header,
        CgraLaunchError.Reason.DuplicateEvent)

      packets.foreach(sendPacket(dut, _))
      sendCompletion(dut, 0x202, 0, 64, 64, 0, 0x31)
      acceptError(dut, CgraLaunchError.Operation.Completion,
        CgraLaunchError.Reason.IdentityMismatch)
      dut.io.packetOut.valid.expect(false.B)

      sendCompletion(dut, 0x201, 1, 64, 64, 0, 0x31)
      acceptError(dut, CgraLaunchError.Operation.Completion,
        CgraLaunchError.Reason.IdentityMismatch)
      sendCompletion(dut, 0x201, 0, 64, 128, 0, 0x31)
      acceptError(dut, CgraLaunchError.Operation.Completion,
        CgraLaunchError.Reason.LengthMismatch)
      sendCompletion(dut, 0x201, 0, 64, 64, 16, 0x31)
      acceptError(dut, CgraLaunchError.Operation.Completion,
        CgraLaunchError.Reason.IdentityMismatch)
      sendCompletion(dut, 0x201, 0, 64, 64, 0, 0x32)
      acceptError(dut, CgraLaunchError.Operation.Completion,
        CgraLaunchError.Reason.IdentityMismatch)
      sendCompletion(dut, 0x201, 0, 128, 0, 0, 0x31,
        consumerStatus = 5)
      acceptError(dut, CgraLaunchError.Operation.Completion,
        CgraLaunchError.Reason.IdentityMismatch,
        requestedBytes = Some(128), actualBytes = Some(0))
      dut.io.packetOut.valid.expect(false.B)

      sendCompletion(dut, 0x201, 0, 64, 64, 0, 0x31,
        consumerStatus = 5,
        producerStatus = 0)
      dut.io.packetOut.valid.expect(false.B)
      acceptResult(dut, 0x201, 0, 64, 64, 0, 0x31, packets.size,
        CgraLaunchStatus.ConsumerFailure)
      dut.io.acceptedPacketCount.expect(0.U)

      sendHeader(dut, 0x202, 0, 64, 16, 0x33, 1)
      sendPacket(dut, makePacket(0x250))
      sendCompletion(dut, 0x202, 0, 64, 0, 16, 0x33,
        consumerStatus = 0,
        producerStatus = 7)
      dut.io.packetOut.valid.expect(false.B)
      acceptResult(dut, 0x202, 0, 64, 0, 16, 0x33, 1,
        CgraLaunchStatus.ProducerFailure)
      dut.io.acceptedPacketCount.expect(0.U)

      sendCompletion(dut, 0x203, 1, 128, 128, 32, 0x32)
      sendCompletion(dut, 0x203, 1, 128, 128, 32, 0x32)
      acceptError(dut, CgraLaunchError.Operation.Completion,
        CgraLaunchError.Reason.DuplicateEvent)
      // A wrong intent is rejected while the completion remains retained.
      sendHeader(dut, 0x204, 1, 128, 32, 0x32, 1)
      acceptResult(dut, 0x204, 1, 128, 128, 32, 0x32, 1,
        CgraLaunchStatus.IdentityMismatch)
      sendHeader(dut, 0x203, 1, 128, 32, 0x32, 1)
      sendPacket(dut, makePacket(0x300))
      collectPackets(dut, Seq(makePacket(0x300)))
      acceptResult(dut, 0x203, 1, 128, 128, 32, 0x32, 1,
        CgraLaunchStatus.LaunchAccepted)

      sendCompletion(dut, 0, 0, 64, 64, 0, 0x34)
      acceptError(dut, CgraLaunchError.Operation.Completion,
        CgraLaunchError.Reason.MalformedCompletion)
      sendCompletion(dut, 0x206, 0, 64, 1, 0, 0x36,
        consumerStatus = 5)
      acceptError(dut, CgraLaunchError.Operation.Completion,
        CgraLaunchError.Reason.MalformedCompletion,
        requestedBytes = Some(64), actualBytes = Some(1))

      // A retained failure is associated by requested length, even though its
      // actual length is zero. A stale intent cannot consume that failure.
      sendCompletion(dut, 0x205, 0, 64, 0, 32, 0x35,
        producerStatus = 7)
      sendHeader(dut, 0x205, 0, 128, 32, 0x35, 1)
      acceptResult(dut, 0x205, 0, 128, 128, 32, 0x35, 1,
        CgraLaunchStatus.IdentityMismatch)
      sendHeader(dut, 0x205, 0, 64, 32, 0x35, 1)
      sendPacket(dut, makePacket(0x350))
      dut.io.packetOut.valid.expect(false.B)
      acceptResult(dut, 0x205, 0, 64, 0, 32, 0x35, 1,
        CgraLaunchStatus.ProducerFailure)
    }
  }

  it should "return typed failures for malformed headers and non-launch packets" in {
    test(new CgraComputeLaunchGate(params)) { dut =>
      initialize(dut)
      val malformed = Seq(
        (BigInt(0), BigInt(0), BigInt(64), BigInt(0), BigInt(1),
          CgraLaunchStatus.InvalidJob),
        (BigInt(1), BigInt(2), BigInt(64), BigInt(0), BigInt(1),
          CgraLaunchStatus.InvalidSlot),
        (BigInt(2), BigInt(0), BigInt(16), BigInt(0), BigInt(1),
          CgraLaunchStatus.InvalidLength),
        (BigInt(3), BigInt(0), BigInt(64), BigInt(120), BigInt(1),
          CgraLaunchStatus.SpmRange),
        (BigInt(4), BigInt(0), BigInt(64), BigInt(0), BigInt(0),
          CgraLaunchStatus.InvalidPacketCount),
        (BigInt(5), BigInt(0), BigInt(64), BigInt(0), BigInt(17),
          CgraLaunchStatus.InvalidPacketCount))
      malformed.foreach { case (job, slot, bytes, spm, count, status) =>
        sendHeader(dut, job, slot, bytes, spm, 0x41, count)
        acceptResult(dut, job, slot, bytes, bytes, spm, 0x41, count, status)
      }

      sendHeader(dut, 0x301, 0, 64, 0, 0x42, 1)
      sendPacket(dut, makePacket(0x1234, CGRACmdGenerated.CMD_CONFIG))
      acceptError(dut, CgraLaunchError.Operation.Packet,
        CgraLaunchError.Reason.NonLaunchPacket)
      acceptResult(dut, 0x301, 0, 64, 64, 0, 0x42, 1,
        CgraLaunchStatus.InvalidPacket)
      dut.io.packetOut.valid.expect(false.B)
    }
  }

  it should "retain and drain the full parameterized sixteen-packet bound" in {
    test(new CgraComputeLaunchGate(params)) { dut =>
      initialize(dut)
      val packets = Seq.tabulate(params.packetCapacity) { index =>
        makePacket((BigInt(index + 1) << 150) | BigInt(index))
      }
      sendHeader(dut, 0x401, 1, 64, 64, 0x51, packets.size)
      packets.foreach(sendPacket(dut, _))
      sendCompletion(dut, 0x401, 1, 64, 64, 64, 0x51)
      collectPackets(dut, packets)
      acceptResult(dut, 0x401, 1, 64, 64, 64, 0x51, packets.size,
        CgraLaunchStatus.LaunchAccepted)
      dut.io.acceptedPacketCount.expect(params.packetCapacity.U)
    }
  }

  it should "serialize simultaneous rejects behind a stable lossless error output" in {
    test(new CgraComputeLaunchGate(params)) { dut =>
      initialize(dut)
      assert(dut.io.errorOut.bits.requestedBytes.getWidth ==
        SpmTransferProtocol.LengthWidth)
      def errorSnapshot: Seq[BigInt] = Seq(
        dut.io.errorOut.bits.jobId.peek().litValue,
        dut.io.errorOut.bits.slot.peek().litValue,
        dut.io.errorOut.bits.requestedBytes.peek().litValue,
        dut.io.errorOut.bits.actualBytes.peek().litValue,
        dut.io.errorOut.bits.spmWordAddress.peek().litValue,
        dut.io.errorOut.bits.dmaTag.peek().litValue,
        dut.io.errorOut.bits.operation.peek().litValue,
        dut.io.errorOut.bits.reason.peek().litValue)
      sendHeader(dut, 0x501, 0, 64, 0, 0x61, 1)

      dut.io.headerIn.bits.jobId.poke(0x501.U)
      dut.io.headerIn.bits.slot.poke(0.U)
      dut.io.headerIn.bits.bytes.poke(64.U)
      dut.io.headerIn.bits.spmWordAddress.poke(0.U)
      dut.io.headerIn.bits.dmaTag.poke(0x61.U)
      dut.io.headerIn.bits.packetCount.poke(1.U)
      dut.io.packetIn.bits.packet.poke(
        makePacket(0x501, CGRACmdGenerated.CMD_CONFIG).U)
      dut.io.headerIn.valid.poke(true.B)
      dut.io.packetIn.valid.poke(true.B)
      val headerWon = dut.io.headerIn.ready.peek().litToBoolean
      val packetWon = dut.io.packetIn.ready.peek().litToBoolean
      assert(headerWon ^ packetWon)
      dut.clock.step()
      if (headerWon) dut.io.headerIn.valid.poke(false.B)
      else dut.io.packetIn.valid.poke(false.B)

      waitFor(dut, dut.io.errorOut.valid)
      val firstOperation = dut.io.errorOut.bits.operation.peek().litValue
      val firstReason = dut.io.errorOut.bits.reason.peek().litValue
      val heldError = errorSnapshot
      dut.clock.step(3)
      dut.io.errorOut.valid.expect(true.B)
      assert(errorSnapshot == heldError)
      if (headerWon) dut.io.packetIn.ready.expect(false.B)
      else dut.io.headerIn.ready.expect(false.B)

      dut.io.errorOut.ready.poke(true.B)
      if (headerWon) dut.io.packetIn.ready.expect(true.B)
      else dut.io.headerIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.errorOut.ready.poke(false.B)
      if (headerWon) dut.io.packetIn.valid.poke(false.B)
      else dut.io.headerIn.valid.poke(false.B)

      waitFor(dut, dut.io.errorOut.valid)
      val secondOperation = dut.io.errorOut.bits.operation.peek().litValue
      val secondReason = dut.io.errorOut.bits.reason.peek().litValue
      assert(Set(firstOperation, secondOperation) == Set(
        CgraLaunchError.Operation.Header.litValue,
        CgraLaunchError.Operation.Packet.litValue))
      assert(Set(firstReason, secondReason) == Set(
        CgraLaunchError.Reason.DuplicateEvent.litValue,
        CgraLaunchError.Reason.NonLaunchPacket.litValue))
      dut.io.errorOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.errorOut.ready.poke(false.B)
      acceptResult(dut, 0x501, 0, 64, 64, 0, 0x61, 1,
        CgraLaunchStatus.InvalidPacket)
      dut.io.acceptedPacketCount.expect(0.U)

      dut.io.packetIn.bits.packet.poke(makePacket(0x502).U)
      dut.io.completionIn.bits.jobId.poke(0.U)
      dut.io.completionIn.bits.slot.poke(0.U)
      dut.io.completionIn.bits.requestedBytes.poke(64.U)
      dut.io.completionIn.bits.actualBytes.poke(64.U)
      dut.io.completionIn.bits.spmWordAddress.poke(0.U)
      dut.io.completionIn.bits.dmaTag.poke(0x62.U)
      dut.io.completionIn.bits.consumerStatus.poke(0.U)
      dut.io.completionIn.bits.producerStatus.poke(0.U)
      dut.io.packetIn.valid.poke(true.B)
      dut.io.completionIn.valid.poke(true.B)
      val packetWonSecond = dut.io.packetIn.ready.peek().litToBoolean
      val completionWon = dut.io.completionIn.ready.peek().litToBoolean
      assert(packetWonSecond ^ completionWon)
      dut.clock.step()
      if (packetWonSecond) dut.io.packetIn.valid.poke(false.B)
      else dut.io.completionIn.valid.poke(false.B)

      waitFor(dut, dut.io.errorOut.valid)
      val thirdOperation = dut.io.errorOut.bits.operation.peek().litValue
      val thirdReason = dut.io.errorOut.bits.reason.peek().litValue
      val heldSecondPair = errorSnapshot
      dut.clock.step(3)
      assert(errorSnapshot == heldSecondPair)
      if (packetWonSecond) dut.io.completionIn.ready.expect(false.B)
      else dut.io.packetIn.ready.expect(false.B)

      dut.io.errorOut.ready.poke(true.B)
      if (packetWonSecond) dut.io.completionIn.ready.expect(true.B)
      else dut.io.packetIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.errorOut.ready.poke(false.B)
      if (packetWonSecond) dut.io.completionIn.valid.poke(false.B)
      else dut.io.packetIn.valid.poke(false.B)

      waitFor(dut, dut.io.errorOut.valid)
      val fourthOperation = dut.io.errorOut.bits.operation.peek().litValue
      val fourthReason = dut.io.errorOut.bits.reason.peek().litValue
      assert(Set(thirdOperation, fourthOperation) == Set(
        CgraLaunchError.Operation.Packet.litValue,
        CgraLaunchError.Operation.Completion.litValue))
      assert(Set(thirdReason, fourthReason) == Set(
        CgraLaunchError.Reason.UnexpectedEvent.litValue,
        CgraLaunchError.Reason.MalformedCompletion.litValue))
      dut.io.errorOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.errorOut.ready.poke(false.B)
      dut.io.errorOut.valid.expect(false.B)
      dut.io.packetOut.valid.expect(false.B)
    }
  }
}

class CgraPacketFifoInputArbiterSpec extends AnyFlatSpec
    with ChiselScalatestTester {
  behavior of "CgraPacketFifoInputArbiter"

  it should "preserve DMA atomic priority, then CPU order, then automatic launch" in {
    test(new CgraPacketFifoInputArbiter(32)) { dut =>
      dut.io.dmaPacketIn.valid.poke(false.B)
      dut.io.cpuPacketIn.valid.poke(false.B)
      dut.io.launchPacketIn.valid.poke(false.B)
      dut.io.packetOut.ready.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)

      dut.io.dmaPacketIn.bits.poke("hda000001".U)
      dut.io.cpuPacketIn.bits.poke("hc0000002".U)
      dut.io.launchPacketIn.bits.poke("ha0000003".U)
      dut.io.dmaPacketIn.valid.poke(true.B)
      dut.io.cpuPacketIn.valid.poke(true.B)
      dut.io.launchPacketIn.valid.poke(true.B)
      dut.io.packetOut.valid.expect(true.B)
      dut.io.packetOut.bits.expect("hda000001".U)
      dut.io.dmaPacketIn.ready.expect(false.B)
      dut.io.cpuPacketIn.ready.expect(false.B)
      dut.io.launchPacketIn.ready.expect(false.B)
      dut.clock.step(3)
      dut.io.packetOut.bits.expect("hda000001".U)

      dut.io.packetOut.ready.poke(true.B)
      dut.io.dmaPacketIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.dmaPacketIn.valid.poke(false.B)
      dut.io.packetOut.bits.expect("hc0000002".U)
      dut.io.cpuPacketIn.ready.expect(true.B)
      dut.io.launchPacketIn.ready.expect(false.B)
      dut.clock.step()
      dut.io.cpuPacketIn.valid.poke(false.B)
      dut.io.packetOut.bits.expect("ha0000003".U)
      dut.io.launchPacketIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.launchPacketIn.valid.poke(false.B)
      dut.io.packetOut.valid.expect(false.B)

      // Once a lower-priority packet is blocked, later arrivals cannot change
      // its payload. Priority is reevaluated after the retained packet fires.
      dut.io.packetOut.ready.poke(false.B)
      dut.io.launchPacketIn.bits.poke("ha0000010".U)
      dut.io.launchPacketIn.valid.poke(true.B)
      dut.clock.step()
      dut.io.packetOut.bits.expect("ha0000010".U)
      dut.io.dmaPacketIn.bits.poke("hda000011".U)
      dut.io.cpuPacketIn.bits.poke("hc0000012".U)
      dut.io.dmaPacketIn.valid.poke(true.B)
      dut.io.cpuPacketIn.valid.poke(true.B)
      dut.clock.step(2)
      dut.io.packetOut.bits.expect("ha0000010".U)
      dut.io.dmaPacketIn.ready.expect(false.B)
      dut.io.cpuPacketIn.ready.expect(false.B)

      dut.io.packetOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.launchPacketIn.valid.poke(false.B)
      dut.io.packetOut.bits.expect("hda000011".U)
      dut.clock.step()
      dut.io.dmaPacketIn.valid.poke(false.B)
      dut.io.packetOut.bits.expect("hc0000012".U)
      dut.clock.step()
      dut.io.cpuPacketIn.valid.poke(false.B)
      dut.io.packetOut.valid.expect(false.B)

      // A complete six-packet DMA command remains contiguous while CPU and
      // automatic launch traffic are both pending. The first DMA packet is
      // also held stable through packet-FIFO backpressure.
      dut.io.packetOut.ready.poke(false.B)
      dut.io.cpuPacketIn.bits.poke("hc1000000".U)
      dut.io.launchPacketIn.bits.poke("ha1000000".U)
      dut.io.cpuPacketIn.valid.poke(true.B)
      dut.io.launchPacketIn.valid.poke(true.B)
      dut.io.dmaPacketIn.bits.poke("hda100000".U)
      dut.io.dmaPacketIn.valid.poke(true.B)
      dut.io.packetOut.bits.expect("hda100000".U)
      dut.clock.step(3)
      dut.io.packetOut.bits.expect("hda100000".U)
      dut.io.cpuPacketIn.ready.expect(false.B)
      dut.io.launchPacketIn.ready.expect(false.B)

      dut.io.packetOut.ready.poke(true.B)
      for (index <- 0 until 6) {
        val packet = BigInt("da100000", 16) + index
        dut.io.dmaPacketIn.bits.poke(packet.U)
        dut.io.packetOut.bits.expect(packet.U)
        dut.io.dmaPacketIn.ready.expect(true.B)
        dut.io.cpuPacketIn.ready.expect(false.B)
        dut.io.launchPacketIn.ready.expect(false.B)
        dut.clock.step()
      }
      dut.io.dmaPacketIn.valid.poke(false.B)
      dut.io.packetOut.bits.expect("hc1000000".U)
      dut.io.cpuPacketIn.ready.expect(true.B)
      dut.io.launchPacketIn.ready.expect(false.B)
      dut.clock.step()
      dut.io.cpuPacketIn.valid.poke(false.B)
      dut.io.packetOut.bits.expect("ha1000000".U)
      dut.io.launchPacketIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.launchPacketIn.valid.poke(false.B)
      dut.io.packetOut.valid.expect(false.B)
    }
  }
}
