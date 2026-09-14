package chipyard.socgen.cgra

import chisel3._
import chiseltest._
import chipyard.example.{CGRACmdGenerated, CGRAGenerated}
import chipyard.socgen.link._
import org.scalatest.flatspec.AnyFlatSpec

class CgraTemplateSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val auto = AutoLinkParams(
    stages = Seq(AutoStageSpec("source", "gemmini", 0), AutoStageSpec("sink", "cgra", 0)),
    dependencies = Seq(AutoDependencySpec(Some(0), 1, Some(AutoCopySpec(0, 0, 32)))),
    endpoints = Seq(
      AutoEndpointSpec("gemmini", Some(AutoBuffer(0x60000000L, 256)), 256),
      AutoEndpointSpec("cgra", None, 512, bufferSlots = 3)),
    beatBytes = 16,
    controlAddress = 0x60020000L,
    controlBytes = 4096)
  private val params = CgraLinkParams(auto, CGRAGenerated.params, packetCapacity = 8)
  private val payloadLsb = params.cgra.packetLayout.dataPayloadLsb
  private val payloadMask = ((BigInt(1) << params.cgra.dataPayloadWidth) - 1) << payloadLsb

  private def packet(command: Int, data: Int): BigInt = {
    (BigInt(command) << params.cgra.packetLayout.cmdLsb) |
      (BigInt(data) << payloadLsb) | BigInt(0x35)
  }

  private val template = Seq(
    packet(CGRACmdGenerated.CMD_REARM, 0),
    packet(CGRACmdGenerated.CMD_REARM, 0) ^ 1,
    packet(CGRACmdGenerated.CMD_CONST, 17),
    packet(CGRACmdGenerated.CMD_CONFIG_TOTAL_CTRL_COUNT, 99),
    packet(CGRACmdGenerated.CMD_CONFIG_CTRL_LOWER_BOUND, 0),
    packet(CGRACmdGenerated.CMD_LAUNCH, 0),
    packet(CGRACmdGenerated.CMD_LAUNCH, 0) ^ 1)

  private def init(dut: CgraLinkAdapter): Unit = {
    dut.io.configIn.valid.poke(false.B)
    dut.io.patchIn.valid.poke(false.B)
    dut.io.packetIn.valid.poke(false.B)
    dut.io.configAck.ready.poke(false.B)
    dut.io.autoLink.watchOutput.valid.poke(false.B)
    dut.io.autoLink.reportOutput.ready.poke(true.B)
    dut.io.autoLink.requestCopy.valid.poke(false.B)
    dut.io.autoLink.reportCopy.ready.poke(true.B)
    dut.io.autoLink.requestCompute.valid.poke(false.B)
    dut.io.autoLink.reportCompute.ready.poke(false.B)
    dut.io.dmaRequest.ready.poke(true.B)
    dut.io.dmaCompletion.valid.poke(false.B)
    dut.io.jobPacket.ready.poke(false.B)
    dut.io.computeResult.valid.poke(false.B)
  }

  private def ack(dut: CgraLinkAdapter, done: Boolean): Unit = {
    dut.io.configAck.valid.expect(true.B)
    dut.io.configAck.bits.done.expect(done.B)
    dut.clock.step(2)
    dut.io.configAck.ready.poke(true.B)
    dut.clock.step()
    dut.io.configAck.ready.poke(false.B)
  }

  private def begin(dut: CgraLinkAdapter, packetCount: Int, patchCount: Int, job: Int): Unit = {
    dut.io.configIn.bits.job.poke(job.U)
    dut.io.configIn.bits.packetCount.poke(packetCount.U)
    dut.io.configIn.bits.expectedCompletions.poke(1.U)
    dut.io.configIn.bits.patchCount.poke(patchCount.U)
    dut.io.configIn.valid.poke(true.B)
    dut.io.configIn.ready.expect(true.B)
    dut.clock.step()
    dut.io.configIn.valid.poke(false.B)
    dut.io.captureActive.expect(true.B)
    dut.io.packetIn.ready.expect(false.B)
  }

  private def capture(
    dut: CgraLinkAdapter,
    patches: Seq[(Int, Int, Int, Int)],
    packets: Seq[BigInt] = template,
    job: Int = 0): Unit = {
    begin(dut, packets.size, patches.size, job)
    ack(dut, done = false)
    for ((index, source, coefficient, bias) <- patches) {
      dut.io.packetIn.ready.expect(false.B)
      dut.io.patchIn.ready.expect(true.B)
      dut.io.patchIn.bits.packetIndex.poke(index.U)
      dut.io.patchIn.bits.source.poke(source.U)
      dut.io.patchIn.bits.coefficient.poke(coefficient.U)
      dut.io.patchIn.bits.bias.poke(bias.U)
      dut.io.patchIn.valid.poke(true.B)
      dut.clock.step()
      dut.io.patchIn.valid.poke(false.B)
    }
    for (value <- packets) {
      dut.io.packetIn.ready.expect(true.B)
      dut.io.packetIn.bits.poke(value.U)
      dut.io.packetIn.valid.poke(true.B)
      dut.clock.step()
      dut.io.packetIn.valid.poke(false.B)
    }
    ack(dut, done = true)
    dut.io.captureActive.expect(false.B)
  }

  private def copy(dut: CgraLinkAdapter, elements: Int, slot: Int = 0): Unit = {
    val request = dut.io.autoLink.requestCopy
    request.bits.task.poke(0.U)
    request.bits.job.poke(0.U)
    request.bits.sourceAddress.poke(0x60000000L.U)
    request.bits.destinationSlot.poke(slot.U)
    request.bits.destinationOffset.poke((slot * 128).U)
    request.bits.bytes.poke(elements.U)
    request.bits.destinationBytes.poke((elements * params.wordBytes).U)
    request.valid.poke(true.B)
    request.ready.expect(true.B)
    dut.clock.step()
    request.valid.poke(false.B)
    dut.io.dmaRequest.valid.expect(true.B)
    dut.io.dmaRequest.bits.packed.expect(true.B)
    dut.clock.step()
    dut.io.dmaCompletion.valid.poke(true.B)
    dut.io.dmaCompletion.bits.dmaTag.poke(0.U)
    dut.io.dmaCompletion.ready.expect(true.B)
    dut.clock.step()
    dut.io.dmaCompletion.valid.poke(false.B)
    dut.io.autoLink.reportCopy.valid.expect(true.B)
    dut.io.autoLink.reportCopy.bits.status.expect(AutoLinkStatus.Success)
    dut.clock.step()
  }

  private def start(dut: CgraLinkAdapter, id: Int, slot: Int, job: Int): Unit = {
    dut.io.autoLink.requestCompute.bits.job.poke(job.U)
    dut.io.autoLink.requestCompute.bits.start.poke(true.B)
    dut.io.autoLink.requestCompute.bits.tile.id.poke(id.U)
    dut.io.autoLink.requestCompute.bits.slot.poke(slot.U)
    dut.io.autoLink.requestCompute.bits.tile.row.poke(1.U)
    dut.io.autoLink.requestCompute.bits.tile.column.poke(2.U)
    dut.io.autoLink.requestCompute.bits.tile.rows.poke(2.U)
    dut.io.autoLink.requestCompute.bits.tile.columns.poke(1.U)
    dut.io.autoLink.requestCompute.bits.tile.last.poke(true.B)
    dut.io.autoLink.requestCompute.valid.poke(true.B)
    dut.io.autoLink.requestCompute.ready.expect(true.B)
    dut.clock.step()
    dut.io.autoLink.requestCompute.valid.poke(false.B)
  }

  private def packets(dut: CgraLinkAdapter, expected: Seq[BigInt]): Unit = {
    for (value <- expected) {
      var cycles = 0
      while (!dut.io.jobPacket.valid.peek().litToBoolean && cycles < 8) {
        dut.clock.step()
        cycles += 1
      }
      dut.io.jobPacket.valid.expect(true.B)
      for (_ <- 0 until 3) {
        dut.io.jobPacket.bits.expect(value.U)
        dut.clock.step()
      }
      dut.io.jobPacket.ready.poke(true.B)
      dut.clock.step()
      dut.io.jobPacket.ready.poke(false.B)
    }
    dut.io.computeResult.bits.poke(7.U)
    dut.io.computeResult.valid.poke(true.B)
    dut.io.computeResult.ready.expect(true.B)
    dut.clock.step()
    dut.io.computeResult.valid.poke(false.B)
  }

  private def replay(dut: CgraLinkAdapter, id: Int, expected: Seq[BigInt], slot: Int = 0, job: Int = 0): Unit = {
    start(dut, id, slot, job)
    packets(dut, expected)
  }

  private def report(dut: CgraLinkAdapter, job: Int = 0): Unit = {
    dut.io.autoLink.reportCompute.valid.expect(true.B)
    dut.io.autoLink.reportCompute.bits.job.expect(job.U)
    dut.io.autoLink.reportCompute.bits.status.expect(AutoLinkStatus.Success)
    dut.io.autoLink.reportCompute.bits.data.expect(7.U)
    dut.clock.step(2)
    dut.io.autoLink.reportCompute.ready.poke(true.B)
    dut.clock.step()
    dut.io.autoLink.reportCompute.ready.poke(false.B)
  }

  private def compute(dut: CgraLinkAdapter, id: Int, expected: Seq[BigInt], slot: Int = 0, job: Int = 0): Unit = {
    replay(dut, id, expected, slot, job)
    report(dut, job)
  }

  behavior of "Cgra cached templates"

  it should "copy the next tile during replay without changing the active element count" in {
    test(new CgraLinkAdapter(params)) { dut =>
      init(dut)
      capture(dut, Seq((3, CgraSymbolSource.Elements, 1, 0)))
      copy(dut, 19)
      start(dut, id = 0, slot = 0, job = 0)
      dut.io.computeActive.expect(true.B)
      dut.io.autoLink.requestCompute.ready.expect(false.B)
      dut.io.configIn.ready.expect(false.B)
      copy(dut, 3, slot = 1)
      dut.io.computeActive.expect(true.B)
      val first = template.updated(3, (template(3) & ~payloadMask) | (BigInt(19) << payloadLsb))
      packets(dut, first)
      report(dut)
      val next = template.updated(3, (template(3) & ~payloadMask) | (BigInt(3) << payloadLsb))
      compute(dut, 1, next, slot = 1)
    }
  }

  it should "bind copy counts and slots on each replay, preserve other bits and clear old patches" in {
    test(new CgraLinkAdapter(params)) { dut =>
      init(dut)
      capture(dut, Seq((2, CgraSymbolSource.Slot, 16, 4), (3, CgraSymbolSource.Elements, 5, 10)))
      for ((id, slot, elements) <- Seq((4, 2, 19), (5, 0, 3), (6, 1, 32))) {
        copy(dut, elements)
        val expected = template.zipWithIndex.map { case (value, index) =>
          val payload = if (index == 2) 4 + slot * 16 else elements * 5 + 10
          if (index == 2 || index == 3) (value & ~payloadMask) | (BigInt(payload) << payloadLsb) else value
        }
        compute(dut, id, expected, slot)
      }
      capture(dut, Nil)
      compute(dut, 7, template)
    }
  }

  it should "switch captured runtime jobs A B A with independent counts and patches under backpressure" in {
    val multiJob = auto.copy(
      stages = auto.stages :+ AutoStageSpec("next", "cgra", 1),
      dependencies = auto.dependencies :+ AutoDependencySpec(Some(0), 2, None))
    test(new CgraLinkAdapter(params.copy(auto = multiJob))) { dut =>
      init(dut)
      val second = Seq(
        packet(CGRACmdGenerated.CMD_REARM, 0),
        packet(CGRACmdGenerated.CMD_CONFIG_CTRL_LOWER_BOUND, 5),
        packet(CGRACmdGenerated.CMD_CONST, 53),
        packet(CGRACmdGenerated.CMD_LAUNCH, 0))
      capture(dut, Seq((2, CgraSymbolSource.Slot, 16, 4)))
      capture(dut, Seq((2, CgraSymbolSource.TileId, 3, 7)), second, job = 1)
      for ((job, id, slot) <- Seq((0, 1, 2), (1, 2, 0), (0, 3, 1))) {
        val packets = if (job == 0) template else second
        val payload = if (job == 0) 4 + slot * 16 else 7 + id * 3
        val expected = packets.updated(2,
          (packets(2) & ~payloadMask) | (BigInt(payload) << payloadLsb))
        compute(dut, id, expected, slot, job)
      }
      capture(dut, Nil, second, job = 1)
      compute(dut, 4, second, job = 1)
      val first = template.updated(2, (template(2) & ~payloadMask) | (BigInt(4) << payloadLsb))
      compute(dut, 5, first)
    }
  }

  it should "bind tile identity for a job without incoming data" in {
    val noCopy = auto.copy(
      stages = Seq(AutoStageSpec("sink", "cgra", 0)),
      dependencies = Seq(AutoDependencySpec(None, 0, None)),
      endpoints = Seq(AutoEndpointSpec("cgra", None, 512)))
    test(new CgraLinkAdapter(params.copy(auto = noCopy))) { dut =>
      init(dut)
      capture(dut, Seq((2, CgraSymbolSource.TileId, 16, 4)))
      for (id <- Seq(4, 7)) {
        val expected = template.updated(2,
          (template(2) & ~payloadMask) | (BigInt(4 + id * 16) << payloadLsb))
        compute(dut, id, expected)
      }
      capture(dut, Nil)
      compute(dut, 8, template)
    }
  }

  it should "hold SPM publication and compute completion independently under backpressure" in {
    test(new CgraLinkAdapter(params)) { dut =>
      init(dut)
      capture(dut, Nil)
      for (id <- Seq(4, 5)) {
        dut.io.autoLink.requestCompute.bits.job.poke(0.U)
        dut.io.autoLink.requestCompute.bits.start.poke(false.B)
        dut.io.autoLink.requestCompute.valid.poke(true.B)
        dut.clock.step()
        dut.io.autoLink.requestCompute.valid.poke(false.B)
        dut.io.autoLink.reportCompute.valid.expect(false.B)

        dut.io.autoLink.watchOutput.bits.job.poke(0.U)
        dut.io.autoLink.watchOutput.valid.poke(true.B)
        dut.io.autoLink.reportOutput.ready.poke(false.B)
        dut.clock.step()
        dut.io.autoLink.watchOutput.valid.poke(false.B)
        replay(dut, id, template)
        dut.io.autoLink.reportOutput.valid.expect(true.B)
        dut.io.autoLink.reportOutput.bits.data.expect(7.U)
        for (_ <- 0 until 3) {
          dut.io.computeActive.expect(false.B)
          dut.io.autoLink.reportCompute.valid.expect(true.B)
          dut.io.autoLink.reportOutput.valid.expect(true.B)
          dut.io.autoLink.requestCompute.ready.expect(false.B)
          dut.io.autoLink.requestCopy.ready.expect(true.B)
          dut.io.configIn.ready.expect(false.B)
          dut.clock.step()
        }
        report(dut)
        dut.io.autoLink.reportOutput.valid.expect(true.B)
        dut.io.autoLink.reportOutput.bits.data.expect(7.U)
        dut.io.autoLink.reportOutput.ready.poke(true.B)
        dut.clock.step()
        dut.io.autoLink.reportOutput.valid.expect(false.B)
      }
      capture(dut, Nil)
      compute(dut, 6, template)
    }
  }
}
