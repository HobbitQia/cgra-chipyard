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
      AutoEndpointSpec("cgra", None, 512)),
    beatBytes = 16,
    controlAddress = 0x60020000L,
    controlBytes = 4096,
    bufferSlots = 3)
  private val params = CgraLinkParams(auto, CGRAGenerated.params, packetCapacity = 8, symbolCapacity = 2)
  private val payloadLsb = params.cgra.packetLayout.dataPayloadLsb
  private val payloadMask = ((BigInt(1) << params.cgra.dataPayloadWidth) - 1) << payloadLsb

  private def packet(command: Int, data: Int): BigInt = {
    (BigInt(command) << params.cgra.packetLayout.cmdLsb) |
      (BigInt(data) << payloadLsb) | BigInt(0x35)
  }

  private val template = Seq(
    packet(CGRACmdGenerated.CMD_CONST, 17),
    packet(CGRACmdGenerated.CMD_CONFIG_TOTAL_CTRL_COUNT, 99),
    packet(CGRACmdGenerated.CMD_LAUNCH, 0))

  private def init(dut: CgraLinkAdapter): Unit = {
    dut.io.configIn.valid.poke(false.B)
    dut.io.symbolIn.valid.poke(false.B)
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
    dut.io.resetRequest.ready.poke(true.B)
    dut.io.computeResult.valid.poke(false.B)
  }

  private def ack(dut: CgraLinkAdapter, done: Boolean, detail: Int): Unit = {
    dut.io.configAck.valid.expect(true.B)
    dut.io.configAck.bits.done.expect(done.B)
    dut.io.configAck.bits.detail.expect(detail.U)
    dut.io.configAck.bits.status.expect(if (detail == 0) AutoLinkStatus.Success else AutoLinkStatus.SinkFailure)
    dut.clock.step(2)
    dut.io.configAck.ready.poke(true.B)
    dut.clock.step()
    dut.io.configAck.ready.poke(false.B)
  }

  private def begin(dut: CgraLinkAdapter, symbolCount: Int, patchCount: Int): Unit = {
    dut.io.configIn.bits.job.poke(0.U)
    dut.io.configIn.bits.packetCount.poke(template.size.U)
    dut.io.configIn.bits.expectedCompletions.poke(1.U)
    dut.io.configIn.bits.symbolCount.poke(symbolCount.U)
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
    symbols: Seq[(Int, Int, Int)],
    patches: Seq[(Int, Int, Int, Int)],
    detail: Int = 0): Unit = {
    begin(dut, symbols.size, patches.size)
    ack(dut, done = false, detail = 0)
    for ((base, stride, source) <- symbols) {
      dut.io.packetIn.ready.expect(false.B)
      dut.io.symbolIn.ready.expect(true.B)
      dut.io.symbolIn.bits.base.poke(base.U)
      dut.io.symbolIn.bits.stride.poke(stride.U)
      dut.io.symbolIn.bits.source.poke(source.U)
      dut.io.symbolIn.valid.poke(true.B)
      dut.clock.step()
      dut.io.symbolIn.valid.poke(false.B)
    }
    for ((index, symbol, scale, offset) <- patches) {
      dut.io.packetIn.ready.expect(false.B)
      dut.io.patchIn.ready.expect(true.B)
      dut.io.patchIn.bits.packetIndex.poke(index.U)
      dut.io.patchIn.bits.symbolIndex.poke(symbol.U)
      dut.io.patchIn.bits.scale.poke(scale.U)
      dut.io.patchIn.bits.offset.poke(offset.U)
      dut.io.patchIn.valid.poke(true.B)
      dut.clock.step()
      dut.io.patchIn.valid.poke(false.B)
    }
    for (value <- template) {
      dut.io.packetIn.ready.expect(true.B)
      dut.io.packetIn.bits.poke(value.U)
      dut.io.packetIn.valid.poke(true.B)
      dut.clock.step()
      dut.io.packetIn.valid.poke(false.B)
    }
    ack(dut, done = true, detail = detail)
    dut.io.captureActive.expect(false.B)
  }

  private def copy(dut: CgraLinkAdapter, elements: Int): Unit = {
    val request = dut.io.autoLink.requestCopy
    request.bits.task.poke(0.U)
    request.bits.job.poke(0.U)
    request.bits.sourceAddress.poke(0x60000000L.U)
    request.bits.destinationOffset.poke(0.U)
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

  private def compute(dut: CgraLinkAdapter, id: Int, expected: Seq[BigInt]): Unit = {
    dut.io.autoLink.requestCompute.bits.job.poke(0.U)
    dut.io.autoLink.requestCompute.bits.start.poke(true.B)
    dut.io.autoLink.requestCompute.bits.tile.id.poke(id.U)
    dut.io.autoLink.requestCompute.valid.poke(true.B)
    dut.io.autoLink.requestCompute.ready.expect(true.B)
    dut.clock.step()
    dut.io.autoLink.requestCompute.valid.poke(false.B)
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
    dut.io.autoLink.reportCompute.valid.expect(true.B)
    dut.io.autoLink.reportCompute.bits.status.expect(AutoLinkStatus.Success)
    dut.clock.step(2)
    dut.io.autoLink.reportCompute.ready.poke(true.B)
    dut.clock.step()
    dut.io.autoLink.reportCompute.ready.poke(false.B)
  }

  behavior of "Cgra cached templates"

  it should "bind copy counts and slots on each replay, preserve other bits and clear old patches" in {
    test(new CgraLinkAdapter(params)) { dut =>
      init(dut)
      capture(dut, Seq((4, 16, 0), (0, 0, 1)), Seq((0, 0, 1, 0), (1, 1, 5, 10)))
      for ((id, elements) <- Seq((4, 19), (5, 3), (6, 32))) {
        copy(dut, elements)
        val expected = template.zipWithIndex.map { case (value, index) =>
          val payload = if (index == 0) 4 + (id % auto.bufferSlots) * 16 else elements * 5 + 10
          if (index < 2) (value & ~payloadMask) | (BigInt(payload) << payloadLsb) else value
        }
        compute(dut, id, expected)
      }
      capture(dut, Nil, Nil)
      compute(dut, 7, template)
    }
  }

  it should "drain malformed metadata, reject execution and accept a replacement capture" in {
    test(new CgraLinkAdapter(params)) { dut =>
      init(dut)
      begin(dut, symbolCount = 3, patchCount = 1)
      ack(dut, done = true, detail = CgraLinkStatus.BadConfig)
      for (patches <- Seq(
        Seq((3, 0, 1, 0)),
        Seq((0, 1, 1, 0)),
        Seq((0, 0, 1, 0), (0, 0, 2, 0)))) {
        capture(dut, Seq((0, 0, 0)), patches, CgraLinkStatus.BadPacket)
        dut.io.autoLink.requestCompute.bits.job.poke(0.U)
        dut.io.autoLink.requestCompute.bits.start.poke(true.B)
        dut.io.autoLink.requestCompute.valid.poke(true.B)
        dut.clock.step()
        dut.io.autoLink.requestCompute.valid.poke(false.B)
        dut.io.jobPacket.valid.expect(false.B)
        dut.io.autoLink.reportCompute.valid.expect(true.B)
        dut.io.autoLink.reportCompute.bits.detail.expect(CgraLinkStatus.BadConfig.U)
        dut.io.autoLink.reportCompute.ready.poke(true.B)
        dut.clock.step()
        dut.io.autoLink.reportCompute.ready.poke(false.B)
      }
      capture(dut, Nil, Nil)
      compute(dut, 0, template)
    }
  }

  it should "reject an element-count source for a job without incoming data" in {
    val noCopy = auto.copy(
      stages = Seq(AutoStageSpec("sink", "cgra", 0)),
      dependencies = Seq(AutoDependencySpec(None, 0, None)),
      endpoints = Seq(AutoEndpointSpec("cgra", None, 512)))
    test(new CgraLinkAdapter(params.copy(auto = noCopy))) { dut =>
      init(dut)
      capture(dut, Seq((0, 0, 1)), Seq((0, 0, 1, 0)), CgraLinkStatus.BadPacket)
    }
  }
}
