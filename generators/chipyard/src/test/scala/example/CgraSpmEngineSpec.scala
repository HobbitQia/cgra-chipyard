package chipyard.example

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CgraSpmEngineSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val protocol = SpmDmaParams(slotCount = 2, slotSizeBytes = 1024, beatBytes = 16, resultWidth = 32)
  private val params = CgraSpmParams(protocol, CGRAGenerated.params, Seq(BigInt("6000f800", 16), BigInt("6000fc00", 16)), packetCapacity = 4)
  private val firstPacket = BigInt(CGRACmdGenerated.CMD_LAUNCH) << params.cgra.packetLayout.cmdLsb
  private val secondPacket = firstPacket | 1

  private def init(dut: CgraSpmEngine): Unit = {
    dut.io.configIn.valid.poke(false.B)
    dut.io.configAck.ready.poke(true.B)
    dut.io.transferStart.valid.poke(false.B)
    dut.io.transferDone.ready.poke(true.B)
    dut.io.consumerStart.valid.poke(false.B)
    dut.io.consumerDone.ready.poke(false.B)
    dut.io.dmaRequest.ready.poke(true.B)
    dut.io.dmaCompletion.valid.poke(false.B)
    dut.io.launchPacket.ready.poke(true.B)
    dut.io.complete.valid.poke(false.B)
    dut.io.cpuComputeActive.poke(false.B)
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
  }

  private def pokeHeader(dut: CgraSpmEngine): Unit = {
    dut.io.configIn.bits.kind.poke(CgraSpmConfigKind.Header)
    dut.io.configIn.bits.header.jobId.poke(11.U)
    dut.io.configIn.bits.header.slot.poke(0.U)
    dut.io.configIn.bits.header.bytes.poke(128.U)
    dut.io.configIn.bits.header.spmWordAddress.poke(0.U)
    dut.io.configIn.bits.header.dmaTag.poke(19.U)
    dut.io.configIn.bits.header.packetCount.poke(2.U)
    dut.io.configIn.bits.packet.poke(0.U)
  }

  private def pokePacket(dut: CgraSpmEngine, packet: BigInt): Unit = {
    dut.io.configIn.bits.kind.poke(CgraSpmConfigKind.Packet)
    dut.io.configIn.bits.header.jobId.poke(0.U)
    dut.io.configIn.bits.header.slot.poke(0.U)
    dut.io.configIn.bits.header.bytes.poke(0.U)
    dut.io.configIn.bits.header.spmWordAddress.poke(0.U)
    dut.io.configIn.bits.header.dmaTag.poke(0.U)
    dut.io.configIn.bits.header.packetCount.poke(0.U)
    dut.io.configIn.bits.packet.poke(packet.U)
  }

  private def pokeCommand(command: SpmDmaCommand): Unit = {
    command.jobId.poke(11.U)
    command.slot.poke(0.U)
    command.bytes.poke(128.U)
  }

  behavior of "CgraSpmEngine"

  it should "store the complete launch sequence before transfer and compute" in {
    test(new CgraSpmEngine(params)) { dut =>
      init(dut)

      pokeHeader(dut)
      dut.io.configIn.valid.poke(true.B)
      dut.clock.step()
      pokePacket(dut, firstPacket)
      dut.clock.step()
      dut.io.configAck.valid.expect(false.B)
      dut.io.cpuComputeActive.poke(true.B)
      pokePacket(dut, secondPacket)
      dut.clock.step()
      dut.io.configIn.valid.poke(false.B)
      dut.io.configAck.valid.expect(false.B)
      dut.io.cpuComputeActive.poke(false.B)
      dut.io.configAck.valid.expect(true.B)
      dut.io.configAck.bits.jobId.expect(11.U)
      dut.io.configAck.bits.bytes.expect(128.U)
      dut.clock.step()

      pokeCommand(dut.io.transferStart.bits)
      dut.io.transferStart.valid.poke(true.B)
      dut.io.transferStart.ready.expect(true.B)
      dut.clock.step()
      dut.io.transferStart.valid.poke(false.B)
      dut.io.dmaRequest.valid.expect(true.B)
      dut.io.dmaRequest.bits.sourceAddress.expect(BigInt("6000f800", 16).U)
      dut.io.dmaRequest.bits.bytes.expect(128.U)
      dut.clock.step()

      dut.io.dmaCompletion.bits.jobId.poke(11.U)
      dut.io.dmaCompletion.bits.slot.poke(0.U)
      dut.io.dmaCompletion.bits.dmaTag.poke(19.U)
      dut.io.dmaCompletion.valid.poke(true.B)
      dut.clock.step()
      dut.io.dmaCompletion.valid.poke(false.B)
      dut.io.transferDone.valid.expect(true.B)
      dut.clock.step()

      pokeCommand(dut.io.consumerStart.bits)
      dut.io.consumerStart.valid.poke(true.B)
      dut.io.consumerStart.ready.expect(true.B)
      dut.clock.step()
      dut.io.consumerStart.valid.poke(false.B)
      dut.io.launchPacket.valid.expect(true.B)
      dut.io.launchPacket.bits.expect(firstPacket.U)
      dut.clock.step()
      dut.io.launchPacket.bits.expect(secondPacket.U)
      dut.clock.step()

      dut.io.complete.bits.poke(29.U)
      dut.io.complete.valid.poke(true.B)
      dut.clock.step()
      dut.io.complete.valid.poke(false.B)
      dut.io.consumerDone.valid.expect(true.B)
      dut.io.consumerDone.bits.status.expect(SpmDmaStatus.Success)
      dut.io.consumerDone.bits.data.expect(29.U)
    }
  }
}
