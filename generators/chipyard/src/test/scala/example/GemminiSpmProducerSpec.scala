package chipyard.example

import chisel3._
import chiseltest._
import freechips.rocketchip.tilelink.TLMessages
import org.scalatest.flatspec.AnyFlatSpec

class GemminiSpmProducerSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val protocol = SpmDmaParams(slotCount = 2, slotSizeBytes = 1024, beatBytes = 16)
  private val params = GemminiSpmParams(Seq(BigInt("6000f800", 16), BigInt("6000fc00", 16)), 1024, 16)

  behavior of "GemminiSpmProducer"

  it should "complete publication only after the final TileLink response" in {
    test(new GemminiSpmProducer(protocol, params)) { dut =>
      dut.io.start.valid.poke(false.B)
      dut.io.write.valid.poke(false.B)
      dut.io.ack.valid.poke(false.B)
      dut.io.done.ready.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)

      dut.io.start.bits.jobId.poke(9.U)
      dut.io.start.bits.slot.poke(0.U)
      dut.io.start.bits.bytes.poke(16.U)
      dut.io.start.valid.poke(true.B)
      dut.clock.step()
      dut.io.start.valid.poke(false.B)

      dut.io.write.bits.address.poke(BigInt("6000f800", 16).U)
      dut.io.write.bits.source.poke(3.U)
      dut.io.write.bits.size.poke(4.U)
      dut.io.write.bits.opcode.poke(TLMessages.PutFullData)
      dut.io.write.bits.mask.poke("hffff".U)
      dut.io.write.valid.poke(true.B)
      dut.clock.step()
      dut.io.write.valid.poke(false.B)
      dut.io.done.valid.expect(false.B)
      dut.clock.step(2)
      dut.io.done.valid.expect(false.B)

      dut.io.ack.bits.source.poke(3.U)
      dut.io.ack.bits.size.poke(4.U)
      dut.io.ack.bits.denied.poke(false.B)
      dut.io.ack.bits.corrupt.poke(false.B)
      dut.io.ack.valid.poke(true.B)
      dut.clock.step()
      dut.io.ack.valid.poke(false.B)
      dut.io.done.valid.expect(true.B)
      dut.io.done.bits.status.expect(SpmDmaStatus.Success)
      dut.io.done.bits.bytes.expect(16.U)
    }
  }
}
