package chipyard.socgen.cgra

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CgraPackedWriterSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "CgraPackedWriter"

  private def pack(values: Seq[Int], bits: Int): BigInt = {
    val mask = (BigInt(1) << bits) - 1
    values.zipWithIndex.map { case (value, lane) =>
      (BigInt(value) & mask) << (lane * bits)
    }.foldLeft(BigInt(0))(_ | _)
  }

  private def init(dut: CgraPackedWriter): Unit = {
    dut.io.packed.poke(false.B)
    dut.io.start.valid.poke(false.B)
    dut.io.start.bits.poke(0.U)
    dut.io.nativeReq.valid.poke(false.B)
    dut.io.nativeReq.bits.address.poke(0.U)
    dut.io.nativeReq.bits.data.poke(0.U)
    dut.io.nativeReq.bits.mask.poke(0.U)
    dut.io.nativeResp.ready.poke(false.B)
    dut.io.memoryReq.ready.poke(false.B)
    dut.io.memoryResp.valid.poke(false.B)
    dut.io.memoryResp.bits.poke(false.B)
  }

  it should "leave raw writes and their completion unchanged" in {
    test(new CgraPackedWriter(128, 32, CgraRequantParams(1, 0))) { dut =>
      init(dut)
      val data = pack(Seq(-1234, 7, 0x12345678, 0), 32)
      dut.io.nativeReq.valid.poke(true.B)
      dut.io.nativeReq.bits.address.poke("h120".U)
      dut.io.nativeReq.bits.data.poke(data.U)
      dut.io.nativeReq.bits.mask.poke("hfff".U)
      dut.io.memoryReq.valid.expect(true.B)
      dut.io.memoryReq.bits.address.expect("h120".U)
      dut.io.memoryReq.bits.data.expect(data.U)
      dut.io.memoryReq.bits.mask.expect("hfff".U)
      dut.io.nativeReq.ready.expect(false.B)
      dut.clock.step(3)
      dut.io.memoryReq.ready.poke(true.B)
      dut.io.nativeReq.ready.expect(true.B)
      dut.clock.step()
      dut.io.nativeReq.valid.poke(false.B)
      dut.io.memoryResp.valid.poke(true.B)
      dut.io.memoryResp.bits.poke(true.B)
      dut.io.nativeResp.valid.expect(true.B)
      dut.io.nativeResp.bits.expect(true.B)
      dut.io.memoryResp.ready.expect(false.B)
      dut.clock.step(3)
      dut.io.nativeResp.ready.poke(true.B)
      dut.io.memoryResp.ready.expect(true.B)
      dut.clock.step()
    }
  }

  it should "pack exact tails across beat boundaries and wait for real acknowledgments" in {
    test(new CgraPackedWriter(128, 32, CgraRequantParams(1, 1))) { dut =>
      init(dut)
      dut.io.packed.poke(true.B)

      def transfer(base: Int, words: Seq[Int]): Unit = {
        dut.io.start.bits.poke(base.U)
        dut.io.start.valid.poke(true.B)
        dut.clock.step()
        dut.io.start.valid.poke(false.B)
        var address = base
        for ((values, beat) <- words.grouped(4).zipWithIndex) {
          val converted = values.map { value =>
            val rounded = (math.abs(value) + 1) / 2 * (if (value < 0) -1 else 1)
            math.max(-128, math.min(127, rounded))
          }
          dut.io.nativeReq.valid.poke(true.B)
          dut.io.nativeReq.bits.address.poke((base + beat * 16).U)
          dut.io.nativeReq.bits.data.poke(pack(values, 32).U)
          dut.io.nativeReq.bits.mask.poke(((BigInt(1) << (values.size * 4)) - 1).U)
          dut.io.nativeReq.ready.expect(true.B)
          dut.clock.step()
          dut.io.nativeReq.valid.poke(false.B)
          var consumed = 0
          while (consumed < values.size) {
            val offset = address % 16
            val count = math.min(values.size - consumed, 16 - offset)
            val data = pack(converted.drop(consumed), 8) << (offset * 8)
            val mask = ((1 << count) - 1) << offset
            dut.io.memoryReq.ready.poke(false.B)
            for (_ <- 0 until 3) {
              dut.io.memoryReq.valid.expect(true.B)
              dut.io.memoryReq.bits.address.expect((address & ~15).U)
              dut.io.memoryReq.bits.mask.expect(mask.U)
              dut.io.memoryReq.bits.data.expect((data & ((BigInt(1) << 128) - 1)).U)
              dut.io.nativeResp.valid.expect(false.B)
              dut.clock.step()
            }
            dut.io.memoryReq.ready.poke(true.B)
            dut.clock.step()
            for (_ <- 0 until 3) {
              dut.io.nativeResp.valid.expect(false.B)
              dut.io.memoryReq.valid.expect(false.B)
              dut.clock.step()
            }
            dut.io.memoryResp.valid.poke(true.B)
            dut.io.memoryResp.ready.expect(true.B)
            dut.clock.step()
            dut.io.memoryResp.valid.poke(false.B)
            consumed += count
            address += count
          }
          dut.io.nativeResp.ready.poke(false.B)
          for (_ <- 0 until 3) {
            dut.io.nativeResp.valid.expect(true.B)
            dut.io.nativeResp.bits.expect(false.B)
            dut.io.nativeReq.ready.expect(false.B)
            dut.io.busy.expect(true.B)
            dut.clock.step()
          }
          dut.io.nativeResp.ready.poke(true.B)
          dut.clock.step()
          dut.io.busy.expect(false.B)
        }
      }

      transfer(0x100, Seq(-1000, -1, 1, 1000, 5, -5, 0))
      transfer(0x10f, Seq(9, -9, 12, 128, -128, 255, -255, 0, 1))
    }
  }
}
