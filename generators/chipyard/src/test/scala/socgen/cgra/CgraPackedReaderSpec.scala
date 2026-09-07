package chipyard.socgen.cgra

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CgraPackedReaderSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "CgraPackedReader"

  private def pack(values: Seq[Int], bits: Int): BigInt = {
    val mask = (BigInt(1) << bits) - 1
    values.zipWithIndex.map { case (value, lane) =>
      (BigInt(value) & mask) << (lane * bits)
    }.foldLeft(BigInt(0))(_ | _)
  }

  private def init(dut: CgraPackedReader): Unit = {
    dut.io.start.valid.poke(false.B)
    dut.io.start.bits.address.poke(0.U)
    dut.io.start.bits.bytes.poke(0.U)
    dut.io.nativeReq.valid.poke(false.B)
    dut.io.nativeReq.bits.poke(0.U)
    dut.io.nativeResp.ready.poke(false.B)
    dut.io.memoryReq.ready.poke(false.B)
    dut.io.memoryResp.valid.poke(false.B)
    dut.io.memoryResp.bits.poke(0.U)
  }

  it should "forward raw full beats with request and response backpressure" in {
    test(new CgraPackedReader(128, 32, 32)) { dut =>
      init(dut)
      dut.io.packed.poke(false.B)
      dut.io.nativeReq.valid.poke(true.B)
      dut.io.nativeReq.bits.poke("h120".U)
      dut.io.memoryReq.valid.expect(true.B)
      dut.io.memoryReq.bits.address.expect("h120".U)
      dut.io.memoryReq.bits.lgSize.expect(4.U)
      dut.io.nativeReq.ready.expect(false.B)
      dut.clock.step(3)
      dut.io.memoryReq.ready.poke(true.B)
      dut.io.nativeReq.ready.expect(true.B)
      dut.clock.step()
      dut.io.nativeReq.valid.poke(false.B)
      dut.io.busy.expect(true.B)
      dut.clock.step(3)
      val data = pack(Seq(-128, 0x12345678, -1, 127), 32)
      dut.io.memoryResp.valid.poke(true.B)
      dut.io.memoryResp.bits.poke(data.U)
      for (_ <- 0 until 3) {
        dut.io.nativeResp.valid.expect(true.B)
        dut.io.nativeResp.bits.expect(data.U)
        dut.io.memoryResp.ready.expect(false.B)
        dut.clock.step()
      }
      dut.io.nativeResp.ready.poke(true.B)
      dut.io.memoryResp.ready.expect(true.B)
      dut.clock.step()
      dut.io.memoryResp.valid.poke(false.B)
      dut.io.busy.expect(false.B)
    }
  }

  it should "expand signed bytes with exact tails and demand-driven cache fills" in {
    test(new CgraPackedReader(128, 32, 32)) { dut =>
      init(dut)
      dut.io.packed.poke(true.B)

      def transfer(base: Int, values: Seq[Int], reads: Seq[(Int, Int)]): Unit = {
        dut.io.packed.poke(false.B)
        dut.io.start.bits.address.poke(base.U)
        dut.io.start.bits.bytes.poke(values.size.U)
        dut.io.start.valid.poke(true.B)
        dut.clock.step()
        dut.io.start.valid.poke(false.B)
        dut.io.packed.poke(true.B)
        dut.io.busy.expect(true.B)
        var readIndex = 0

        for ((words, beat) <- values.grouped(4).zipWithIndex) {
          dut.io.nativeResp.ready.poke(false.B)
          dut.io.memoryReq.valid.expect(false.B)
          dut.clock.step(2)
          dut.io.memoryReq.valid.expect(false.B)
          dut.io.nativeReq.valid.poke(true.B)
          dut.io.nativeReq.bits.poke((base + beat * 16).U)
          dut.io.nativeReq.ready.expect(true.B)
          dut.clock.step()
          dut.io.nativeReq.valid.poke(false.B)

          var pending = Option.empty[(BigInt, Int)]
          var stalledRequest = Option.empty[(BigInt, BigInt)]
          var responseCycles = 0
          var done = false
          var cycle = 0
          val expected = pack(words.padTo(4, 0), 32)
          while (!done && cycle < 100) {
            dut.io.memoryReq.ready.poke((cycle % 3 != 0).B)
            dut.io.memoryResp.valid.poke(pending.exists(_._2 == 0).B)
            pending.foreach { case (data, _) => dut.io.memoryResp.bits.poke(data.U) }
            dut.io.nativeResp.ready.poke((responseCycles >= 3).B)
            stalledRequest.foreach { case (address, size) =>
              dut.io.memoryReq.valid.expect(true.B)
              dut.io.memoryReq.bits.address.expect(address.U)
              dut.io.memoryReq.bits.lgSize.expect(size.U)
            }
            stalledRequest = None
            if (dut.io.memoryReq.valid.peek().litToBoolean) {
              val address = dut.io.memoryReq.bits.address.peek().litValue.toInt
              val size = dut.io.memoryReq.bits.lgSize.peek().litValue.toInt
              if (dut.io.memoryReq.ready.peek().litToBoolean) {
                assert(pending.isEmpty)
                assert(readIndex < reads.size)
                assert((address, size) == reads(readIndex))
                val count = 1 << size
                assert(address % count == 0)
                assert(address >= base && address + count <= base + values.size)
                val bytes = values.slice(address - base, address - base + count)
                pending = Some((pack(bytes.padTo(16, 0x5a), 8), 3))
                readIndex += 1
              } else {
                stalledRequest = Some((BigInt(address), BigInt(size)))
              }
            }
            val memoryFire = dut.io.memoryResp.valid.peek().litToBoolean &&
              dut.io.memoryResp.ready.peek().litToBoolean
            if (responseCycles > 0) {
              dut.io.nativeResp.valid.expect(true.B)
            }
            if (dut.io.nativeResp.valid.peek().litToBoolean) {
              dut.io.nativeResp.bits.expect(expected.U)
              done = dut.io.nativeResp.ready.peek().litToBoolean
              responseCycles += 1
            }
            dut.clock.step()
            pending = if (memoryFire) None else pending.map { case (data, delay) =>
              (data, math.max(0, delay - 1))
            }
            cycle += 1
          }
          assert(done)
          assert(pending.isEmpty)
          dut.io.memoryResp.valid.poke(false.B)
        }
        assert(readIndex == reads.size)
        dut.io.busy.expect(false.B)
        dut.io.nativeReq.ready.expect(false.B)
      }

      transfer(0x100, Seq.tabulate(19)(i => i * 13 - 128),
        Seq((0x100, 4), (0x110, 1), (0x112, 0)))
      transfer(0x200, Seq(-128, -1, 127), Seq((0x200, 1), (0x202, 0)))
      transfer(0x300, Seq.tabulate(35)(i => (i * 17 & 255) - 128),
        Seq((0x300, 4), (0x310, 4), (0x320, 1), (0x322, 0)))
      transfer(0x403, Seq.tabulate(19)(i => i - 9),
        Seq((0x403, 0), (0x404, 2), (0x408, 3), (0x410, 1), (0x412, 0),
          (0x413, 0), (0x414, 1)))
    }
  }
}
