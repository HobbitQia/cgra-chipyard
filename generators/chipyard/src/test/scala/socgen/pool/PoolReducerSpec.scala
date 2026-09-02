package chipyard.socgen.pool

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class PoolReducerSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "PoolReducer"

  it should "reduce signed values across padding and a channel tail" in {
    val params = PoolParams(elementBits = 32)
    test(new PoolReducer(params, beatBits = 256)) { dut =>
      val mask = (BigInt(1) << params.elementBits) - 1
      def pack(values: Seq[Int]): BigInt = values.zipWithIndex.map { case (value, lane) =>
        (BigInt(value) & mask) << (lane * params.elementBits)
      }.reduce(_ | _)
      def send(values: Seq[Int], sampleValid: Boolean, first: Boolean, last: Boolean): Unit = {
        dut.io.input.bits.data.poke(pack(values).U)
        dut.io.input.bits.lanes.poke("h07".U)
        dut.io.input.bits.sampleValid.poke(sampleValid.B)
        dut.io.input.bits.first.poke(first.B)
        dut.io.input.bits.last.poke(last.B)
        dut.io.input.valid.poke(true.B)
        while (!dut.io.input.ready.peek().litToBoolean) {
          dut.clock.step()
        }
        dut.clock.step()
        dut.io.input.valid.poke(false.B)
      }

      dut.io.mode.poke(PoolMode.Max)
      dut.io.output.ready.poke(false.B)
      dut.io.input.valid.poke(false.B)
      send(Seq.fill(8)(0), sampleValid = false, first = true, last = false)
      send(Seq(-8, 4, -2, 70, 80, 90, 100, 110), sampleValid = true, first = false, last = false)
      send(Seq(-3, 1, 9, 60, 70, 80, 90, 100), sampleValid = true, first = false, last = true)

      dut.io.output.valid.expect(true.B)
      dut.io.output.bits.lanes.expect("h07".U)
      val tailMask = (BigInt(1) << (3 * params.elementBits)) - 1
      assert((dut.io.output.bits.data.peek().litValue & tailMask) ==
        (pack(Seq(-3, 4, 9)) & tailMask))
    }
  }
}
