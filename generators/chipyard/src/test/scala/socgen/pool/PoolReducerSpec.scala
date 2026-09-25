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

      dut.io.clear.poke(false.B)
      dut.io.mode.poke(PoolMode.Max)
      dut.io.supported.expect(true.B)
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
      dut.clock.step(3)
      dut.io.output.valid.expect(true.B)
      dut.io.input.ready.expect(false.B)
      dut.io.output.ready.poke(true.B)
      dut.clock.step()
      dut.io.output.valid.expect(false.B)
      dut.io.input.ready.expect(true.B)
    }
  }

  for (elementBits <- Seq(8, 16, 32)) {
    it should s"average signed $elementBits-bit values with padding, rounding and backpressure" in {
      val params = PoolParams(elementBits = elementBits)
      val lanes = 4
      val minimum = -(BigInt(1) << (elementBits - 1))
      val maximum = (BigInt(1) << (elementBits - 1)) - 1
      val mask = (BigInt(1) << elementBits) - 1
      test(new PoolReducer(params, beatBits = lanes * elementBits)) { dut =>
        def pack(values: Seq[BigInt]): BigInt = values.zipWithIndex.foldLeft(BigInt(0)) {
          case (packed, (value, lane)) => packed | ((value & mask) << (lane * elementBits))
        }
        def send(values: Seq[BigInt], sampleValid: Boolean, first: Boolean, last: Boolean): Unit = {
          dut.io.input.ready.expect(true.B)
          dut.io.input.bits.data.poke(pack(values).U)
          dut.io.input.bits.lanes.poke("h7".U)
          dut.io.input.bits.sampleValid.poke(sampleValid.B)
          dut.io.input.bits.first.poke(first.B)
          dut.io.input.bits.last.poke(last.B)
          dut.io.input.valid.poke(true.B)
          dut.clock.step()
          dut.io.input.valid.poke(false.B)
        }
        def receive(values: Seq[BigInt]): Unit = {
          var cycles = 0
          while (!dut.io.output.valid.peek().litToBoolean && cycles <= lanes * (elementBits + 1)) {
            dut.io.input.ready.expect(false.B)
            dut.clock.step()
            cycles += 1
          }
          dut.io.output.valid.expect(true.B)
          dut.io.output.bits.lanes.expect("h7".U)
          val tailMask = (BigInt(1) << (3 * elementBits)) - 1
          assert((dut.io.output.bits.data.peek().litValue & tailMask) == (pack(values) & tailMask))
          val held = dut.io.output.bits.data.peek().litValue
          dut.clock.step(3)
          dut.io.output.valid.expect(true.B)
          dut.io.output.bits.data.expect(held.U)
          dut.io.output.bits.lanes.expect("h7".U)
          dut.io.input.ready.expect(false.B)
          dut.io.output.ready.poke(true.B)
          dut.clock.step()
          dut.io.output.ready.poke(false.B)
          dut.io.output.valid.expect(false.B)
          dut.io.input.ready.expect(true.B)
        }

        dut.io.clear.poke(false.B)
        dut.io.mode.poke(PoolMode.Average)
        dut.io.supported.expect(true.B)
        dut.io.output.ready.poke(false.B)
        dut.io.input.valid.poke(false.B)

        send(Seq.fill(lanes)(maximum), sampleValid = false, first = true, last = false)
        send(Seq(1, -1, 3, 7).map(BigInt(_)), sampleValid = true, first = false, last = false)
        send(Seq(2, -2, 4, 8).map(BigInt(_)), sampleValid = true, first = false, last = false)
        send(Seq.fill(lanes)(minimum), sampleValid = false, first = false, last = true)
        receive(Seq(2, -2, 4).map(BigInt(_)))

        send(Seq(1, -1, -2, 9).map(BigInt(_)), sampleValid = true, first = true, last = false)
        send(Seq(1, -1, -2, 9).map(BigInt(_)), sampleValid = true, first = false, last = false)
        send(Seq(2, -2, -1, 9).map(BigInt(_)), sampleValid = true, first = false, last = true)
        receive(Seq(1, -1, -2).map(BigInt(_)))

        val extremes = Seq(minimum, maximum, minimum, maximum)
        send(extremes, sampleValid = true, first = true, last = false)
        send(extremes, sampleValid = true, first = false, last = false)
        send(extremes, sampleValid = true, first = false, last = true)
        receive(extremes.take(3))

        send(extremes, sampleValid = true, first = true, last = true)
        dut.clock.step(elementBits / 2)
        dut.io.clear.poke(true.B)
        dut.clock.step()
        dut.io.clear.poke(false.B)
        dut.io.output.valid.expect(false.B)
        dut.io.input.ready.expect(true.B)
        send(Seq(maximum, minimum, BigInt(0), BigInt(0)), sampleValid = true, first = true, last = true)
        receive(Seq(maximum, minimum, BigInt(0)))

        dut.io.mode.poke(PoolMode.Max)
        dut.io.supported.expect(true.B)
        send(extremes, sampleValid = true, first = true, last = true)
        dut.io.output.valid.expect(true.B)
        receive(extremes.take(3))
      }
    }
  }
}
