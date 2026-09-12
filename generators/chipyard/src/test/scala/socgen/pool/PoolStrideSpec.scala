package chipyard.socgen.pool

import chisel3._
import chiseltest._
import chipyard.socgen.link._
import org.scalatest.flatspec.AnyFlatSpec

class PoolStrideSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Pool row strides"

  for ((elementBits, beatBits) <- Seq((8, 128), (16, 256), (32, 256))) {
    it should s"preserve row gaps, split beats and restart with $elementBits-bit elements" in {
      val params = PoolParams(elementBits = elementBits)
      val elementBytes = params.elementBytes
      val beatBytes = beatBits / 8
      val lanes = beatBits / elementBits
      val channels = lanes + 1
      val rowElements = 2 * channels
      val rowBytes = rowElements * elementBytes
      val destination = 64 + beatBytes - elementBytes
      test(new PoolWriteSplitter(params, beatBits)) { dut =>
        dut.io.start.poke(false.B)
        dut.io.cancel.poke(false.B)
        dut.io.input.valid.poke(false.B)
        dut.io.output.ready.poke(false.B)

        def start(rows: Int, stride: Int): Unit = {
          dut.io.destination.poke(destination.U)
          dut.io.elementCount.poke((rows * rowElements).U)
          dut.io.rowElements.poke(rowElements.U)
          dut.io.rowStride.poke(stride.U)
          dut.io.start.poke(true.B)
          dut.clock.step()
          dut.io.start.poke(false.B)
        }

        def run(rows: Int, stride: Int): Unit = {
          val memory = Array.fill(512)(0xa5)
          val expected = memory.clone()
          start(rows, stride)
          for (row <- 0 until rows; pixel <- 0 until 2; group <- 0 until 2) {
            val count = if (group == 0) lanes else 1
            val index = pixel * channels + group * lanes
            val values = (0 until count).map(lane => row * rowElements + index + lane + 1)
            val data = values.zipWithIndex.foldLeft(BigInt(0)) { case (packed, (value, lane)) =>
              packed | (BigInt(value) << (lane * elementBits))
            }
            val address = destination + row * stride + index * elementBytes
            val bytes = count * elementBytes
            for (byte <- 0 until bytes) {
              expected(address + byte) = ((data >> (byte * 8)) & 0xff).toInt
            }
            dut.io.input.ready.expect(true.B)
            dut.io.input.bits.data.poke(data.U)
            dut.io.input.bits.lanes.poke(((BigInt(1) << count) - 1).U)
            dut.io.input.valid.poke(true.B)
            dut.clock.step()
            dut.io.input.valid.poke(false.B)

            val first = address / beatBytes * beatBytes
            val last = (address + bytes - 1) / beatBytes * beatBytes
            for (beat <- first to last by beatBytes) {
              val selected = (0 until beatBytes).filter(byte => beat + byte >= address && beat + byte < address + bytes)
              val mask = selected.foldLeft(BigInt(0))((value, byte) => value | (BigInt(1) << byte))
              val finalBeat = row == rows - 1 && pixel == 1 && group == 1 && beat == last
              dut.io.output.valid.expect(true.B)
              dut.io.output.bits.address.expect(beat.U)
              dut.io.output.bits.mask.expect(mask.U)
              dut.io.output.bits.last.expect(finalBeat.B)
              val beatData = dut.io.output.bits.data.peek().litValue
              dut.io.input.ready.expect(false.B)
              dut.clock.step(2)
              dut.io.output.valid.expect(true.B)
              dut.io.output.bits.address.expect(beat.U)
              dut.io.output.bits.mask.expect(mask.U)
              dut.io.output.bits.data.expect(beatData.U)
              dut.io.output.bits.last.expect(finalBeat.B)
              for (byte <- selected) {
                memory(beat + byte) = ((beatData >> (byte * 8)) & 0xff).toInt
              }
              dut.io.output.ready.poke(true.B)
              dut.clock.step()
              dut.io.output.ready.poke(false.B)
            }
          }
          assert(memory.toSeq == expected.toSeq)
          dut.io.output.valid.expect(false.B)
          dut.io.input.ready.expect(false.B)
        }

        run(rows = 3, stride = rowBytes + 3 * elementBytes)
        start(rows = 2, stride = rowBytes + 5 * elementBytes)
        dut.io.input.bits.data.poke(0.U)
        dut.io.input.bits.lanes.poke(((BigInt(1) << lanes) - 1).U)
        dut.io.input.valid.poke(true.B)
        dut.clock.step()
        dut.io.input.valid.poke(false.B)
        dut.io.output.ready.poke(true.B)
        dut.clock.step()
        dut.io.output.ready.poke(false.B)
        dut.io.output.valid.expect(true.B)
        dut.io.cancel.poke(true.B)
        dut.io.output.valid.expect(false.B)
        dut.clock.step()
        dut.io.cancel.poke(false.B)
        dut.io.output.valid.expect(false.B)
        dut.io.input.ready.expect(false.B)
        run(rows = 2, stride = rowBytes)
      }
    }
  }

  private def configure(job: PoolJob, rows: Int = 3): Unit = {
    job.tiled.poke(false.B)
    job.mode.poke(PoolMode.Max)
    job.source.poke(0x1000.U)
    job.destination.poke(0x2000.U)
    job.outputStride.poke(32.U)
    job.inputHeight.poke(rows.U)
    job.inputWidth.poke(2.U)
    job.channels.poke(3.U)
    job.kernelHeight.poke(1.U)
    job.kernelWidth.poke(1.U)
    job.strideHeight.poke(1.U)
    job.strideWidth.poke(1.U)
    job.padHeight.poke(0.U)
    job.padWidth.poke(0.U)
    job.padBottom.poke(0.U)
    job.padRight.poke(0.U)
  }

  it should "drain outstanding reads before reporting a denied transfer" in {
    test(new PoolEngine(PoolParams(), beatBits = 64)) { dut =>
      dut.io.job.valid.poke(false.B)
      dut.io.inputDone.ready.poke(false.B)
      dut.io.done.ready.poke(false.B)
      dut.io.dma.readRequest.ready.poke(true.B)
      dut.io.dma.writeRequest.ready.poke(true.B)
      dut.io.dma.readResponse.valid.poke(false.B)
      dut.io.dma.writeResponse.valid.poke(false.B)
      configure(dut.io.job.bits)
      dut.io.job.valid.poke(true.B)
      dut.clock.step()
      dut.io.job.valid.poke(false.B)
      val sources = scala.collection.mutable.ArrayBuffer.empty[BigInt]
      var cycles = 0
      while (sources.size < 4 && cycles < 30) {
        if (dut.io.dma.readRequest.valid.peek().litToBoolean) {
          sources += dut.io.dma.readRequest.bits.source.peek().litValue
        }
        dut.clock.step()
        cycles += 1
      }
      assert(sources.size == 4)
      dut.io.dma.readRequest.ready.poke(false.B)
      dut.io.dma.readResponse.bits.data.poke(0.U)
      dut.io.dma.readResponse.bits.corrupt.poke(false.B)
      for ((source, index) <- sources.reverse.zipWithIndex) {
        dut.io.dma.readResponse.bits.source.poke(source.U)
        dut.io.dma.readResponse.bits.denied.poke((index == 0).B)
        dut.io.dma.readResponse.valid.poke(true.B)
        dut.io.dma.readResponse.ready.expect(true.B)
        dut.io.done.valid.expect(false.B)
        dut.clock.step()
        dut.io.dma.readResponse.valid.poke(false.B)
        if (index < sources.size - 1) {
          dut.clock.step(2)
          dut.io.done.valid.expect(false.B)
        }
      }
      cycles = 0
      while (!dut.io.done.valid.peek().litToBoolean && cycles < 10) {
        dut.clock.step()
        cycles += 1
      }
      dut.io.done.valid.expect(true.B)
      dut.io.done.bits.status.expect(PoolStatus.Denied)
      dut.io.inputDone.valid.expect(true.B)
      dut.io.inputDone.bits.status.expect(PoolStatus.Denied)
      dut.io.dma.writeRequest.valid.expect(false.B)
      dut.clock.step(2)
      dut.io.done.valid.expect(true.B)
      dut.io.inputDone.ready.poke(true.B)
      dut.io.done.ready.poke(true.B)
      dut.clock.step()
      dut.io.job.ready.expect(true.B)
    }
  }

  it should "publish engine completion and preserve its status" in {
    val auto = AutoLinkParams(
      stages = Seq(AutoStageSpec("pool", "pool", 0)),
      dependencies = Seq(AutoDependencySpec(None, 0, None)),
      endpoints = Seq(AutoEndpointSpec("pool", None, 256)),
      beatBytes = 8,
      controlAddress = 0x30000,
      controlBytes = 4096)
    test(new PoolLinkAdapter(PoolParams(), Some(PoolLinkParams(auto)))) { dut =>
      val port = dut.io.autoLink.get
      for ((rows, stride, expected) <- Seq((3, 0, PoolStatus.Success),
          (3, 24, PoolStatus.Success), (1, 32, PoolStatus.Success), (3, 24, PoolStatus.Corrupt))) {
        dut.reset.poke(true.B)
        dut.clock.step()
        dut.reset.poke(false.B)
        configure(dut.io.configuredJob, rows)
        dut.io.configuredJob.outputStride.poke(stride.U)
        dut.io.job.ready.poke(true.B)
        dut.io.inputDone.valid.poke(false.B)
        dut.io.jobDone.valid.poke(false.B)
        port.requestCopy.valid.poke(false.B)
        port.requestCompute.valid.poke(false.B)
        port.reportOutput.ready.poke(false.B)
        port.reportCopy.ready.poke(false.B)
        port.reportCompute.ready.poke(false.B)
        port.watchOutput.bits.job.poke(0.U)
        port.watchOutput.bits.address.poke(0x2000.U)
        port.watchOutput.bits.bytes.poke((rows * 24).U)
        port.watchOutput.valid.poke(true.B)
        dut.clock.step()
        port.watchOutput.valid.poke(false.B)
        port.requestCopy.bits.task.poke(0.U)
        port.requestCopy.bits.job.poke(0.U)
        port.requestCopy.bits.sourceAddress.poke(0x1000.U)
        port.requestCopy.bits.bytes.poke((rows * 24).U)
        port.requestCopy.valid.poke(true.B)
        dut.io.job.valid.expect(true.B)
        dut.io.job.bits.outputStride.expect(stride.U)
        dut.clock.step()
        port.requestCopy.valid.poke(false.B)
        dut.io.inputDone.bits.status.poke(expected)
        dut.io.inputDone.valid.poke(true.B)
        dut.io.jobDone.bits.status.poke(expected)
        dut.io.jobDone.valid.poke(true.B)
        dut.clock.step()
        dut.io.inputDone.valid.poke(false.B)
        dut.io.jobDone.valid.poke(false.B)
        port.reportOutput.valid.expect(true.B)
        port.reportOutput.bits.detail.expect(expected)
      }
    }
  }
}
