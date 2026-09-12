package chipyard.socgen.pool

import chisel3._
import chiseltest._
import chipyard.socgen.link._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable

class PoolTileSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Pool tile binding"

  private val auto = AutoLinkParams(
    stages = Seq(AutoStageSpec("pool", "pool", 0)),
    dependencies = Seq(AutoDependencySpec(None, 0, None)),
    endpoints = Seq(AutoEndpointSpec("pool", None, 256)),
    beatBytes = 8,
    controlAddress = 0x30000,
    controlBytes = 4096)

  private def configure(job: PoolJob): Unit = {
    job.tiled.poke(true.B)
    job.mode.poke(PoolMode.Max)
    job.source.poke(0x1000.U)
    job.destination.poke(0x2000.U)
    job.outputStride.poke(0.U)
    job.inputHeight.poke(9.U)
    job.inputWidth.poke(11.U)
    job.channels.poke(3.U)
    job.kernelHeight.poke(3.U)
    job.kernelWidth.poke(3.U)
    job.strideHeight.poke(2.U)
    job.strideWidth.poke(2.U)
    job.padHeight.poke(1.U)
    job.padWidth.poke(1.U)
    job.padBottom.poke(1.U)
    job.padRight.poke(1.U)
  }

  private def tile(value: AutoTile, row: Int, column: Int, rows: Int, columns: Int): Unit = {
    value.id.poke(3.U)
    value.row.poke(row.U)
    value.column.poke(column.U)
    value.rows.poke(rows.U)
    value.columns.poke(columns.U)
    value.last.poke(false.B)
  }

  it should "bind first, interior and tail regions without changing the template" in {
    test(new PoolTileBinding(PoolParams(elementBits = 8), auto)) { dut =>
      configure(dut.io.configured)
      dut.io.request.sourceAddress.poke(0x1800.U)
      val cases = Seq(
        (0, 0, 2, 2, 0, 0, 4, 4, 1, 1, 0, 0),
        (2, 2, 2, 3, 3, 3, 5, 7, 0, 0, 0, 0),
        (4, 5, 1, 1, 7, 9, 2, 2, 0, 0, 1, 1))
      for ((row, column, rows, columns, y, x, height, width, top, left, bottom, right) <- cases) {
        tile(dut.io.request.tile, row, column, rows, columns)
        tile(dut.io.request.sourceTile, y, x, height, width)
        dut.io.request.bytes.poke((height * width * 3).U)
        dut.io.job.source.expect(0x1800.U)
        dut.io.job.destination.expect((0x2000 + (row * 6 + column) * 3).U)
        dut.io.job.outputStride.expect(18.U)
        dut.io.job.inputHeight.expect(height.U)
        dut.io.job.inputWidth.expect(width.U)
        dut.io.job.padHeight.expect(top.U)
        dut.io.job.padWidth.expect(left.U)
        dut.io.job.padBottom.expect(bottom.U)
        dut.io.job.padRight.expect(right.U)
        dut.clock.step(2)
      }
      dut.io.configured.outputStride.poke(32.U)
      dut.io.job.destination.expect((0x2000 + 4 * 32 + 5 * 3).U)
      dut.io.job.outputStride.expect(32.U)
    }
  }

  it should "hold a bound job under backpressure and propagate input errors" in {
    test(new PoolLinkAdapter(PoolParams(elementBits = 8), Some(PoolLinkParams(auto)))) { dut =>
      configure(dut.io.configuredJob)
      dut.io.job.ready.poke(false.B)
      dut.io.inputDone.valid.poke(false.B)
      dut.io.jobDone.valid.poke(false.B)
      val port = dut.io.autoLink.get
      port.watchOutput.valid.poke(false.B)
      port.reportOutput.ready.poke(true.B)
      port.requestCompute.valid.poke(false.B)
      port.reportCompute.ready.poke(true.B)
      port.reportCopy.ready.poke(false.B)
      tile(port.requestCopy.bits.tile, 0, 0, 2, 2)
      tile(port.requestCopy.bits.sourceTile, 0, 0, 4, 4)
      port.requestCopy.bits.bytes.poke(48.U)
      port.requestCopy.bits.task.poke(0.U)
      port.requestCopy.bits.job.poke(0.U)
      port.requestCopy.valid.poke(true.B)
      port.requestCopy.ready.expect(false.B)
      dut.io.job.valid.expect(true.B)
      dut.io.job.bits.padHeight.expect(1.U)
      dut.io.job.bits.padBottom.expect(0.U)
      dut.io.job.bits.outputStride.expect(18.U)
      dut.clock.step(3)
      port.reportCopy.valid.expect(false.B)
      dut.io.job.ready.poke(true.B)
      dut.clock.step()
      port.requestCopy.valid.poke(false.B)
      port.reportCopy.valid.expect(false.B)
      dut.io.inputDone.bits.status.poke(PoolStatus.Denied)
      dut.io.inputDone.valid.poke(true.B)
      dut.io.jobDone.bits.status.poke(PoolStatus.Denied)
      dut.io.jobDone.valid.poke(true.B)
      dut.clock.step()
      dut.io.inputDone.valid.poke(false.B)
      dut.io.jobDone.valid.poke(false.B)
      port.reportCopy.valid.expect(true.B)
      port.reportCopy.bits.status.expect(AutoLinkStatus.SinkFailure)
      port.reportCopy.bits.detail.expect(PoolStatus.Denied)
      dut.clock.step(2)
      port.reportCopy.valid.expect(true.B)
      port.reportCopy.ready.poke(true.B)
      dut.clock.step()
      port.requestCompute.bits.start.poke(false.B)
      port.requestCompute.valid.poke(true.B)
      port.requestCompute.ready.expect(true.B)
      dut.clock.step()
      port.requestCompute.valid.poke(false.B)
      port.requestCopy.valid.poke(true.B)
      dut.io.job.valid.expect(true.B)
      dut.io.job.bits.padHeight.expect(1.U)
      dut.io.job.bits.padBottom.expect(0.U)
      dut.io.job.bits.outputStride.expect(18.U)
    }
  }

  it should "compute asymmetric border padding without producing an extra row or column" in {
    test(new PoolEngine(PoolParams(), beatBits = 64)) { dut =>
      configure(dut.io.job.bits)
      dut.io.job.bits.inputHeight.poke(2.U)
      dut.io.job.bits.inputWidth.poke(2.U)
      dut.io.job.bits.channels.poke(1.U)
      dut.io.job.bits.strideHeight.poke(1.U)
      dut.io.job.bits.strideWidth.poke(1.U)
      dut.io.job.bits.padBottom.poke(0.U)
      dut.io.job.bits.padRight.poke(0.U)
      dut.io.inputDone.ready.poke(true.B)
      dut.io.done.ready.poke(false.B)
      dut.io.dma.readRequest.ready.poke(true.B)
      dut.io.dma.writeRequest.ready.poke(true.B)
      dut.io.dma.readResponse.valid.poke(false.B)
      dut.io.dma.writeResponse.valid.poke(false.B)
      dut.io.job.valid.poke(true.B)
      dut.clock.step()
      dut.io.job.valid.poke(false.B)

      val reads = mutable.Queue[(BigInt, BigInt)]()
      val writes = mutable.Queue[BigInt]()
      val output = mutable.Map[BigInt, Int]()
      val memory = Seq(-4, -3, -2, -1).flatMap(value => (0 until 4).map(byte => (value >>> (byte * 8)) & 255))
      var cycles = 0
      while (!dut.io.done.valid.peek().litToBoolean && cycles < 200) {
        dut.io.dma.readResponse.valid.poke(reads.nonEmpty.B)
        dut.io.dma.readResponse.bits.denied.poke(false.B)
        dut.io.dma.readResponse.bits.corrupt.poke(false.B)
        reads.headOption.foreach { case (source, data) =>
          dut.io.dma.readResponse.bits.source.poke(source.U)
          dut.io.dma.readResponse.bits.data.poke(data.U)
        }
        dut.io.dma.writeResponse.valid.poke(writes.nonEmpty.B)
        dut.io.dma.writeResponse.bits.denied.poke(false.B)
        dut.io.dma.writeResponse.bits.corrupt.poke(false.B)
        writes.headOption.foreach(source => dut.io.dma.writeResponse.bits.source.poke(source.U))
        val readAccepted = reads.nonEmpty && dut.io.dma.readResponse.ready.peek().litToBoolean
        val writeAccepted = writes.nonEmpty && dut.io.dma.writeResponse.ready.peek().litToBoolean
        val nextRead = if (dut.io.dma.readRequest.valid.peek().litToBoolean) {
          val address = dut.io.dma.readRequest.bits.address.peek().litValue.toInt
          val data = (0 until 8).foldLeft(BigInt(0)) { (packed, byte) =>
            packed | (BigInt(memory(address - 0x1000 + byte)) << (byte * 8))
          }
          Some(dut.io.dma.readRequest.bits.source.peek().litValue -> data)
        } else None
        val nextWrite = if (dut.io.dma.writeRequest.valid.peek().litToBoolean) {
          val address = dut.io.dma.writeRequest.bits.address.peek().litValue
          val mask = dut.io.dma.writeRequest.bits.mask.peek().litValue
          val data = dut.io.dma.writeRequest.bits.data.peek().litValue
          for (byte <- 0 until 8 if mask.testBit(byte)) {
            output(address + byte) = ((data >> (byte * 8)) & 255).toInt
          }
          Some(dut.io.dma.writeRequest.bits.source.peek().litValue)
        } else None
        dut.clock.step()
        if (readAccepted) reads.dequeue()
        if (writeAccepted) writes.dequeue()
        nextRead.foreach(reads.enqueue(_))
        nextWrite.foreach(writes.enqueue(_))
        cycles += 1
      }
      dut.io.done.valid.expect(true.B)
      dut.io.done.bits.status.expect(PoolStatus.Success)
      assert(output.toMap == (0 until 4).map(byte => BigInt(0x2000 + byte) -> 255).toMap)
      assert(reads.isEmpty && writes.isEmpty)
    }
  }
}
