package chipyard.socgen.link

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AutoTransferCheck(params: AutoLinkParams, dependency: Int) extends Module {
  val io = IO(new Bundle {
    val transfer = Input(new AutoTransfer(params))
    val multipleSlots = Input(Bool())
    val valid = Output(Bool())
  })
  io.valid := AutoTileBinding.transferValid(params, dependency, io.transfer, io.multipleSlots)
}

class AutoBindingSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val params = AutoLinkParams(
    stages = Seq(AutoStageSpec("producer", "gemmini", 0), AutoStageSpec("join", "cgra", 0)),
    dependencies = Seq(
      AutoDependencySpec(None, 0, None),
      AutoDependencySpec(None, 1, None),
      AutoDependencySpec(Some(0), 1, Some(AutoCopySpec(0, 0, 128, expansion = 4)))),
    endpoints = Seq(
      AutoEndpointSpec("gemmini", Some(AutoBuffer(0x60000000L, 2048)), 2048, bufferSlots = 2),
      AutoEndpointSpec("cgra", None, 2048, bufferSlots = 2)),
    beatBytes = 16,
    controlAddress = 0x60020000L,
    controlBytes = 4096)

  private case class Tile(row: Int, column: Int, rows: Int, columns: Int)
  private val tiles = Seq(Tile(0, 0, 2, 2), Tile(0, 2, 2, 2), Tile(2, 0, 1, 2), Tile(2, 2, 1, 2))

  private def region(tile: Tile, halo: Boolean): Tile = {
    val margin = if (halo) 1 else 0
    val end = if (halo) 1 else -1
    val row = math.max(0, tile.row * 2 - margin)
    val column = math.max(0, tile.column * 2 - margin)
    Tile(row, column, math.min(5, (tile.row + tile.rows) * 2 + end) - row,
      math.min(7, (tile.column + tile.columns) * 2 + end) - column)
  }

  private def check(port: AutoTile, id: Int, expected: Tile): Unit = {
    port.id.expect(id.U)
    port.row.expect(expected.row.U)
    port.column.expect(expected.column.U)
    port.rows.expect(expected.rows.U)
    port.columns.expect(expected.columns.U)
    port.last.expect((id == tiles.size - 1).B)
  }

  private def event(port: AutoEvent): Unit = {
    port.stage.poke(0.U)
    port.job.poke(0.U)
    port.status.poke(0.U)
    port.detail.poke(0.U)
    port.data.poke(0.U)
  }

  behavior of "AutoScheduler runtime bindings"

  for (expansion <- Seq(1, 4)) {
    it should s"validate buffered transfer slots and alignment with expansion $expansion" in {
      val buffered = params.copy(
        dependencies = params.dependencies.updated(2,
          AutoDependencySpec(Some(0), 1, Some(AutoCopySpec(0, 0, 128, expansion)))),
        endpoints = params.endpoints.updated(1,
          AutoEndpointSpec("cgra", None, 2048, bufferedInput = true, inputAlignment = 4, bufferSlots = 2)))
      test(new AutoTransferCheck(buffered, 2)) { dut =>
        val destinationBytes = 128 * expansion
        def check(sourceOffset: Int = 0, sourceStride: Int = 128,
            destinationOffset: Int = 0, destinationStride: Int = destinationBytes,
            multipleSlots: Boolean = true, valid: Boolean = true): Unit = {
          dut.io.transfer.sourceOffset.poke(sourceOffset.U)
          dut.io.transfer.sourceStride.poke(sourceStride.U)
          dut.io.transfer.destinationOffset.poke(destinationOffset.U)
          dut.io.transfer.destinationStride.poke(destinationStride.U)
          dut.io.transfer.bytesPerPixel.poke(4.U)
          dut.io.multipleSlots.poke(multipleSlots.B)
          dut.io.valid.expect(valid.B)
        }
        check()
        check(sourceStride = 0, valid = false)
        check(sourceStride = 64, valid = false)
        check(destinationStride = 0, valid = false)
        check(destinationStride = destinationBytes / 2, valid = false)
        check(sourceStride = 0, destinationStride = 0, multipleSlots = false)
        check(destinationOffset = 1, valid = false)
        check(destinationStride = destinationBytes + 1, valid = false)
        if (expansion == 1) {
          check(sourceOffset = 1, valid = false)
          check(sourceStride = 129, valid = false)
          check(destinationOffset = 4, valid = false)
          check(destinationStride = destinationBytes + 4, valid = false)
          check(sourceOffset = 16, sourceStride = 144, destinationOffset = 16,
            destinationStride = destinationBytes + 16)
        } else {
          check(sourceOffset = 1, sourceStride = 129, destinationOffset = 4,
            destinationStride = destinationBytes + 4)
        }
      }
    }
  }

  it should "allow an unbuffered streaming destination without a slot stride" in {
    test(new AutoTransferCheck(params, 2)) { dut =>
      dut.io.multipleSlots.poke(true.B)
      dut.io.transfer.sourceOffset.poke(3.U)
      dut.io.transfer.sourceStride.poke(128.U)
      dut.io.transfer.destinationOffset.poke(5.U)
      dut.io.transfer.destinationStride.poke(0.U)
      dut.io.transfer.bytesPerPixel.poke(4.U)
      dut.io.valid.expect(true.B)
      dut.io.transfer.sourceStride.poke(0.U)
      dut.io.valid.expect(false.B)
      dut.io.transfer.sourceStride.poke(64.U)
      dut.io.valid.expect(false.B)
    }
  }

  it should "bind clipped regions and slot transfers without changing join identity, and drain invalid tiles" in {
    test(new AutoScheduler(params)) { dut =>
      dut.io.transfers.foreach { transfer =>
        transfer.sourceOffset.poke(0.U)
        transfer.destinationOffset.poke(0.U)
        transfer.sourceStride.poke(0.U)
        transfer.destinationStride.poke(0.U)
        transfer.bytesPerPixel.poke(0.U)
      }
      val transfer = dut.io.transfers(2)
      transfer.sourceOffset.poke(32.U)
      transfer.destinationOffset.poke(64.U)
      transfer.sourceStride.poke(128.U)
      transfer.destinationStride.poke(512.U)
      dut.io.regions.zipWithIndex.foreach { case (shape, index) =>
        shape.rows.poke(5.U)
        shape.columns.poke(7.U)
        shape.rowStep.poke(2.U)
        shape.columnStep.poke(2.U)
        shape.top.poke((if (index == 0) 1 else 0).U)
        shape.left.poke((if (index == 0) 1 else 0).U)
        val adjustment = if (index == 0) BigInt(1) else (BigInt(1) << params.lengthWidth) - 1
        shape.bottom.poke(adjustment.U)
        shape.right.poke(adjustment.U)
      }

      def run(pixelBytes: Int): Unit = {
        transfer.bytesPerPixel.poke(pixelBytes.U)
        val copyDue = Array.fill(2)(-1)
        val computeDue = Array.fill(2)(-1)
        val outputDue = Array.fill(2)(-1)
        val watches = scala.collection.mutable.Set.empty[Int]
        val copies = scala.collection.mutable.Set.empty[Int]
        val launches = scala.collection.mutable.Set.empty[(Int, Int)]
        val skips = scala.collection.mutable.Set.empty[(Int, Int)]
        val results = Array.fill(2)(0)
        val validTiles = tiles.indices.filter { id =>
          val source = region(tiles(id), halo = true)
          source.rows * source.columns * pixelBytes <= 128
        }.toSet
        var sent = 0
        var cycle = 0
        while ((sent < tiles.size || results.exists(_ == 0) || dut.io.busy.peek().litToBoolean) && cycle < 600) {
          val id = math.min(sent, tiles.size - 1)
          val root = tiles(id)
          dut.io.root.valid.poke((sent < tiles.size).B)
          event(dut.io.root.bits.event)
          dut.io.root.bits.slot.poke(0.U)
          dut.io.root.bits.tile.id.poke(id.U)
          dut.io.root.bits.tile.row.poke(root.row.U)
          dut.io.root.bits.tile.column.poke(root.column.U)
          dut.io.root.bits.tile.rows.poke(root.rows.U)
          dut.io.root.bits.tile.columns.poke(root.columns.U)
          dut.io.root.bits.tile.last.poke((id == tiles.size - 1).B)
          dut.io.result.foreach(_.ready.poke((cycle % 5 != 0).B))
          dut.io.endpoint.zipWithIndex.foreach { case (port, index) =>
            port.watchOutput.ready.poke((cycle % 3 != 0).B)
            port.requestCopy.ready.poke((cycle % 4 != 0).B)
            port.requestCompute.ready.poke((cycle % 3 != 0).B)
            port.reportCopy.valid.poke((copyDue(index) >= 0 && copyDue(index) <= cycle).B)
            port.reportCopy.bits.task.poke(2.U)
            port.reportCopy.bits.status.poke(0.U)
            port.reportCopy.bits.detail.poke(0.U)
            port.reportCompute.valid.poke((computeDue(index) >= 0 && computeDue(index) <= cycle).B)
            event(port.reportCompute.bits)
            port.reportOutput.valid.poke((outputDue(index) >= 0 && outputDue(index) <= cycle).B)
            event(port.reportOutput.bits)
          }
          if (dut.io.root.valid.peek().litToBoolean && dut.io.root.ready.peek().litToBoolean) sent += 1
          dut.io.endpoint.zipWithIndex.foreach { case (port, index) =>
            if (port.reportCopy.valid.peek().litToBoolean && port.reportCopy.ready.peek().litToBoolean) copyDue(index) = -1
            if (port.reportCompute.valid.peek().litToBoolean && port.reportCompute.ready.peek().litToBoolean) computeDue(index) = -1
            if (port.reportOutput.valid.peek().litToBoolean && port.reportOutput.ready.peek().litToBoolean) outputDue(index) = -1
            if (port.watchOutput.valid.peek().litToBoolean) {
              assert(index == 0)
              val tileId = port.watchOutput.bits.tile.id.peek().litValue.toInt
              assert(validTiles.contains(tileId))
              val source = region(tiles(tileId), halo = true)
              check(port.watchOutput.bits.tile, tileId, source)
              val slot = port.watchOutput.bits.slot.peek().litValue.toInt
              port.watchOutput.bits.address.expect((0x60000000L + 32 + slot * 128).U)
              port.watchOutput.bits.bytes.expect((source.rows * source.columns * pixelBytes).U)
              if (port.watchOutput.ready.peek().litToBoolean) watches += tileId
            }
            if (port.requestCopy.valid.peek().litToBoolean) {
              assert(index == 1)
              val tileId = port.requestCopy.bits.tile.id.peek().litValue.toInt
              assert(validTiles.contains(tileId))
              val source = region(tiles(tileId), halo = true)
              val bytes = source.rows * source.columns * pixelBytes
              check(port.requestCopy.bits.sourceTile, tileId, source)
              check(port.requestCopy.bits.tile, tileId, region(tiles(tileId), halo = false))
              val sourceSlot = port.requestCopy.bits.sourceSlot.peek().litValue.toInt
              port.requestCopy.bits.sourceAddress.expect((0x60000000L + 32 + sourceSlot * 128).U)
              port.requestCopy.bits.destinationOffset.expect(64.U)
              port.requestCopy.bits.bytes.expect(bytes.U)
              port.requestCopy.bits.destinationBytes.expect((bytes * 4).U)
              if (port.requestCopy.ready.peek().litToBoolean) {
                copies += tileId
                copyDue(index) = cycle + 2
              }
            }
            if (port.requestCompute.valid.peek().litToBoolean) {
              val tileId = port.requestCompute.bits.tile.id.peek().litValue.toInt
              check(port.requestCompute.bits.tile, tileId, region(tiles(tileId), halo = index == 0))
              port.requestCompute.bits.start.expect(validTiles.contains(tileId).B)
              if (port.requestCompute.ready.peek().litToBoolean) {
                if (validTiles.contains(tileId)) {
                  launches += ((index, tileId))
                  computeDue(index) = cycle + 4
                  if (index == 0) outputDue(index) = cycle + 3
                } else {
                  skips += ((index, tileId))
                }
              }
            }
          }
          dut.io.result.zipWithIndex.foreach { case (port, index) =>
            if (port.valid.peek().litToBoolean && port.ready.peek().litToBoolean) {
              port.bits.stage.expect(index.U)
              port.bits.status.expect((if (validTiles.size == tiles.size) AutoLinkStatus.Success else AutoLinkStatus.ConfigFailure))
              results(index) += 1
            }
          }
          dut.clock.step()
          cycle += 1
        }
        assert(cycle < 600)
        assert(results.forall(_ == 1))
        assert(watches.toSet == validTiles && copies.toSet == validTiles)
        assert(launches.toSet == (for (index <- 0 until 2; id <- validTiles) yield (index, id)).toSet)
        assert(skips.toSet == (for (index <- 0 until 2; id <- tiles.indices if !validTiles.contains(id)) yield (index, id)).toSet)
      }

      run(4)
      run(16)
      run(4)
    }
  }
}
