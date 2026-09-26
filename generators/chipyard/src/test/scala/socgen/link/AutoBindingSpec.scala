package chipyard.socgen.link

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

object AutoGraphTest {
  def configure(stages: Vec[AutoGraphStage], edges: Vec[AutoGraphEdge], transfers: Vec[AutoTransfer], params: AutoLinkParams): Unit = {
    stages.zipWithIndex.foreach { case (port, index) =>
      val stage = params.stages.lift(index)
      port.enabled.poke(stage.nonEmpty.B)
      port.endpoint.poke(stage.map(value => params.endpoints.indexWhere(_.name == value.endpoint)).getOrElse(0).U)
      port.output.writeback.poke(false.B)
      port.output.packed.poke(false.B)
      port.output.sourceOffset.poke(0.U)
      port.output.sourceStride.poke(0.U)
      port.output.address.poke(0.U)
      port.output.stride.poke(0.U)
      port.output.bytes.poke(0.U)
      port.output.bytesPerPixel.poke(0.U)
    }
    edges.zipWithIndex.foreach { case (port, index) =>
      val edge = params.dependencies.lift(index)
      val copy = edge.flatMap(_.copy)
      port.enabled.poke(edge.nonEmpty.B)
      port.root.poke(edge.forall(_.source.isEmpty).B)
      port.source.poke(edge.flatMap(_.source).getOrElse(0).U)
      port.destination.poke(edge.map(_.destination).getOrElse(0).U)
      port.copy.poke(copy.nonEmpty.B)
      port.bytes.poke(copy.map(_.bytes).getOrElse(0).U)
      port.expansion.poke(chisel3.util.log2Ceil(copy.map(_.expansion).getOrElse(1)).U)
      val base = copy.map(value => value.sourceAddress.getOrElse(
        params.endpoint(params.stage(edge.get.source.get).endpoint).buffer.get.baseAddress)).getOrElse(BigInt(0))
      transfers(index).sourceBase.poke(base.U)
    }
  }
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

  it should "bind clipped regions and slot transfers without changing join identity" in {
    test(new AutoScheduler(params)) { dut =>
      AutoGraphTest.configure(dut.io.stages, dut.io.edges, dut.io.transfers, params)
      dut.io.jobs.zip(params.stages).foreach { case (port, stage) => port.poke(stage.job.U) }
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
        val results = Array.fill(2)(0)
        val tileIds = tiles.indices.toSet
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
              assert(tileIds.contains(tileId))
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
              assert(tileIds.contains(tileId))
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
              port.requestCompute.bits.start.expect(true.B)
              if (port.requestCompute.ready.peek().litToBoolean) {
                launches += ((index, tileId))
                computeDue(index) = cycle + 4
                if (index == 0) outputDue(index) = cycle + 3
              }
            }
          }
          dut.io.result.zipWithIndex.foreach { case (port, index) =>
            if (port.valid.peek().litToBoolean && port.ready.peek().litToBoolean) {
              port.bits.stage.expect(index.U)
              port.bits.status.expect(AutoLinkStatus.Success)
              results(index) += 1
            }
          }
          dut.clock.step()
          cycle += 1
        }
        assert(cycle < 600)
        assert(results.forall(_ == 1))
        assert(watches.toSet == tileIds && copies.toSet == tileIds)
        assert(launches.toSet == (for (index <- 0 until 2; id <- tileIds) yield (index, id)).toSet)
      }

      run(4)
      run(2)
      run(4)
    }
  }

  it should "replace stage bindings and edges after draining a graph" in {
    val flexible = params.copy(stageCapacity = 3, dependencyCapacity = 4,
      endpoints = params.endpoints.map(_.copy(jobs = 2)))
    test(new AutoScheduler(flexible)) { dut =>
      AutoGraphTest.configure(dut.io.stages, dut.io.edges, dut.io.transfers, flexible)
      dut.io.transfers.foreach { transfer =>
        transfer.sourceOffset.poke(0.U)
        transfer.destinationOffset.poke(0.U)
        transfer.sourceStride.poke(0.U)
        transfer.destinationStride.poke(0.U)
        transfer.bytesPerPixel.poke(0.U)
      }
      dut.io.regions.foreach { region =>
        region.rows.poke(0.U)
        region.columns.poke(0.U)
        region.rowStep.poke(0.U)
        region.columnStep.poke(0.U)
        region.top.poke(0.U)
        region.bottom.poke(0.U)
        region.left.poke(0.U)
        region.right.poke(0.U)
      }
      dut.io.jobs.foreach(_.poke(0.U))
      dut.io.result.foreach(_.ready.poke(true.B))
      event(dut.io.root.bits.event)
      dut.io.root.bits.slot.poke(0.U)
      dut.io.root.bits.tile.id.poke(0.U)
      dut.io.root.bits.tile.row.poke(0.U)
      dut.io.root.bits.tile.column.poke(0.U)
      dut.io.root.bits.tile.rows.poke(1.U)
      dut.io.root.bits.tile.columns.poke(1.U)
      dut.io.root.bits.tile.last.poke(true.B)
      for (replacement <- Seq(false, true)) {
        val sink = if (replacement) 2 else 1
        val sourceEndpoint = if (replacement) 1 else 0
        val sinkEndpoint = 1 - sourceEndpoint
        val job = if (replacement) 1 else 0
        val sourceAddress = if (replacement) 0x80001000L else 0x60000000L
        dut.io.stages(0).endpoint.poke(sourceEndpoint.U)
        dut.io.stages(1).enabled.poke((!replacement).B)
        dut.io.stages(2).enabled.poke(replacement.B)
        dut.io.stages(sink).endpoint.poke(sinkEndpoint.U)
        dut.io.jobs(0).poke(job.U)
        dut.io.jobs(sink).poke(job.U)
        dut.io.edges(1).enabled.poke(false.B)
        dut.io.edges(2).destination.poke(sink.U)
        dut.io.transfers(2).sourceBase.poke(sourceAddress.U)
        val output = dut.io.stages(sink).output
        output.writeback.poke(replacement.B)
        output.packed.poke(replacement.B)
        output.sourceOffset.poke(64.U)
        output.address.poke(0x80002000L.U)
        output.bytes.poke(32.U)
        val compute = Array.fill(2)(false)
        val publication = Array.fill(2)(false)
        val copy = Array.fill(2)(false)
        val watched = Array.fill(2)(false)
        val launched = scala.collection.mutable.ArrayBuffer.empty[Int]
        val results = scala.collection.mutable.Set.empty[Int]
        var submitted = false
        var cycle = 0
        while ((!submitted || results.size < 2 || dut.io.busy.peek().litToBoolean) && cycle < 200) {
          dut.io.root.valid.poke((!submitted).B)
          dut.io.endpoint.zipWithIndex.foreach { case (port, index) =>
            port.watchOutput.ready.poke(true.B)
            port.requestCopy.ready.poke(true.B)
            port.requestCompute.ready.poke(true.B)
            port.reportCopy.valid.poke(copy(index).B)
            port.reportCopy.bits.task.poke(2.U)
            port.reportCopy.bits.status.poke(0.U)
            port.reportCopy.bits.detail.poke(0.U)
            port.reportCompute.valid.poke(compute(index).B)
            event(port.reportCompute.bits)
            port.reportCompute.bits.job.poke(job.U)
            port.reportOutput.valid.poke(publication(index).B)
            event(port.reportOutput.bits)
            port.reportOutput.bits.job.poke(job.U)
          }
          if (dut.io.root.valid.peek().litToBoolean && dut.io.root.ready.peek().litToBoolean) submitted = true
          dut.io.endpoint.zipWithIndex.foreach { case (port, index) =>
            if (port.reportCopy.valid.peek().litToBoolean && port.reportCopy.ready.peek().litToBoolean) copy(index) = false
            if (port.reportCompute.valid.peek().litToBoolean && port.reportCompute.ready.peek().litToBoolean) compute(index) = false
            if (port.reportOutput.valid.peek().litToBoolean && port.reportOutput.ready.peek().litToBoolean) publication(index) = false
            if (port.watchOutput.valid.peek().litToBoolean) {
              watched(index) = true
              port.watchOutput.bits.writeback.expect((replacement && index == sinkEndpoint).B)
              port.watchOutput.bits.address.expect((if (index == sourceEndpoint) sourceAddress else 0x80002000L).U)
              if (index == sinkEndpoint) {
                port.watchOutput.bits.sourceOffset.expect(64.U)
                port.watchOutput.bits.bytes.expect(32.U)
              }
            }
            if (port.requestCopy.valid.peek().litToBoolean) {
              assert(index == sinkEndpoint)
              port.requestCopy.bits.sourceAddress.expect(sourceAddress.U)
              copy(index) = true
            }
            if (port.requestCompute.valid.peek().litToBoolean) {
              port.requestCompute.bits.job.expect(job.U)
              port.requestCompute.bits.hasInput.expect((index == sinkEndpoint).B)
              launched += index
              compute(index) = true
              publication(index) = watched(index)
            }
          }
          dut.io.result.foreach { port =>
            if (port.valid.peek().litToBoolean) results += port.bits.stage.peek().litValue.toInt
          }
          dut.clock.step()
          cycle += 1
        }
        assert(cycle < 200)
        assert(launched.toSeq == Seq(sourceEndpoint, sinkEndpoint))
        assert(results.toSet == Set(0, sink))
        dut.io.root.valid.poke(false.B)
        dut.clock.step(2)
      }
    }
  }
}
