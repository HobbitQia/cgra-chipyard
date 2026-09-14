package chipyard.socgen.link

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AutoScheduleSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val params = AutoLinkParams(
    stages = Seq(
      AutoStageSpec("conv1", "gemmini", 0),
      AutoStageSpec("relu", "cgra", 0),
      AutoStageSpec("conv2", "gemmini", 1),
      AutoStageSpec("add", "cgra", 1)),
    dependencies = Seq(
      AutoDependencySpec(None, 0, None),
      AutoDependencySpec(None, 3, None),
      AutoDependencySpec(Some(0), 1, Some(AutoCopySpec(0, 0, 32))),
      AutoDependencySpec(Some(1), 2, Some(AutoCopySpec(0, 0, 32))),
      AutoDependencySpec(Some(2), 3, Some(AutoCopySpec(0, 0, 32))),
      AutoDependencySpec(Some(0), 2, None)),
    endpoints = Seq(
      AutoEndpointSpec("gemmini", Some(AutoBuffer(0x60000000L, 256)), 256, bufferSlots = 2),
      AutoEndpointSpec("cgra", Some(AutoBuffer(0x60010000L, 256)), 256, bufferedInput = true,
        bufferSlots = 2, releaseOnCopy = true)),
    beatBytes = 16,
    controlAddress = 0x60020000L,
    controlBytes = 4096)

  private def event(port: AutoEvent, job: Int, status: Int = 0, detail: Int = 0): Unit = {
    port.stage.poke(0.U)
    port.job.poke(job.U)
    port.status.poke(status.U)
    port.detail.poke(detail.U)
    port.data.poke(0.U)
  }

  private def tile(port: AutoTile, id: Int, last: Boolean): Unit = {
    port.id.poke(id.U)
    port.row.poke((id / 3 * 2).U)
    port.column.poke((id % 3 * 2).U)
    port.rows.poke(2.U)
    port.columns.poke((if (id % 3 == 2) 1 else 2).U)
    port.last.poke(last.B)
  }

  private def bindings(transfers: Vec[AutoTransfer], regions: Vec[AutoRegion]): Unit = {
    transfers.foreach { transfer =>
      transfer.sourceOffset.poke(0.U)
      transfer.destinationOffset.poke(0.U)
      transfer.sourceStride.poke(0.U)
      transfer.destinationStride.poke(0.U)
      transfer.bytesPerPixel.poke(0.U)
    }
    regions.foreach { region =>
      region.rows.poke(0.U)
      region.columns.poke(0.U)
      region.rowStep.poke(0.U)
      region.columnStep.poke(0.U)
      region.top.poke(0.U)
      region.bottom.poke(0.U)
      region.left.poke(0.U)
      region.right.poke(0.U)
    }
  }

  behavior of "AutoScheduler"

  it should "overlap six tiles, retain joins and slot lifetime, and aggregate errors across shared endpoints" in {
    test(new AutoScheduler(params)) { dut =>
      bindings(dut.io.transfers, dut.io.regions)
      case class Completion(due: Int, stage: Int, id: Int, status: Int)
      case class Copy(due: Int, task: Int)
      val compute = Array.fill[Option[Completion]](2)(None)
      val publish = Array.fill[Option[Completion]](2)(None)
      val copies = Array.fill[Option[Copy]](2)(None)
      val watched = Array.fill[Option[(Int, Int)]](2)(None)
      var cycle = 0

      def run(count: Int, injectFailure: Boolean): Unit = {
        var submitted = 0
        val started = scala.collection.mutable.Set.empty[(Int, Int)]
        val completed = scala.collection.mutable.Set.empty[(Int, Int)]
        val results = Array.fill(4)(0)
        var overlap = false
        val deadline = cycle + 2000
        while ((submitted < count || completed.size < count * 4 || results.exists(_ == 0) ||
          dut.io.busy.peek().litToBoolean) && cycle < deadline) {
          dut.io.root.valid.poke((submitted < count).B)
          event(dut.io.root.bits.event, 0)
          dut.io.root.bits.slot.poke(0.U)
          tile(dut.io.root.bits.tile, submitted, submitted == count - 1)
          dut.io.result.foreach(_.ready.poke((cycle % 7 != 0).B))
          dut.io.endpoint.zipWithIndex.foreach { case (port, index) =>
            port.watchOutput.ready.poke((cycle % 5 != 0).B)
            port.requestCopy.ready.poke((cycle % 4 != 0).B)
            port.requestCompute.ready.poke((cycle % 3 != 0).B)
            port.reportCopy.valid.poke(copies(index).exists(_.due <= cycle).B)
            port.reportCopy.bits.task.poke(copies(index).map(_.task).getOrElse(0).U)
            port.reportCopy.bits.status.poke(0.U)
            port.reportCopy.bits.detail.poke(0.U)
            port.reportCompute.valid.poke(compute(index).exists(_.due <= cycle).B)
            compute(index) match {
              case Some(value) => event(port.reportCompute.bits, value.stage / 2, value.status, if (value.status != 0) 37 else 0)
              case None => event(port.reportCompute.bits, 0)
            }
            port.reportOutput.valid.poke(publish(index).exists(_.due <= cycle).B)
            publish(index) match {
              case Some(value) => event(port.reportOutput.bits, value.stage / 2, value.status, if (value.status != 0) 37 else 0)
              case None => event(port.reportOutput.bits, 0)
            }
          }

          if (dut.io.root.valid.peek().litToBoolean && dut.io.root.ready.peek().litToBoolean) {
            submitted += 1
          }

          dut.io.endpoint.zipWithIndex.foreach { case (port, index) =>
            if (port.reportCopy.valid.peek().litToBoolean && port.reportCopy.ready.peek().litToBoolean) {
              copies(index) = None
            }
            if (port.reportCompute.valid.peek().litToBoolean && port.reportCompute.ready.peek().litToBoolean) {
              val value = compute(index).get
              completed += ((value.stage, value.id))
              compute(index) = None
            }
            if (port.reportOutput.valid.peek().litToBoolean && port.reportOutput.ready.peek().litToBoolean) {
              publish(index) = None
              watched(index) = None
            }
            if (port.watchOutput.valid.peek().litToBoolean && port.watchOutput.ready.peek().litToBoolean) {
              assert(watched(index).isEmpty && compute(index).isEmpty && publish(index).isEmpty)
              watched(index) = Some((port.watchOutput.bits.job.peek().litValue.toInt * 2 + index,
                port.watchOutput.bits.tile.id.peek().litValue.toInt))
            }
            if (port.requestCopy.valid.peek().litToBoolean && port.requestCopy.ready.peek().litToBoolean) {
              assert(copies(index).isEmpty)
              if (!params.endpoints(index).bufferedInput) assert(compute(index).isEmpty)
              copies(index) = Some(Copy(cycle + 2 + index, port.requestCopy.bits.task.peek().litValue.toInt))
            }
            if (port.requestCompute.valid.peek().litToBoolean && port.requestCompute.ready.peek().litToBoolean) {
              val job = port.requestCompute.bits.job.peek().litValue.toInt
              val stage = job * 2 + index
              val id = port.requestCompute.bits.tile.id.peek().litValue.toInt
              assert(!started.contains((stage, id)))
              assert(compute(index).isEmpty && copies(index).isEmpty)
              started += ((stage, id))
              if (stage > 0) assert(started.contains((stage - 1, id)))
              if (port.requestCompute.bits.start.peek().litToBoolean) {
                val status = if (injectFailure && stage == 1 && id == 1) 2 else 0
                val value = Completion(cycle + 10 + index * 7, stage, id, status)
                compute(index) = Some(value)
                watched(index).foreach { watch =>
                  assert(watch == ((stage, id)))
                  publish(index) = Some(value.copy(due = value.due - 2))
                }
              } else {
                assert(injectFailure && id == 1 && stage >= 2)
                completed += ((stage, id))
                watched(index) = None
              }
            }
          }
          if (compute.forall(_.nonEmpty) && compute(0).get.id != compute(1).get.id) overlap = true
          dut.io.result.zipWithIndex.foreach { case (port, stage) =>
            if (port.valid.peek().litToBoolean && port.ready.peek().litToBoolean) {
              assert(started.contains((stage, count - 1)))
              port.bits.stage.expect(stage.U)
              port.bits.status.expect((if (injectFailure && stage > 0) 2 else 0).U)
              if (injectFailure && stage > 0) port.bits.detail.expect(37.U)
              results(stage) += 1
            }
          }
          dut.clock.step()
          cycle += 1
        }
        assert(cycle < deadline)
        assert(results.forall(_ == 1))
        assert(started.size == count * 4)
        if (count > 1) {
          assert(overlap)
        }
      }

      run(6, injectFailure = true)
      run(1, injectFailure = false)
    }
  }

  it should "hold a partial join without replacing its tile with an unmatched dependency" in {
    test(new AutoStage(params, 3)) { dut =>
      bindings(dut.io.transfers, dut.io.regions)
      dut.io.claim.ready.poke(false.B)
      dut.io.slot.poke(0.U)
      dut.io.rearm.poke(true.B)
      dut.io.watchOutput.ready.poke(false.B)
      dut.io.requestCopy.ready.poke(false.B)
      dut.io.requestCompute.ready.poke(false.B)
      dut.io.reportOutput.valid.poke(false.B)
      dut.io.reportCopy.valid.poke(false.B)
      dut.io.reportCompute.valid.poke(false.B)
      dut.io.result.ready.poke(true.B)
      val skip = dut.io.dependency(0)
      val computed = dut.io.dependency(1)
      event(skip.bits.event, 0)
      event(computed.bits.event, 1)
      skip.bits.slot.poke(0.U)
      computed.bits.slot.poke(0.U)
      tile(skip.bits.tile, 2, false)
      tile(computed.bits.tile, 3, false)
      skip.valid.poke(true.B)
      computed.valid.poke(true.B)
      skip.ready.expect(true.B)
      computed.ready.expect(false.B)
      dut.clock.step()
      skip.valid.poke(false.B)
      for (_ <- 0 until 4) {
        computed.ready.expect(false.B)
        dut.io.claim.valid.expect(false.B)
        dut.clock.step()
      }
      tile(computed.bits.tile, 2, false)
      computed.ready.expect(true.B)
      dut.clock.step()
      computed.valid.poke(false.B)
      dut.clock.step()
      dut.io.claim.valid.expect(true.B)
      dut.io.claim.ready.poke(true.B)
      dut.clock.step()
      dut.io.requestCopy.valid.expect(true.B)
      dut.io.requestCopy.bits.tile.id.expect(2.U)
    }
  }
}
