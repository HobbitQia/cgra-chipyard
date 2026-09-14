package chipyard.socgen.link

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AutoBufferSpec extends AnyFlatSpec with ChiselScalatestTester {
  private def event(port: AutoEvent): Unit = {
    port.stage.poke(0.U)
    port.job.poke(0.U)
    port.status.poke(0.U)
    port.detail.poke(0.U)
    port.data.poke(0.U)
  }

  private def tile(port: AutoTile, id: Int, last: Boolean): Unit = {
    port.id.poke(id.U)
    port.row.poke(id.U)
    port.column.poke(0.U)
    port.rows.poke(1.U)
    port.columns.poke(1.U)
    port.last.poke(last.B)
  }

  private def bindings(dut: AutoScheduler, params: AutoLinkParams): Unit = {
    dut.io.transfers.zipWithIndex.foreach { case (transfer, index) =>
      transfer.sourceOffset.poke(0.U)
      transfer.destinationOffset.poke(params.dependencies(index).copy.map(_.destinationOffset).getOrElse(0).U)
      transfer.sourceStride.poke(32.U)
      transfer.destinationStride.poke(64.U)
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
  }

  behavior of "AutoScheduler buffer pools"

  it should "reject direct data dependencies on the same physical endpoint" in {
    val error = intercept[IllegalArgumentException] {
      AutoLinkParams(
        stages = Seq(AutoStageSpec("first", "ip", 0), AutoStageSpec("second", "ip", 1)),
        dependencies = Seq(
          AutoDependencySpec(None, 0, None),
          AutoDependencySpec(Some(0), 1, Some(AutoCopySpec(0, 0, 32)))),
        endpoints = Seq(AutoEndpointSpec("ip", Some(AutoBuffer(0x60000000L, 256)), 256)),
        beatBytes = 16, controlAddress = 0x60020000L, controlBytes = 4096)
    }
    assert(error.getMessage.contains("same physical endpoint"))
  }

  private def exercise(params: AutoLinkParams, triple: Boolean): Unit = {
    test(new AutoScheduler(params)) { dut =>
      bindings(dut, params)
      val count = 6
      case class Work(id: Int, slot: Int, due: Int)
      case class Copy(id: Int, task: Int, slot: Int, due: Int)
      val size = params.endpoints.size
      val compute = Array.fill[Option[Work]](size)(None)
      val publish = Array.fill[Option[Work]](size)(None)
      val copy = Array.fill[Option[Copy]](size)(None)
      val watch = Array.fill[Option[Work]](size)(None)
      val held = scala.collection.mutable.Map.empty[(Int, Int), (Int, Set[Int])]
      val produced = scala.collection.mutable.Set.empty[(Int, Int)]
      val deferred = scala.collection.mutable.Map.empty[(Int, Int), Vector[Copy]]
      val results = Array.fill(size)(0)
      val completed = scala.collection.mutable.Set.empty[(Int, Int)]
      val joinSlots = scala.collection.mutable.Map.empty[Int, Set[Int]]
      var submitted = 0
      var cycle = 0
      var tripleSeen = false
      var advanced = false
      var bufferedOverlap = false

      def consume(value: Copy): Unit = {
        val source = params.dependencies(value.task).source.get
        val key = (source, value.slot)
        assert(held(key)._1 == value.id)
        held(key) = (value.id, held(key)._2 - value.task)
      }

      while ((submitted < count || results.exists(_ == 0) || dut.io.busy.peek().litToBoolean) && cycle < 3000) {
        dut.io.root.valid.poke((submitted < count).B)
        event(dut.io.root.bits.event)
        tile(dut.io.root.bits.tile, submitted, submitted == count - 1)
        dut.io.root.bits.slot.poke(0.U)
        dut.io.result.foreach(_.ready.poke((cycle % 7 != 0).B))
        dut.io.endpoint.zipWithIndex.foreach { case (port, endpoint) =>
          port.watchOutput.ready.poke((cycle % 5 != 0).B)
          port.requestCopy.ready.poke((cycle % 3 != 0).B)
          port.requestCompute.ready.poke((cycle % 4 != 0).B)
          port.reportCopy.valid.poke(copy(endpoint).exists(_.due <= cycle).B)
          port.reportCopy.bits.task.poke(copy(endpoint).map(_.task).getOrElse(0).U)
          port.reportCopy.bits.status.poke(0.U)
          port.reportCopy.bits.detail.poke(0.U)
          port.reportCompute.valid.poke(compute(endpoint).exists(_.due <= cycle).B)
          event(port.reportCompute.bits)
          port.reportOutput.valid.poke(publish(endpoint).exists(_.due <= cycle).B)
          event(port.reportOutput.bits)
        }
        if (dut.io.root.valid.peek().litToBoolean && dut.io.root.ready.peek().litToBoolean) submitted += 1

        val active = compute.indices.flatMap(endpoint => compute(endpoint).map(_.id).orElse(copy(endpoint).map(_.id)))
        if (active.distinct.length >= 3) {
          if (triple) dut.io.activeCount.expect(3.U)
          tripleSeen = true
        }
        dut.io.endpoint.zipWithIndex.foreach { case (port, endpoint) =>
          if (port.reportCopy.valid.peek().litToBoolean && port.reportCopy.ready.peek().litToBoolean) {
            val value = copy(endpoint).get
            if (params.endpoints(endpoint).releaseOnCopy) consume(value)
            else {
              val key = (endpoint, value.id)
              deferred(key) = deferred.getOrElse(key, Vector.empty) :+ value
            }
            copy(endpoint) = None
          }
          if (port.reportCompute.valid.peek().litToBoolean && port.reportCompute.ready.peek().litToBoolean) {
            val value = compute(endpoint).get
            produced += ((endpoint, value.id))
            completed += ((endpoint, value.id))
            deferred.remove((endpoint, value.id)).getOrElse(Vector.empty).foreach(consume)
            compute(endpoint) = None
          }
          if (port.reportOutput.valid.peek().litToBoolean && port.reportOutput.ready.peek().litToBoolean) {
            publish(endpoint) = None
            watch(endpoint) = None
          }
          if (port.watchOutput.valid.peek().litToBoolean && port.watchOutput.ready.peek().litToBoolean) {
            assert(watch(endpoint).isEmpty && compute(endpoint).isEmpty && publish(endpoint).isEmpty)
            val id = port.watchOutput.bits.tile.id.peek().litValue.toInt
            val slot = port.watchOutput.bits.slot.peek().litValue.toInt
            val key = (endpoint, slot)
            held.get(key).foreach { case (previous, consumers) =>
              assert(consumers.isEmpty && produced.contains((endpoint, previous)))
            }
            assert(slot < params.endpoints(endpoint).bufferSlots)
            val tasks = params.dependencies.indices.filter(task =>
              params.dependencies(task).source.contains(endpoint) && params.dependencies(task).copy.nonEmpty).toSet
            held(key) = (id, tasks)
            watch(endpoint) = Some(Work(id, slot, 0))
            if (endpoint == 0 && id >= 2 && !completed.contains((size - 1, id - 2))) advanced = true
          }
          if (port.requestCopy.valid.peek().litToBoolean && port.requestCopy.ready.peek().litToBoolean) {
            assert(copy(endpoint).isEmpty)
            val id = port.requestCopy.bits.tile.id.peek().litValue.toInt
            val task = port.requestCopy.bits.task.peek().litValue.toInt
            val slot = port.requestCopy.bits.sourceSlot.peek().litValue.toInt
            val source = params.dependencies(task).source.get
            assert(held((source, slot))._1 == id)
            port.requestCopy.bits.sourceAddress.expect((params.endpoints(source).buffer.get.baseAddress + slot * 32).U)
            val destinationSlot = port.requestCopy.bits.destinationSlot.peek().litValue.toInt
            if (params.endpoints(endpoint).bufferedInput) {
              val offset = params.dependencies(task).copy.get.destinationOffset
              port.requestCopy.bits.destinationOffset.expect((offset + destinationSlot * 64).U)
              compute(endpoint).foreach { active =>
                assert(id != active.id && destinationSlot != active.slot)
                bufferedOverlap = true
              }
            } else {
              assert(compute(endpoint).isEmpty)
              if (params.dependencies.exists(dependency => dependency.source.contains(endpoint) && dependency.copy.nonEmpty)) {
                assert(watch(endpoint).exists(_.id == id))
              }
            }
            if (endpoint == size - 1) joinSlots(id) = joinSlots.getOrElse(id, Set.empty) + slot
            copy(endpoint) = Some(Copy(id, task, slot, cycle + (if (endpoint == size - 1) 45 else 7)))
          }
          if (port.requestCompute.valid.peek().litToBoolean && port.requestCompute.ready.peek().litToBoolean) {
            assert(compute(endpoint).isEmpty && copy(endpoint).isEmpty)
            port.requestCompute.bits.start.expect(true.B)
            val id = port.requestCompute.bits.tile.id.peek().litValue.toInt
            val slot = port.requestCompute.bits.slot.peek().litValue.toInt
            watch(endpoint).foreach(value => assert(value.id == id && value.slot == slot))
            val value = Work(id, slot, cycle + 25 + endpoint * 5)
            compute(endpoint) = Some(value)
            val delay = if (params.endpoints(endpoint).bufferedInput) 5 else -1
            watch(endpoint).foreach(_ => publish(endpoint) = Some(value.copy(due = value.due + delay)))
          }
        }
        dut.io.result.zipWithIndex.foreach { case (port, stage) =>
          if (port.valid.peek().litToBoolean && port.ready.peek().litToBoolean) {
            port.bits.status.expect(AutoLinkStatus.Success)
            results(stage) += 1
          }
        }
        dut.clock.step()
        cycle += 1
      }
      assert(cycle < 3000)
      assert(results.forall(_ == 1))
      assert(completed.size == count * size)
      assert(held.values.forall(_._2.isEmpty))
      if (triple) {
        assert(tripleSeen)
        assert(advanced)
        assert(bufferedOverlap)
      } else {
        assert(joinSlots.values.exists(_.size > 1))
      }
    }
  }

  it should "overlap three tile identities with two slots per producer and no streaming destination pool" in {
    val params = AutoLinkParams(
      stages = (0 until 3).map(index => AutoStageSpec(s"stage$index", s"ip$index", 0)),
      dependencies = Seq(
        AutoDependencySpec(None, 0, None),
        AutoDependencySpec(Some(0), 1, Some(AutoCopySpec(0, 0, 32))),
        AutoDependencySpec(Some(1), 2, Some(AutoCopySpec(0, 0, 32)))),
      endpoints = Seq(
        AutoEndpointSpec("ip0", Some(AutoBuffer(0x60000000L, 256)), 256, bufferSlots = 2),
        AutoEndpointSpec("ip1", Some(AutoBuffer(0x60010000L, 256)), 256,
          bufferedInput = true, bufferSlots = 2, releaseOnCopy = true),
        AutoEndpointSpec("ip2", None, 256, releaseOnCopy = true)),
      beatBytes = 16, controlAddress = 0x60020000L, controlBytes = 4096)
    exercise(params, triple = true)
  }

  it should "retain fan-out buffers until all consumers finish and join independently allocated source slots" in {
    val params = AutoLinkParams(
      stages = (0 until 4).map(index => AutoStageSpec(s"stage$index", s"ip$index", 0)),
      dependencies = Seq(
        AutoDependencySpec(None, 0, None),
        AutoDependencySpec(Some(0), 1, Some(AutoCopySpec(0, 0, 32))),
        AutoDependencySpec(Some(0), 2, Some(AutoCopySpec(0, 0, 32))),
        AutoDependencySpec(Some(1), 3, Some(AutoCopySpec(0, 0, 32))),
        AutoDependencySpec(Some(2), 3, Some(AutoCopySpec(0, 32, 32)))),
      endpoints = (0 until 4).map(index => AutoEndpointSpec(s"ip$index",
        if (index == 3) None else Some(AutoBuffer(0x60000000L + index * 0x10000L, 256)),
        256, bufferedInput = index > 0, bufferSlots = if (index == 1) 1 else 2,
        releaseOnCopy = index != 2)),
      beatBytes = 16, controlAddress = 0x60040000L, controlBytes = 4096)
    exercise(params, triple = false)
  }
}
