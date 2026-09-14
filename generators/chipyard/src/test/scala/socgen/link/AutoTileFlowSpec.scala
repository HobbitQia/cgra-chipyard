package chipyard.socgen.link

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AutoTileFlowSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val params = AutoLinkParams(
    stages = Seq(AutoStageSpec("source", "gemmini", 0), AutoStageSpec("sink", "cgra", 0)),
    dependencies = Seq(AutoDependencySpec(Some(0), 1, Some(AutoCopySpec(0, 0, 32)))),
    endpoints = Seq(
      AutoEndpointSpec("gemmini", Some(AutoBuffer(0x60000000L, 256)), 256),
      AutoEndpointSpec("cgra", None, 512)),
    beatBytes = 16,
    controlAddress = 0x60020000L,
    controlBytes = 4096)

  behavior of "AutoStage tile context"

  it should "retain each dependency context through copy, compute and rearm" in {
    test(new AutoStage(params, 1)) { dut =>
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
      dut.io.claim.ready.poke(true.B)
      dut.io.slot.poke(0.U)
      dut.io.watchOutput.ready.poke(true.B)
      dut.io.requestCopy.ready.poke(false.B)
      dut.io.requestCompute.ready.poke(false.B)
      dut.io.reportCopy.valid.poke(false.B)
      dut.io.reportOutput.valid.poke(false.B)
      dut.io.reportCompute.valid.poke(false.B)
      dut.io.result.ready.poke(true.B)
      dut.io.rearm.poke(false.B)
      val input = dut.io.dependency.head
      input.valid.poke(false.B)
      input.bits.slot.poke(0.U)
      input.bits.event.stage.poke(0.U)
      input.bits.event.job.poke(0.U)
      input.bits.event.status.poke(AutoLinkStatus.Success)
      input.bits.event.detail.poke(0.U)
      input.bits.event.data.poke(0.U)

      for (id <- 0 until 2) {
        val tileId = (BigInt(1) << params.lengthWidth) + id
        input.bits.tile.id.poke(tileId.U)
        input.bits.tile.row.poke((id * 2).U)
        input.bits.tile.column.poke(0.U)
        input.bits.tile.rows.poke((2 - id).U)
        input.bits.tile.columns.poke(3.U)
        input.bits.tile.last.poke((id == 1).B)
        input.valid.poke(true.B)
        input.ready.expect(true.B)
        dut.clock.step()
        input.valid.poke(false.B)
        dut.clock.step(2)
        for (_ <- 0 until 3) {
          dut.io.requestCopy.valid.expect(true.B)
          dut.io.requestCopy.bits.tile.id.expect(tileId.U)
          dut.io.workingTile.expect(tileId.U)
          dut.io.requestCopy.bits.tile.rows.expect((2 - id).U)
          dut.clock.step()
        }
        dut.io.requestCopy.ready.poke(true.B)
        dut.clock.step()
        dut.io.requestCopy.ready.poke(false.B)
        dut.io.reportCopy.bits.task.poke(0.U)
        dut.io.reportCopy.bits.status.poke(AutoLinkStatus.Success)
        dut.io.reportCopy.bits.detail.poke(0.U)
        dut.io.reportCopy.valid.poke(true.B)
        dut.io.reportCopy.ready.expect(true.B)
        dut.clock.step()
        dut.io.reportCopy.valid.poke(false.B)
        for (_ <- 0 until 3) {
          dut.io.requestCompute.valid.expect(true.B)
          dut.io.requestCompute.bits.tile.id.expect(tileId.U)
          dut.io.workingTile.expect(tileId.U)
          dut.io.requestCompute.bits.tile.row.expect((id * 2).U)
          dut.io.requestCompute.bits.tile.columns.expect(3.U)
          dut.io.requestCompute.bits.tile.last.expect((id == 1).B)
          dut.clock.step()
        }
        dut.io.requestCompute.ready.poke(true.B)
        dut.clock.step()
        dut.io.requestCompute.ready.poke(false.B)
        dut.io.reportCompute.bits.stage.poke(0.U)
        dut.io.reportCompute.bits.job.poke(0.U)
        dut.io.reportCompute.bits.status.poke(AutoLinkStatus.Success)
        dut.io.reportCompute.bits.detail.poke(0.U)
        dut.io.reportCompute.bits.data.poke(0.U)
        dut.io.reportCompute.valid.poke(true.B)
        dut.io.reportCompute.ready.expect(true.B)
        dut.clock.step()
        dut.io.reportCompute.valid.poke(false.B)
        dut.clock.step(4)
        dut.io.finished.expect(true.B)
        dut.io.rearm.poke(true.B)
        dut.clock.step()
        dut.io.rearm.poke(false.B)
      }
    }
  }

  it should "prepare a failed next tile while retaining the active publication and final error" in {
    val buffered = params.copy(
      stages = params.stages :+ AutoStageSpec("output", "stream", 0),
      dependencies = params.dependencies :+ AutoDependencySpec(Some(1), 2, Some(AutoCopySpec(0, 0, 32))),
      endpoints = Seq(
        params.endpoints.head.copy(bufferSlots = 2),
        AutoEndpointSpec("cgra", Some(AutoBuffer(0x60010000L, 512)), 512,
          bufferedInput = true, bufferSlots = 2, releaseOnCopy = true),
        AutoEndpointSpec("stream", None, 512)))
    test(new AutoStage(buffered, 1)) { dut =>
      dut.io.transfers.foreach { transfer =>
        transfer.sourceOffset.poke(0.U)
        transfer.destinationOffset.poke(0.U)
        transfer.sourceStride.poke(64.U)
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
      def event(port: AutoEvent): Unit = {
        port.stage.poke(0.U)
        port.job.poke(0.U)
        port.status.poke(AutoLinkStatus.Success)
        port.detail.poke(0.U)
        port.data.poke(0.U)
      }
      def waitFor(signal: Bool): Unit = {
        var cycles = 0
        while (!signal.peek().litToBoolean && cycles < 30) {
          dut.clock.step()
          cycles += 1
        }
        signal.expect(true.B)
      }
      val input = dut.io.dependency.head
      input.valid.poke(false.B)
      event(input.bits.event)
      event(dut.io.reportCompute.bits)
      event(dut.io.reportOutput.bits)
      dut.io.claim.ready.poke(true.B)
      dut.io.execute.ready.poke(true.B)
      dut.io.slot.poke(0.U)
      dut.io.rearm.poke(true.B)
      dut.io.watchOutput.ready.poke(true.B)
      dut.io.requestCopy.ready.poke(false.B)
      dut.io.requestCompute.ready.poke(false.B)
      dut.io.reportCopy.valid.poke(false.B)
      dut.io.reportCopy.bits.task.poke(0.U)
      dut.io.reportCopy.bits.status.poke(AutoLinkStatus.Success)
      dut.io.reportCopy.bits.detail.poke(0.U)
      dut.io.reportCompute.valid.poke(false.B)
      dut.io.reportOutput.valid.poke(false.B)
      dut.io.output.head.ready.poke(false.B)
      dut.io.result.ready.poke(false.B)

      def prepare(id: Int, fail: Boolean): Unit = {
        dut.io.slot.poke(id.U)
        input.bits.slot.poke(id.U)
        input.bits.tile.id.poke(id.U)
        input.bits.tile.row.poke(id.U)
        input.bits.tile.column.poke(0.U)
        input.bits.tile.rows.poke((2 - id).U)
        input.bits.tile.columns.poke(4.U)
        input.bits.tile.last.poke((id == 1).B)
        input.valid.poke(true.B)
        waitFor(input.ready)
        dut.clock.step()
        input.valid.poke(false.B)
        waitFor(dut.io.requestCopy.valid)
        dut.io.requestCopy.bits.tile.id.expect(id.U)
        dut.io.requestCopy.bits.destinationSlot.expect(id.U)
        dut.io.requestCopy.bits.destinationOffset.expect((id * 64).U)
        dut.io.requestCopy.ready.poke(true.B)
        dut.clock.step()
        dut.io.requestCopy.ready.poke(false.B)
        dut.io.reportCopy.bits.status.poke((if (fail) 2 else 0).U)
        dut.io.reportCopy.bits.detail.poke((if (fail) 19 else 0).U)
        dut.io.reportCopy.valid.poke(true.B)
        dut.io.reportCopy.ready.expect(true.B)
        dut.io.consumed.head.valid.expect(true.B)
        dut.io.consumed.head.bits.expect(id.U)
        dut.clock.step()
        dut.io.reportCopy.valid.poke(false.B)
      }

      prepare(0, fail = false)
      waitFor(dut.io.requestCompute.valid)
      input.bits.tile.id.poke(1.U)
      input.bits.tile.rows.poke(1.U)
      input.bits.tile.last.poke(true.B)
      input.valid.poke(true.B)
      for (_ <- 0 until 3) {
        input.ready.expect(false.B)
        dut.io.requestCopy.valid.expect(false.B)
        dut.io.requestCompute.bits.tile.id.expect(0.U)
        dut.io.requestCompute.bits.tile.rows.expect(2.U)
        dut.clock.step()
      }
      input.valid.poke(false.B)
      dut.io.requestCompute.ready.poke(true.B)
      dut.clock.step()
      dut.io.requestCompute.ready.poke(false.B)

      prepare(1, fail = true)
      for (_ <- 0 until 3) {
        dut.io.reportCompute.ready.expect(true.B)
        dut.io.execute.valid.expect(false.B)
        dut.io.watchOutput.valid.expect(false.B)
        dut.io.requestCompute.valid.expect(false.B)
        dut.clock.step()
      }
      dut.io.reportCompute.valid.poke(true.B)
      dut.io.consumed.head.valid.expect(false.B)
      dut.clock.step()
      dut.io.reportCompute.valid.poke(false.B)
      dut.clock.step(2)
      dut.io.execute.valid.expect(false.B)
      dut.io.reportOutput.valid.poke(true.B)
      dut.io.reportOutput.ready.expect(true.B)
      dut.clock.step()
      dut.io.reportOutput.valid.poke(false.B)
      for (_ <- 0 until 3) {
        dut.io.output.head.valid.expect(true.B)
        dut.io.output.head.bits.tile.id.expect(0.U)
        dut.io.output.head.bits.slot.expect(0.U)
        dut.io.output.head.bits.event.status.expect(AutoLinkStatus.Success)
        dut.io.execute.valid.expect(false.B)
        dut.clock.step()
      }
      dut.io.output.head.ready.poke(true.B)
      waitFor(dut.io.release)
      dut.io.releaseTile.id.expect(0.U)
      dut.io.releaseSlot.expect(0.U)
      dut.clock.step()
      waitFor(dut.io.requestCompute.valid)
      dut.io.requestCompute.bits.start.expect(false.B)
      dut.io.requestCompute.bits.tile.id.expect(1.U)
      dut.io.requestCompute.bits.slot.expect(1.U)
      dut.io.watchOutput.valid.expect(false.B)
      dut.io.consumed.head.valid.expect(false.B)
      dut.io.requestCompute.ready.poke(true.B)
      dut.clock.step()
      dut.io.requestCompute.ready.poke(false.B)
      dut.io.output.head.valid.expect(true.B)
      dut.io.output.head.bits.tile.id.expect(1.U)
      dut.io.output.head.bits.slot.expect(1.U)
      dut.io.output.head.bits.event.status.expect(AutoLinkStatus.SinkFailure)
      waitFor(dut.io.result.valid)
      dut.io.result.bits.status.expect(AutoLinkStatus.SinkFailure)
      dut.io.result.bits.detail.expect(19.U)
      dut.io.result.ready.poke(true.B)
      waitFor(dut.io.release)
      dut.io.releaseTile.id.expect(1.U)
      dut.io.releaseSlot.expect(1.U)
      dut.clock.step(3)
      dut.io.busy.expect(false.B)
    }
  }
}
