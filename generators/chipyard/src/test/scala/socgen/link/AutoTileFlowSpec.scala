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
      input.bits.event.stage.poke(0.U)
      input.bits.event.job.poke(0.U)
      input.bits.event.status.poke(AutoLinkStatus.Success)
      input.bits.event.detail.poke(0.U)
      input.bits.event.data.poke(0.U)

      for (id <- 0 until 2) {
        input.bits.tile.id.poke(id.U)
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
          dut.io.requestCopy.bits.tile.id.expect(id.U)
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
          dut.io.requestCompute.bits.tile.id.expect(id.U)
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
}
