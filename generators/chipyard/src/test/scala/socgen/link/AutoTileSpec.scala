package chipyard.socgen.link

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AutoTileSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "AutoTileCursor"

  private def plan(dut: AutoTileCursor, rows: Int, columns: Int, tileRows: Int, tileColumns: Int): Unit = {
    dut.io.start.bits.rows.poke(rows.U)
    dut.io.start.bits.columns.poke(columns.U)
    dut.io.start.bits.tileRows.poke(tileRows.U)
    dut.io.start.bits.tileColumns.poke(tileColumns.U)
  }

  private def tile(dut: AutoTileCursor, id: Int, row: Int, column: Int, rows: Int, columns: Int, last: Boolean): Unit = {
    dut.io.out.valid.expect(true.B)
    dut.io.out.bits.id.expect(id.U)
    dut.io.out.bits.row.expect(row.U)
    dut.io.out.bits.column.expect(column.U)
    dut.io.out.bits.rows.expect(rows.U)
    dut.io.out.bits.columns.expect(columns.U)
    dut.io.out.bits.last.expect(last.B)
    dut.io.busy.expect(true.B)
    dut.io.start.ready.expect(false.B)
  }

  it should "clip row-major tails, hold stalled tiles and restart after the last handshake" in {
    test(new AutoTileCursor(4)) { dut =>
      dut.io.start.valid.poke(false.B)
      dut.io.out.ready.poke(false.B)
      dut.io.busy.expect(false.B)
      dut.io.out.valid.expect(false.B)
      plan(dut, 5, 7, 2, 3)
      dut.io.start.valid.poke(true.B)
      dut.io.start.ready.expect(true.B)
      dut.io.error.expect(false.B)
      dut.clock.step()

      // A competing start must not replace the active geometry.
      plan(dut, 1, 1, 15, 15)
      for ((origin, id) <- (for (row <- 0 until 5 by 2; column <- 0 until 7 by 3) yield (row, column)).zipWithIndex) {
        val (row, column) = origin
        val rows = math.min(2, 5 - row)
        val columns = math.min(3, 7 - column)
        val last = id == 8
        dut.io.out.ready.poke(false.B)
        for (_ <- 0 until 3) {
          tile(dut, id, row, column, rows, columns, last)
          dut.io.error.expect(false.B)
          dut.clock.step()
        }
        dut.io.out.ready.poke(true.B)
        tile(dut, id, row, column, rows, columns, last)
        dut.clock.step()
      }
      dut.io.busy.expect(false.B)
      dut.io.out.valid.expect(false.B)
      dut.io.start.ready.expect(true.B)
      dut.clock.step()
      dut.io.start.valid.poke(false.B)
      tile(dut, 0, 0, 0, 1, 1, last = true)
      dut.clock.step()
      dut.io.busy.expect(false.B)
      dut.io.out.valid.expect(false.B)
    }
  }

  it should "accept each zero dimension as an error without entering a run" in {
    test(new AutoTileCursor(4)) { dut =>
      dut.io.start.valid.poke(false.B)
      dut.io.out.ready.poke(true.B)
      for (shape <- Seq((0, 7, 2, 3), (5, 0, 2, 3), (5, 7, 0, 3), (5, 7, 2, 0))) {
        plan(dut, shape._1, shape._2, shape._3, shape._4)
        dut.io.start.valid.poke(true.B)
        dut.io.start.ready.expect(true.B)
        dut.io.error.expect(true.B)
        dut.io.out.valid.expect(false.B)
        dut.clock.step()
        dut.io.start.valid.poke(false.B)
        dut.io.error.expect(false.B)
        dut.io.busy.expect(false.B)
        dut.io.out.valid.expect(false.B)
        dut.clock.step()
      }
      plan(dut, 1, 1, 1, 1)
      dut.io.start.valid.poke(true.B)
      dut.io.error.expect(false.B)
      dut.clock.step()
      dut.io.start.valid.poke(false.B)
      tile(dut, 0, 0, 0, 1, 1, last = true)
      dut.clock.step()
      dut.io.busy.expect(false.B)
    }
  }

  it should "keep tile identities distinct when the tile count exceeds a coordinate width" in {
    test(new AutoTileCursor(4)) { dut =>
      dut.io.start.valid.poke(true.B)
      dut.io.out.ready.poke(true.B)
      plan(dut, 15, 15, 1, 1)
      dut.clock.step()
      dut.io.start.valid.poke(false.B)
      for (id <- 0 until 225) {
        tile(dut, id, id / 15, id % 15, 1, 1, last = id == 224)
        dut.clock.step()
      }
      dut.io.busy.expect(false.B)
      dut.io.out.valid.expect(false.B)
    }
  }
}
