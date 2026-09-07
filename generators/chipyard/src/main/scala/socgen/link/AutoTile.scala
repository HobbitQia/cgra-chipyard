package chipyard.socgen.link

import chisel3._
import chisel3.util._

class AutoTilePlan(width: Int = 32) extends Bundle {
  val rows = UInt(width.W)
  val columns = UInt(width.W)
  val tileRows = UInt(width.W)
  val tileColumns = UInt(width.W)
}

class AutoTile(width: Int = 32) extends Bundle {
  val id = UInt((2 * width).W)
  val row = UInt(width.W)
  val column = UInt(width.W)
  val rows = UInt(width.W)
  val columns = UInt(width.W)
  val last = Bool()
}

class AutoTileCursor(width: Int = 32) extends Module {
  require(width > 0)

  val io = IO(new Bundle {
    val start = Flipped(Decoupled(new AutoTilePlan(width)))
    val out = Decoupled(new AutoTile(width))
    val busy = Output(Bool())
    val error = Output(Bool())
  })

  val active = RegInit(false.B)
  val plan = Reg(new AutoTilePlan(width))
  val row = RegInit(0.U(width.W))
  val column = RegInit(0.U(width.W))
  val id = RegInit(0.U((2 * width).W))
  val remainingRows = plan.rows - row
  val remainingColumns = plan.columns - column
  val rows = Mux(remainingRows < plan.tileRows, remainingRows, plan.tileRows)
  val columns = Mux(remainingColumns < plan.tileColumns, remainingColumns, plan.tileColumns)
  val lastRow = rows === remainingRows
  val lastColumn = columns === remainingColumns
  val validPlan = io.start.bits.rows =/= 0.U && io.start.bits.columns =/= 0.U &&
    io.start.bits.tileRows =/= 0.U && io.start.bits.tileColumns =/= 0.U

  io.start.ready := !active
  io.out.valid := active
  io.out.bits.id := id
  io.out.bits.row := row
  io.out.bits.column := column
  io.out.bits.rows := rows
  io.out.bits.columns := columns
  io.out.bits.last := lastRow && lastColumn
  io.busy := active
  io.error := io.start.fire && !validPlan

  when(io.start.fire) {
    plan := io.start.bits
    row := 0.U
    column := 0.U
    id := 0.U
    active := validPlan
  }
  when(io.out.fire) {
    when(io.out.bits.last) {
      active := false.B
    }.otherwise {
      id := id + 1.U
      when(lastColumn) {
        row := row + rows
        column := 0.U
      }.otherwise {
        column := column + columns
      }
    }
  }
}
