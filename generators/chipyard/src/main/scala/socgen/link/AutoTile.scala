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

class AutoTransfer(params: AutoLinkParams) extends Bundle {
  val sourceOffset = UInt(params.addressWidth.W)
  val destinationOffset = UInt(params.addressWidth.W)
  val sourceStride = UInt(params.addressWidth.W)
  val destinationStride = UInt(params.addressWidth.W)
  val bytesPerPixel = UInt(params.lengthWidth.W)
}

class AutoRegion(width: Int) extends Bundle {
  val rows = UInt(width.W)
  val columns = UInt(width.W)
  val rowStep = UInt(width.W)
  val columnStep = UInt(width.W)
  val top = UInt(width.W)
  val bottom = UInt(width.W)
  val left = UInt(width.W)
  val right = UInt(width.W)
}

class AutoRun(params: AutoLinkParams) extends Bundle {
  val plan = new AutoTilePlan(params.lengthWidth)
  val transfers = Vec(params.dependencies.size, new AutoTransfer(params))
  val regions = Vec(params.stages.size, new AutoRegion(params.lengthWidth))
}

class AutoProgress extends Bundle {
  val running = Bool()
  val emitting = Bool()
  val cycles = UInt(64.W)
  val overlap = UInt(64.W)
  val peakActive = UInt(64.W)
}

object AutoTileBinding {
  def transferValid(params: AutoLinkParams, index: Int, transfer: AutoTransfer, multipleSlots: Bool): Bool = {
    val dependency = params.dependencies(index)
    val copy = dependency.copy.get
    val sourceEndpoint = params.endpoint(params.stage(dependency.source.get).endpoint)
    val source = sourceEndpoint.buffer.get
    val destination = params.endpoint(params.stage(dependency.destination).endpoint)
    val destinationSlots = if (destination.bufferedInput) destination.bufferSlots else 1
    val sourceEnd = transfer.sourceOffset +& transfer.sourceStride * (sourceEndpoint.bufferSlots - 1).U +& copy.bytes.U
    val destinationEnd = transfer.destinationOffset +& transfer.destinationStride * (destinationSlots - 1).U +& copy.destinationBytes.U
    val separate = ((sourceEndpoint.bufferSlots == 1).B || transfer.sourceStride >= copy.bytes.U) &&
      ((destinationSlots == 1).B || transfer.destinationStride >= copy.destinationBytes.U)
    val aligned = if (destination.bufferedInput) {
      val alignment = if (copy.expansion == 1) params.beatBytes else destination.inputAlignment
      val destinationAligned = ((transfer.destinationOffset | transfer.destinationStride) & (alignment - 1).U) === 0.U
      val sourceAligned = if (copy.expansion == 1) {
        (((source.baseAddress.U + transfer.sourceOffset) | transfer.sourceStride) & (params.beatBytes - 1).U) === 0.U
      } else true.B
      destinationAligned && sourceAligned
    } else true.B
    sourceEnd <= source.sizeBytes.U && destinationEnd <= destination.localBytes.U &&
      transfer.bytesPerPixel <= copy.bytes.U && (!multipleSlots || separate) && aligned
  }

  def region(tile: AutoTile, shape: AutoRegion): AutoTile = {
    val mapped = WireDefault(tile)
    val row = tile.row * shape.rowStep
    val column = tile.column * shape.columnStep
    val endRow = ((tile.row +& tile.rows) * shape.rowStep).zext + shape.bottom.asSInt
    val endColumn = ((tile.column +& tile.columns) * shape.columnStep).zext + shape.right.asSInt
    val firstRow = Mux(row < shape.top, 0.U, row - shape.top)
    val firstColumn = Mux(column < shape.left, 0.U, column - shape.left)
    val lastRow = Mux(endRow > shape.rows.zext, shape.rows.zext, endRow)
    val lastColumn = Mux(endColumn > shape.columns.zext, shape.columns.zext, endColumn)
    when(shape.rows =/= 0.U) {
      mapped.row := firstRow
      mapped.column := firstColumn
      mapped.rows := Mux(lastRow > firstRow.zext, (lastRow - firstRow.zext).asUInt, 0.U)
      mapped.columns := Mux(lastColumn > firstColumn.zext, (lastColumn - firstColumn.zext).asUInt, 0.U)
    }
    mapped
  }

  def defaults(params: AutoLinkParams): Vec[AutoTransfer] = {
    val values = WireDefault(0.U.asTypeOf(Vec(params.dependencies.size, new AutoTransfer(params))))
    params.dependencies.zipWithIndex.foreach { case (dependency, index) =>
      dependency.copy.foreach { copy =>
        values(index).sourceOffset := copy.sourceOffset.U
        values(index).destinationOffset := copy.destinationOffset.U
      }
    }
    values
  }
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
