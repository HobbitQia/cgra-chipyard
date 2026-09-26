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
  val sourceBase = UInt(params.addressWidth.W)
  val sourceOffset = UInt(params.addressWidth.W)
  val destinationOffset = UInt(params.addressWidth.W)
  val sourceStride = UInt(params.addressWidth.W)
  val destinationStride = UInt(params.addressWidth.W)
  val bytesPerPixel = UInt(params.lengthWidth.W)
}

class AutoOutput(params: AutoLinkParams) extends Bundle {
  val writeback = Bool()
  val packed = Bool()
  val sourceOffset = UInt(params.addressWidth.W)
  val sourceStride = UInt(params.addressWidth.W)
  val address = UInt(params.addressWidth.W)
  val stride = UInt(params.addressWidth.W)
  val bytes = UInt(params.lengthWidth.W)
  val bytesPerPixel = UInt(params.lengthWidth.W)
}

class AutoGraphStage(params: AutoLinkParams) extends Bundle {
  val enabled = Bool()
  val endpoint = UInt(params.endpointWidth.W)
  val output = new AutoOutput(params)
}

class AutoGraphEdge(params: AutoLinkParams) extends Bundle {
  val enabled = Bool()
  val root = Bool()
  val source = UInt(params.stageWidth.W)
  val destination = UInt(params.stageWidth.W)
  val copy = Bool()
  val bytes = UInt(params.lengthWidth.W)
  val expansion = UInt(3.W)
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
  val transfers = Vec(params.dependencyCount, new AutoTransfer(params))
  val regions = Vec(params.stageCount, new AutoRegion(params.lengthWidth))
  val jobs = Vec(params.stageCount, UInt(params.jobWidth.W))
  val stages = Vec(params.stageCount, new AutoGraphStage(params))
  val edges = Vec(params.dependencyCount, new AutoGraphEdge(params))
}

class AutoProgress extends Bundle {
  val running = Bool()
  val emitting = Bool()
  val done = Bool()
  val failed = Bool()
  val cycles = UInt(64.W)
  val overlap = UInt(64.W)
  val peakActive = UInt(64.W)
}

object AutoTileBinding {
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
    val values = WireDefault(0.U.asTypeOf(Vec(params.dependencyCount, new AutoTransfer(params))))
    params.dependencies.zipWithIndex.foreach { case (dependency, index) =>
      dependency.copy.foreach { copy =>
        values(index).sourceBase := copy.sourceAddress.getOrElse(dependency.source.map(source =>
          params.endpoint(params.stage(source).endpoint).buffer.get.baseAddress).getOrElse(BigInt(0))).U
        values(index).sourceOffset := copy.sourceOffset.U
        values(index).destinationOffset := copy.destinationOffset.U
      }
    }
    values
  }

  def stages(params: AutoLinkParams): Vec[AutoGraphStage] = {
    val values = WireDefault(0.U.asTypeOf(Vec(params.stageCount, new AutoGraphStage(params))))
    params.stages.zipWithIndex.foreach { case (stage, index) =>
      values(index).enabled := true.B
      values(index).endpoint := params.endpoints.indexWhere(_.name == stage.endpoint).U
    }
    values
  }

  def edges(params: AutoLinkParams): Vec[AutoGraphEdge] = {
    val values = WireDefault(0.U.asTypeOf(Vec(params.dependencyCount, new AutoGraphEdge(params))))
    params.dependencies.zipWithIndex.foreach { case (edge, index) =>
      values(index).enabled := true.B
      values(index).root := edge.source.isEmpty.B
      values(index).source := edge.source.getOrElse(0).U
      values(index).destination := edge.destination.U
      values(index).copy := edge.copy.nonEmpty.B
      edge.copy.foreach { copy =>
        values(index).bytes := copy.bytes.U
        values(index).expansion := log2Ceil(copy.expansion).U
      }
    }
    values
  }

  def jobs(params: AutoLinkParams): Vec[UInt] = VecInit(
    (0 until params.stageCount).map(index => params.stages.lift(index).map(_.job).getOrElse(0).U(params.jobWidth.W)))
}

class AutoTileCursor(width: Int = 32) extends Module {
  require(width > 0)

  val io = IO(new Bundle {
    val start = Flipped(Decoupled(new AutoTilePlan(width)))
    val out = Decoupled(new AutoTile(width))
    val busy = Output(Bool())
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
  io.start.ready := !active
  io.out.valid := active
  io.out.bits.id := id
  io.out.bits.row := row
  io.out.bits.column := column
  io.out.bits.rows := rows
  io.out.bits.columns := columns
  io.out.bits.last := lastRow && lastColumn
  io.busy := active

  when(io.start.fire) {
    plan := io.start.bits
    row := 0.U
    column := 0.U
    id := 0.U
    active := true.B
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
