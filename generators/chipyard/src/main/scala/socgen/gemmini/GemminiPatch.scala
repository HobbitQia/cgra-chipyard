package chipyard.socgen.gemmini

import chisel3._
import chisel3.util._
import chipyard.socgen.link._
import chipyard.socgen.generated.CgraLinkControlGenerated._
import freechips.rocketchip.tile.RoCCCommand
import org.chipsalliance.cde.config.Parameters

/** A software-defined rectangular input footprint, independent of native opcodes. */
class GemminiWindow extends Bundle {
  val region = new AutoRegion(32)
  val address = UInt(64.W)
  val pixelBytes = UInt(32.W)
  val rowBytes = UInt(32.W)
}

object GemminiValue {
  val Rows = GEMMINI_VALUE_ROWS
  val Columns = GEMMINI_VALUE_COLUMNS
  val InputRows = GEMMINI_VALUE_INPUT_ROWS
  val InputColumns = GEMMINI_VALUE_INPUT_COLUMNS
  val InputStride = GEMMINI_VALUE_INPUT_STRIDE
  val InputAddress = GEMMINI_VALUE_INPUT_ADDRESS
  val OutputAddress = GEMMINI_VALUE_OUTPUT_ADDRESS
  val Top = GEMMINI_VALUE_TOP
  val Bottom = GEMMINI_VALUE_BOTTOM
  val Left = GEMMINI_VALUE_LEFT
  val Right = GEMMINI_VALUE_RIGHT
  val Slot = GEMMINI_VALUE_SLOT
  val TileId = GEMMINI_VALUE_TILE_ID
  val Count = GEMMINI_VALUE_COUNT
}

class GemminiPatchEntry extends Bundle {
  val command = UInt(32.W)
  val operand = Bool()
  val lsb = UInt(6.W)
  val bitCount = UInt(7.W)
  val source = UInt(4.W)
  val scale = UInt(32.W)
  val offset = UInt(32.W)
}

/** Relocates native command fields using software-supplied tile metadata. */
class GemminiPatch(params: GemminiLinkParams)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val begin = Flipped(Valid(new GemminiLinkConfig(params)))
    val patch = Flipped(Decoupled(new GemminiPatchEntry))
    val configured = Output(Bool())
    val copy = Flipped(Valid(new AutoCopyRequest(params.auto)))
    val request = Input(new AutoComputeRequest(params.auto))
    val watch = Input(new AutoWatch(params.auto))
    val start = Input(Bool())
    val ready = Output(Bool())
    val job = Input(UInt(params.auto.jobWidth.W))
    val index = Input(UInt(params.commandCountWidth.W))
    val command = Input(new RoCCCommand)
    val patched = Output(new RoCCCommand)
  })

  val entries = Reg(Vec(params.jobCount, Vec(params.patchCapacity, new GemminiPatchEntry)))
  val counts = RegInit(VecInit(Seq.fill(params.jobCount)(0.U(32.W))))
  val windows = Reg(Vec(params.jobCount, new GemminiWindow))
  val captureJob = RegInit(0.U(params.jobIndexWidth.W))
  val captured = RegInit(0.U(32.W))
  def selected[T <: Data](values: Vec[T], job: UInt): T =
    if (params.jobCount == 1) values.head else values(job(params.jobIndexWidth - 1, 0))

  io.patch.ready := captured < selected(counts, captureJob)
  io.configured := captured === selected(counts, captureJob)
  when(io.begin.valid) {
    captureJob := io.begin.bits.job
    captured := 0.U
    selected(counts, io.begin.bits.job) := io.begin.bits.patchCount
    selected(windows, io.begin.bits.job) := io.begin.bits.window
  }
  when(io.patch.fire) {
    selected(entries, captureJob)(captured(log2Ceil(params.patchCapacity) - 1, 0)) := io.patch.bits
    captured := captured + 1.U
  }

  val view = Reg(new AutoCopyRequest(params.auto))
  when(io.copy.valid) {
    view := io.copy.bits
  }
  val needsView = VecInit((0 until params.jobCount).map { job =>
    params.auto.dependencies.exists { dependency =>
      val stage = params.auto.stage(dependency.destination)
      stage.endpoint == "gemmini" && stage.job == job && dependency.copy.nonEmpty
    }.B
  })
  val inputView = selected(needsView, io.request.job)
  val config = selected(windows, io.request.job)
  val tile = io.request.tile
  val footprint = AutoTileBinding.region(tile, config.region)
  val firstRow = (tile.row * config.region.rowStep).zext - config.region.top.zext
  val firstColumn = (tile.column * config.region.columnStep).zext - config.region.left.zext
  val endRow = ((tile.row +& tile.rows) * config.region.rowStep).zext + config.region.bottom.asSInt
  val endColumn = ((tile.column +& tile.columns) * config.region.columnStep).zext + config.region.right.asSInt
  val sourceRow = Mux(inputView, view.sourceTile.row, 0.U)
  val sourceColumn = Mux(inputView, view.sourceTile.column, 0.U)
  val inputRows = Mux(inputView, view.sourceTile.rows, config.region.rows)
  val inputColumns = Mux(inputView, view.sourceTile.columns, config.region.columns)
  val rowBytes = Mux(inputView, inputColumns * config.pixelBytes, config.rowBytes)
  val sourceAddress = Mux(inputView, view.sourceAddress, config.address)
  val inputAddress = sourceAddress +& (footprint.row - sourceRow) * rowBytes +&
    (footprint.column - sourceColumn) * config.pixelBytes
  val values = Wire(Vec(GemminiValue.Count, UInt(64.W)))
  values(GemminiValue.Rows) := tile.rows
  values(GemminiValue.Columns) := tile.columns
  values(GemminiValue.InputRows) := inputRows
  values(GemminiValue.InputColumns) := inputColumns
  values(GemminiValue.InputStride) := config.pixelBytes
  values(GemminiValue.InputAddress) := inputAddress
  values(GemminiValue.OutputAddress) := io.watch.address
  values(GemminiValue.Top) := Mux(firstRow < 0.S, -firstRow, 0.S).asUInt
  values(GemminiValue.Bottom) := Mux(endRow > config.region.rows.zext, endRow - config.region.rows.zext, 0.S).asUInt
  values(GemminiValue.Left) := Mux(firstColumn < 0.S, -firstColumn, 0.S).asUInt
  values(GemminiValue.Right) := Mux(endColumn > config.region.columns.zext, endColumn - config.region.columns.zext, 0.S).asUInt
  values(GemminiValue.Slot) := io.request.slot
  values(GemminiValue.TileId) := tile.id
  val enabled = selected(counts, io.request.job) =/= 0.U

  val bound = Reg(Vec(GemminiValue.Count, UInt(64.W)))
  val payloads = Reg(Vec(params.patchCapacity, UInt(64.W)))
  val masks = Reg(Vec(params.patchCapacity, UInt(64.W)))
  val bindJob = Reg(UInt(params.jobIndexWidth.W))
  val bindIndex = RegInit(0.U(log2Ceil(params.patchCapacity).W))
  val preparing = RegInit(false.B)
  io.ready := !preparing
  when(io.start) {
    bound := values
    bindJob := io.request.job
    bindIndex := 0.U
    preparing := enabled
  }
  // Share one field evaluator; command replay only applies the prepared masks.
  when(preparing) {
    val entry = selected(entries, bindJob)(bindIndex)
    val value = (bound(entry.source) * entry.scale)(63, 0) + entry.offset.asSInt.pad(64).asUInt
    val mask = (((1.U(65.W) << entry.bitCount) - 1.U)(63, 0) << entry.lsb)(63, 0)
    masks(bindIndex) := mask
    payloads(bindIndex) := (value << entry.lsb) & mask
    when(bindIndex +& 1.U === selected(counts, bindJob)) {
      preparing := false.B
    }.otherwise {
      bindIndex := bindIndex + 1.U
    }
  }
  def patched(operand: Bool, original: UInt): UInt = {
    val active = (0 until params.patchCapacity).map { index =>
      val entry = selected(entries, io.job)(index)
      index.U < selected(counts, io.job) && entry.command === io.index && entry.operand === operand
    }
    val mask = active.zipWithIndex.map { case (enable, index) => Mux(enable, masks(index), 0.U) }.reduce(_ | _)
    val payload = active.zipWithIndex.map { case (enable, index) => Mux(enable, payloads(index), 0.U) }.reduce(_ | _)
    (original & ~mask) | payload
  }
  io.patched := io.command
  io.patched.rs1 := patched(false.B, io.command.rs1)
  io.patched.rs2 := patched(true.B, io.command.rs2)
}
