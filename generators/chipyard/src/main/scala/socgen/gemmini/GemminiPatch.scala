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
  val outputBytes = UInt(32.W)
  val maxRows = UInt(32.W)
  val maxColumns = UInt(32.W)
  val outputBase = UInt(64.W)
  val outputSize = UInt(32.W)
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
    val captureValid = Output(Bool())
    val copy = Flipped(Valid(new AutoCopyRequest(params.auto)))
    val request = Input(new AutoComputeRequest(params.auto))
    val watch = Input(new AutoWatch(params.auto))
    val watchValid = Input(Bool())
    val start = Input(Bool())
    val requestValid = Output(Bool())
    val ready = Output(Bool())
    val boundValid = Output(Bool())
    val job = Input(UInt(params.auto.jobWidth.W))
    val index = Input(UInt(params.commandCountWidth.W))
    val command = Input(new RoCCCommand)
    val patched = Output(new RoCCCommand)
  })

  val entries = Reg(Vec(params.jobCount, Vec(params.patchCapacity, new GemminiPatchEntry)))
  val counts = RegInit(VecInit(Seq.fill(params.jobCount)(0.U(32.W))))
  val windows = Reg(Vec(params.jobCount, new GemminiWindow))
  val valid = RegInit(VecInit(Seq.fill(params.jobCount)(false.B)))
  val captureJob = RegInit(0.U(params.jobIndexWidth.W))
  val captured = RegInit(0.U(32.W))
  val commands = Reg(UInt(32.W))
  def selected[T <: Data](values: Vec[T], job: UInt): T =
    if (params.jobCount == 1) values.head else values(job(params.jobIndexWidth - 1, 0))

  io.patch.ready := captured < selected(counts, captureJob)
  io.configured := captured === selected(counts, captureJob)
  io.captureValid := selected(valid, captureJob)
  when(io.begin.valid) {
    captureJob := io.begin.bits.job
    captured := 0.U
    commands := io.begin.bits.commandCount
    selected(counts, io.begin.bits.job) := io.begin.bits.patchCount
    selected(windows, io.begin.bits.job) := io.begin.bits.window
    selected(valid, io.begin.bits.job) := true.B
  }
  when(io.patch.fire) {
    selected(entries, captureJob)(captured(log2Ceil(params.patchCapacity) - 1, 0)) := io.patch.bits
    captured := captured + 1.U
    val overlaps = (0 until params.patchCapacity).map { index =>
      val entry = selected(entries, captureJob)(index)
      index.U < captured && entry.command === io.patch.bits.command &&
        entry.operand === io.patch.bits.operand &&
        entry.lsb < (io.patch.bits.lsb +& io.patch.bits.bitCount) &&
        io.patch.bits.lsb < (entry.lsb +& entry.bitCount)
    }.reduce(_ || _)
    when(overlaps || io.patch.bits.command >= commands || io.patch.bits.bitCount === 0.U ||
        (io.patch.bits.lsb +& io.patch.bits.bitCount) > 64.U || io.patch.bits.source >= GemminiValue.Count.U) {
      selected(valid, captureJob) := false.B
    }
  }

  val view = Reg(new AutoCopyRequest(params.auto))
  val viewValid = RegInit(false.B)
  when(io.copy.valid) {
    view := io.copy.bits
    viewValid := true.B
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
  val viewMatches = viewValid && view.job === io.request.job && view.tile.asUInt === tile.asUInt &&
    view.sourceTile.id === tile.id && sourceRow <= footprint.row && sourceColumn <= footprint.column &&
    (sourceRow +& inputRows) >= (footprint.row +& footprint.rows) &&
    (sourceColumn +& inputColumns) >= (footprint.column +& footprint.columns) &&
    view.bytes === inputRows * inputColumns * config.pixelBytes
  val enabled = selected(counts, io.request.job) =/= 0.U
  io.requestValid := !enabled || (config.region.rows =/= 0.U && config.region.columns =/= 0.U &&
    footprint.rows =/= 0.U && footprint.columns =/= 0.U &&
    tile.rows =/= 0.U && tile.rows <= config.maxRows && tile.columns =/= 0.U && tile.columns <= config.maxColumns &&
    (!inputView || viewMatches) && io.watchValid && io.watch.job === io.request.job &&
    io.watch.address >= config.outputBase &&
    (io.watch.address +& io.watch.bytes) <= (config.outputBase +& config.outputSize) &&
    io.watch.tile.asUInt === tile.asUInt && io.watch.bytes === tile.rows * tile.columns * config.outputBytes)

  val bound = Reg(Vec(GemminiValue.Count, UInt(64.W)))
  val payloads = Reg(Vec(params.patchCapacity, UInt(64.W)))
  val masks = Reg(Vec(params.patchCapacity, UInt(64.W)))
  val bindJob = Reg(UInt(params.jobIndexWidth.W))
  val bindIndex = RegInit(0.U(log2Ceil(params.patchCapacity).W))
  val preparing = RegInit(false.B)
  val boundValid = RegInit(true.B)
  io.ready := !preparing
  io.boundValid := boundValid
  when(io.start) {
    bound := values
    bindJob := io.request.job
    bindIndex := 0.U
    preparing := enabled
    boundValid := true.B
    viewValid := false.B
  }
  // Share one field evaluator; command replay only applies the prepared masks.
  when(preparing) {
    val entry = selected(entries, bindJob)(bindIndex)
    val value = (bound(entry.source) * entry.scale).zext + entry.offset.asSInt
    val mask = (((1.U(65.W) << entry.bitCount) - 1.U)(63, 0) << entry.lsb)(63, 0)
    masks(bindIndex) := mask
    payloads(bindIndex) := (value.asUInt << entry.lsb) & mask
    when(value < 0.S || (value.asUInt >> entry.bitCount) =/= 0.U) {
      boundValid := false.B
    }
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
