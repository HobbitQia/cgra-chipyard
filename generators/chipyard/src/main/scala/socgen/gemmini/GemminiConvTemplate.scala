package chipyard.socgen.gemmini

import chisel3._
import chisel3.util._
import chipyard.socgen.link._
import freechips.rocketchip.tile.RoCCCommand
import gemmini.GemminiISA._
import org.chipsalliance.cde.config.Parameters

case class GemminiConvParams(dim: Int, spmRows: Int, accRows: Int, elementBytes: Int,
    spmBase: BigInt, spmBytes: Int, addressBits: Int = 64) {
  require(isPow2(dim) && spmRows > 0 && accRows > 0)
  require(elementBytes == 1)
  require(spmBytes == spmRows * dim * elementBytes)
  require(addressBits > 0 && addressBits <= 64)
  require(spmBase >= 0 && spmBase + spmBytes <= (BigInt(1) << addressBits))
}

class GemminiConvShape extends Bundle {
  val rows = UInt(16.W)
  val columns = UInt(16.W)
  val inputs = UInt(16.W)
  val outputs = UInt(16.W)
  val channels = UInt(16.W)
  val kernel = UInt(16.W)
  val kernelRows = UInt(16.W)
  val kernelColumns = UInt(16.W)
  val kernelChannels = UInt(16.W)
  val stride = UInt(8.W)
  val padding = UInt(8.W)
  val dilation = UInt(10.W)
  val inputStride = UInt(16.W)
  val outputStride = UInt(16.W)
  val storeStride = UInt(32.W)
  val executeConfigured = Bool()
  val storeConfigured = Bool()
  val left = UInt(16.W)
  val top = UInt(16.W)
  val input = UInt(64.W)
  val aHalf = UInt(2.W)
  val bHalf = UInt(2.W)
}

class GemminiConvBinding extends Bundle {
  val rows = UInt(16.W)
  val columns = UInt(16.W)
  val left = UInt(16.W)
  val right = UInt(16.W)
  val top = UInt(16.W)
  val bottom = UInt(8.W)
  val input = UInt(64.W)
  val output = UInt(64.W)
  val inputRows = UInt(16.W)
  val inputColumns = UInt(16.W)
  val inputStride = UInt(16.W)
}

/** Binds a captured native Conv to a full image or compact upstream view. */
class GemminiConvTemplate(params: GemminiLinkParams, native: GemminiConvParams)(implicit p: Parameters)
    extends Module {
  val io = IO(new Bundle {
    val begin = Flipped(Valid(new GemminiLinkConfig(params)))
    val capture = Flipped(Valid(new RoCCCommand))
    val captureValid = Output(Bool())
    val copy = Flipped(Valid(new AutoCopyRequest(params.auto)))
    val request = Input(new AutoComputeRequest(params.auto))
    val start = Input(Bool())
    val watch = Input(new AutoWatch(params.auto))
    val watchValid = Input(Bool())
    val requestValid = Output(Bool())
    val command = Input(new RoCCCommand)
    val patched = Output(new RoCCCommand)
  })

  val shapes = Reg(Vec(params.jobCount, new GemminiConvShape))
  val enabled = RegInit(VecInit(Seq.fill(params.jobCount)(false.B)))
  val seen = RegInit(VecInit(Seq.fill(params.jobCount)(0.U(7.W))))
  val failed = RegInit(VecInit(Seq.fill(params.jobCount)(false.B)))
  val view = Reg(new AutoCopyRequest(params.auto))
  val viewValid = RegInit(false.B)
  val captureJob = RegInit(0.U(params.jobIndexWidth.W))
  def selected[T <: Data](values: Vec[T], job: UInt): T = {
    if (params.jobCount == 1) values.head else values(job(params.jobIndexWidth - 1, 0))
  }
  val shape = selected(shapes, captureJob)
  val captureEnabled = selected(enabled, captureJob)
  val captured = selected(seen, captureJob)
  val badCapture = selected(failed, captureJob)
  val funct = io.capture.bits.inst.funct
  val rs1 = io.capture.bits.rs1
  val rs2 = io.capture.bits.rs2

  when(io.begin.valid) {
    viewValid := false.B
    captureJob := io.begin.bits.job
    selected(enabled, io.begin.bits.job) := io.begin.bits.convTemplate
    selected(seen, io.begin.bits.job) := 0.U
    selected(failed, io.begin.bits.job) := false.B
    selected(shapes, io.begin.bits.job) := 0.U.asTypeOf(new GemminiConvShape)
  }
  when(io.copy.valid) {
    view := io.copy.bits
    viewValid := true.B
  }
  when(io.start) { viewValid := false.B }
  when(io.capture.valid && captureEnabled) {
    val isConfig = funct >= LOOP_CONV_WS_CONFIG_1 && funct <= LOOP_CONV_WS_CONFIG_6
    val isLaunch = funct === LOOP_CONV_WS
    val isExecute = funct === CONFIG_CMD && rs1(1, 0) === CONFIG_EX
    val isStore = funct === CONFIG_CMD && rs1(1, 0) === CONFIG_STORE
    val index = Mux(isLaunch, 6.U, funct - LOOP_CONV_WS_CONFIG_1)
    when(isConfig || isLaunch) {
      val mask = (1.U(7.W) << index)(6, 0)
      captured := captured | mask
      when((captured & mask).orR || captured(6) ||
          (isLaunch && (captured =/= 63.U || !shape.executeConfigured || !shape.storeConfigured))) {
        badCapture := true.B
      }
    }
    when(captured(6) && (isExecute || isStore)) {
      badCapture := true.B
    }
    when(isExecute && !rs1(7)) {
      shape.executeConfigured := true.B
      when(rs1(2) =/= gemmini.Dataflow.WS.id.U || rs1(9, 8).orR) {
        badCapture := true.B
      }
    }
    when(isStore) {
      shape.storeConfigured := true.B
      shape.storeStride := rs2(31, 0)
      when(rs1(5, 4).orR) { badCapture := true.B }
    }
    switch(funct) {
      is(LOOP_CONV_WS_CONFIG_1) {
        shape.rows := rs1(31, 16)
        shape.inputs := rs1(47, 32)
        shape.outputs := rs1(63, 48)
        shape.stride := rs2(55, 48)
        shape.padding := rs2(63, 56)
        when(rs1(15, 0) =/= 1.U) { badCapture := true.B }
      }
      is(LOOP_CONV_WS_CONFIG_2) {
        shape.kernel := rs1(63, 48)
        shape.channels := rs2(15, 0)
        when(rs2(63, 48) =/= 1.U || rs1(31, 0) =/= "h00010100".U) {
          badCapture := true.B
        }
      }
      is(LOOP_CONV_WS_CONFIG_3) {
        shape.columns := rs2(15, 0)
        shape.kernelRows := rs1(63, 48)
        shape.kernelColumns := rs1(47, 32)
        shape.kernelChannels := rs1(31, 16)
        shape.left := rs1(15, 0)
        shape.top := rs2(47, 32)
        when(rs2(23, 16) =/= 0.U) { badCapture := true.B }
      }
      is(LOOP_CONV_WS_CONFIG_4) {
        shape.dilation := rs1(9, 0)
        shape.inputStride := rs2(63, 48)
        shape.outputStride := rs2(31, 16)
        when(rs1(47, 10) =/= 0.U) { badCapture := true.B }
      }
      is(LOOP_CONV_WS_CONFIG_5) {
        when(rs1 === 0.U || rs2 === 0.U) { badCapture := true.B }
      }
      is(LOOP_CONV_WS_CONFIG_6) {
        shape.input := rs2
        // A nonzero bias pointer also requests native zero initialization with no_bias.
        when(rs1 === 0.U || rs2 === 0.U) { badCapture := true.B }
      }
      is(LOOP_CONV_WS) {
        shape.aHalf := rs1(19, 18)
        shape.bHalf := rs1(17, 16)
        when(rs1(6, 1) =/= 0.U || rs2(2, 0) =/= 1.U ||
            rs1(19, 18) === 0.U || rs1(19, 18) > 2.U ||
            rs1(17, 16) === 0.U || rs1(17, 16) > 2.U) {
          badCapture := true.B
        }
      }
    }
  }
  val shapeValid = shape.rows =/= 0.U && shape.columns =/= 0.U &&
    shape.inputs =/= 0.U && shape.inputs < 32768.U && shape.outputs =/= 0.U &&
    shape.channels === shape.outputs && shape.kernelChannels === shape.inputs &&
    shape.kernelRows === shape.kernel && shape.kernelColumns === shape.kernel &&
    shape.kernel =/= 0.U && shape.stride =/= 0.U && shape.dilation =/= 0.U &&
    shape.inputStride >= shape.inputs && shape.outputStride === shape.outputs &&
    shape.executeConfigured && shape.storeConfigured &&
    shape.storeStride === shape.outputs * native.elementBytes.U &&
    shape.left === shape.padding && shape.top === shape.padding
  io.captureValid := !captureEnabled || (captured.andR && !badCapture && shapeValid)

  val current = selected(shapes, io.request.job)
  val bind = Wire(new GemminiConvBinding)
  val tile = io.request.tile
  val rows = tile.rows.pad(16)(15, 0)
  val columns = tile.columns.pad(16)(15, 0)
  val y = (tile.row * current.stride).zext - current.padding.zext
  val x = (tile.column * current.stride).zext - current.padding.zext
  val extent = (current.kernel - 1.U) * current.dilation
  val spanRows = rows * current.stride +& extent
  val spanColumns = columns * current.stride +& extent
  val top = Mux(y < 0.S, -y, 0.S).asUInt
  val left = Mux(x < 0.S, -x, 0.S).asUInt
  val bottom = Mux(y + spanRows.zext > current.rows.zext,
    y + spanRows.zext - current.rows.zext, 0.S).asUInt
  val right = Mux(x + spanColumns.zext > current.columns.zext,
    x + spanColumns.zext - current.columns.zext, 0.S).asUInt
  val sourceRow = Mux(y < 0.S, 0.S, y).asUInt
  val sourceColumn = Mux(x < 0.S, 0.S, x).asUInt
  val needsView = VecInit((0 until params.jobCount).map { job =>
    params.auto.dependencies.exists { dependency =>
      val stage = params.auto.stage(dependency.destination)
      stage.endpoint == "gemmini" && stage.job == job && dependency.copy.nonEmpty
    }.B
  })
  val source = view.sourceTile
  val inputBase = Mux(viewValid, view.sourceAddress, current.input)
  val inputRows = Mux(viewValid, source.rows, current.rows)
  val inputColumns = Mux(viewValid, source.columns, current.columns)
  val inputStride = Mux(viewValid, current.inputs, current.inputStride)
  val localRow = sourceRow - Mux(viewValid, source.row, 0.U)
  val localColumn = sourceColumn - Mux(viewValid, source.column, 0.U)
  val inputOffset = (localRow * inputColumns +& localColumn) * inputStride
  val inputAddress = inputBase +& inputOffset
  val inputBytes = inputRows * inputColumns * inputStride
  val inputEnd = inputBase +& inputBytes
  val viewMatches = view.job === io.request.job && view.tile.asUInt === tile.asUInt &&
    source.id === tile.id && source.rows =/= 0.U && source.rows <= 65535.U &&
    source.columns =/= 0.U && source.columns <= 65535.U &&
    (source.row +& source.rows) <= current.rows &&
    (source.column +& source.columns) <= current.columns &&
    source.row <= sourceRow && source.column <= sourceColumn &&
    (source.row +& source.rows) >= sourceRow +& (spanRows - top - bottom) &&
    (source.column +& source.columns) >= sourceColumn +& (spanColumns - left - right) &&
    view.bytes === inputBytes && inputBase =/= 0.U
  val viewReady = Mux(viewValid, viewMatches, !selected(needsView, io.request.job))
  val bytes = rows * columns * current.outputs
  val outputEnd = io.watch.address +& bytes
  val aRows = ((current.inputs +& (native.dim - 1).U) >> log2Ceil(native.dim)) * spanRows * spanColumns
  val bRows = ((current.outputs +& (native.dim - 1).U) >> log2Ceil(native.dim)) *
    current.kernelRows * current.kernelColumns * current.inputs
  val cRows = ((current.outputs +& (native.dim - 1).U) >> log2Ceil(native.dim)) * rows * columns
  val halfRows = native.spmRows / 2
  val rowBytes = native.dim * native.elementBytes
  val aStart = native.spmBase.U + (current.aHalf - 1.U) * (halfRows * rowBytes).U
  val aEnd = aStart +& aRows * rowBytes.U
  val bEnd = native.spmBase.U + current.bHalf * (halfRows * rowBytes).U
  val bStart = bEnd - bRows * rowBytes.U
  def disjoint(start: UInt, end: UInt, otherStart: UInt, otherEnd: UInt): Bool =
    end <= otherStart || start >= otherEnd
  val inputSafe = disjoint(inputBase, inputEnd, aStart, aEnd) &&
    disjoint(inputBase, inputEnd, bStart, bEnd)
  val outputSafe = io.watch.address >= native.spmBase.U &&
    outputEnd <= (native.spmBase + native.spmBytes).U &&
    disjoint(io.watch.address, outputEnd, aStart, aEnd) &&
    disjoint(io.watch.address, outputEnd, bStart, bEnd) &&
    disjoint(io.watch.address, outputEnd, inputBase, inputEnd)
  val boundsValid = tile.rows =/= 0.U && tile.rows <= 65535.U &&
    tile.columns =/= 0.U && tile.columns <= 65535.U &&
    y + ((rows - 1.U) * current.stride).zext + extent.zext < (current.rows +& current.padding).zext &&
    x + ((columns - 1.U) * current.stride).zext + extent.zext < (current.columns +& current.padding).zext &&
    sourceRow < current.rows && sourceColumn < current.columns &&
    spanRows > top +& bottom && spanColumns > left +& right &&
    spanRows < 32768.U && spanColumns < 32768.U && bottom <= 255.U && right <= 65535.U
  val storageValid = aRows <= halfRows.U && bRows <= halfRows.U &&
    disjoint(aStart, aEnd, bStart, bEnd) && cRows <= (native.accRows / 2).U &&
    inputBytes <= "hffffffff".U && inputEnd <= (BigInt(1) << native.addressBits).U &&
    inputAddress < (BigInt(1) << native.addressBits).U &&
    inputSafe && outputSafe
  val watchMatches = io.watchValid && io.watch.job === io.request.job &&
    io.watch.tile.asUInt === tile.asUInt && io.watch.bytes === bytes && bytes <= params.publicationBytes.U
  io.requestValid := !selected(enabled, io.request.job) ||
    (boundsValid && storageValid && watchMatches && viewReady)
  bind.rows := rows
  bind.columns := columns
  bind.left := left
  bind.right := right
  bind.top := top
  bind.bottom := bottom
  bind.input := inputAddress
  bind.output := io.watch.address
  bind.inputRows := inputRows
  bind.inputColumns := inputColumns
  bind.inputStride := inputStride

  val active = RegInit(false.B)
  val binding = Reg(new GemminiConvBinding)
  when(io.start) {
    active := selected(enabled, io.request.job)
    binding := bind
  }
  io.patched := io.command
  when(active) {
    val command = io.command
    switch(command.inst.funct) {
      is(LOOP_CONV_WS_CONFIG_1) {
        io.patched.rs1 := Cat(command.rs1(63, 32), binding.inputRows, command.rs1(15, 0))
        io.patched.rs2 := Cat(command.rs2(63, 48), binding.columns, binding.rows, binding.rows)
      }
      is(LOOP_CONV_WS_CONFIG_2) {
        io.patched.rs1 := Cat(command.rs1(63, 48), binding.columns, command.rs1(31, 0))
        io.patched.rs2 := Cat(command.rs2(63, 48), binding.rows, binding.columns, command.rs2(15, 0))
      }
      is(LOOP_CONV_WS_CONFIG_3) {
        io.patched.rs1 := Cat(command.rs1(63, 16), binding.left)
        io.patched.rs2 := Cat(binding.right, binding.top, binding.bottom, command.rs2(23, 16), binding.inputColumns)
      }
      is(LOOP_CONV_WS_CONFIG_4) {
        io.patched.rs1 := Cat(binding.rows, command.rs1(47, 0))
        io.patched.rs2 := Cat(binding.inputStride, command.rs2(47, 16), binding.columns)
      }
      is(LOOP_CONV_WS_CONFIG_5) { io.patched.rs2 := binding.output }
      is(LOOP_CONV_WS_CONFIG_6) { io.patched.rs2 := binding.input }
    }
  }
}
