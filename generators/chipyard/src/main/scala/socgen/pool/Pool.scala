package chipyard.socgen.pool

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.LazyModule

case class PoolParams(
  elementBits: Int = 32,
  fifoDepth: Int = 2,
  lineBufferEntries: Int = 8,
  maxInflight: Int = 4) {
  require(Seq(8, 16, 32).contains(elementBits))
  require(fifoDepth > 0)
  require(lineBufferEntries > 0)
  require(maxInflight > 0)

  val elementBytes: Int = elementBits / 8
  val addressBits: Int = 64
  val dimensionBits: Int = 16
  val sourceBits: Int = math.max(1, log2Ceil(2 * maxInflight))
}

object PoolCommand {
  val Source = 0
  val Destination = 1
  val Shape = 2
  val Channels = 3
  val Kernel = 4
  val Stride = 5
  val Padding = 6
  val Start = 7
  val Wait = 8
  val Mode = 9
}

object PoolMode {
  val Width = 2
  val Max = 0.U(Width.W)
  val Average = 1.U(Width.W)
}

object PoolStatus {
  val Width = 8
  val Success = 0.U(Width.W)
  val BadJob = 1.U(Width.W)
  val Denied = 2.U(Width.W)
  val Corrupt = 3.U(Width.W)
  val BadLength = 4.U(Width.W)
  val UnsupportedMode = 5.U(Width.W)
  val BadAddress = 6.U(Width.W)
}

class PoolJob(params: PoolParams) extends Bundle {
  val mode = UInt(PoolMode.Width.W)
  val source = UInt(params.addressBits.W)
  val destination = UInt(params.addressBits.W)
  val inputHeight = UInt(params.dimensionBits.W)
  val inputWidth = UInt(params.dimensionBits.W)
  val channels = UInt(params.dimensionBits.W)
  val kernelHeight = UInt(params.dimensionBits.W)
  val kernelWidth = UInt(params.dimensionBits.W)
  val strideHeight = UInt(params.dimensionBits.W)
  val strideWidth = UInt(params.dimensionBits.W)
  val padHeight = UInt(params.dimensionBits.W)
  val padWidth = UInt(params.dimensionBits.W)
}

class PoolEvent extends Bundle {
  val status = UInt(PoolStatus.Width.W)
}

class PoolChunk(params: PoolParams, beatBits: Int) extends Bundle {
  val data = UInt(beatBits.W)
  val lanes = UInt((beatBits / params.elementBits).W)
  val sampleValid = Bool()
  val first = Bool()
  val last = Bool()
}

class PoolOutput(params: PoolParams, beatBits: Int) extends Bundle {
  val data = UInt(beatBits.W)
  val lanes = UInt((beatBits / params.elementBits).W)
}

class PoolEngine(params: PoolParams)(implicit p: Parameters) extends LazyModule {
  val node = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name = "pool-dma",
    sourceId = IdRange(0, 2 * params.maxInflight),
    requestFifo = false)))))

  override lazy val module = new PoolEngineImp(this, params)
}

class PoolEngineImp(outer: PoolEngine, params: PoolParams)(implicit p: Parameters)
    extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val job = Flipped(Decoupled(new PoolJob(params)))
    val inputDone = Decoupled(new PoolEvent)
    val done = Decoupled(new PoolEvent)
    val busy = Output(Bool())
  })

  val (tl, edge) = outer.node.out(0)
  val beatBits = tl.a.bits.data.getWidth
  val beatBytes = beatBits / 8
  val beatShift = log2Ceil(beatBytes)
  val laneCount = beatBits / params.elementBits
  val laneBits = log2Ceil(laneCount + 1)
  require(beatBits % params.elementBits == 0)

  object State {
    val idle :: prepare :: feed :: waitOutput :: waitWrites :: drain :: Nil = Enum(6)
  }

  val state = RegInit(State.idle)
  val job = Reg(new PoolJob(params))
  val outputHeight = Reg(UInt(params.dimensionBits.W))
  val outputWidth = Reg(UInt(params.dimensionBits.W))
  val channelGroups = Reg(UInt(params.dimensionBits.W))
  val inputEntries = Reg(UInt(params.addressBits.W))
  val outputRow = RegInit(0.U(params.dimensionBits.W))
  val outputColumn = RegInit(0.U(params.dimensionBits.W))
  val group = RegInit(0.U(params.dimensionBits.W))
  val kernelRow = RegInit(0.U(params.dimensionBits.W))
  val kernelColumn = RegInit(0.U(params.dimensionBits.W))
  val inputEventValid = RegInit(false.B)
  val inputStatus = RegInit(PoolStatus.Success)
  val inputIssued = RegInit(false.B)
  val doneEventValid = RegInit(false.B)
  val doneStatus = RegInit(PoolStatus.Success)
  val failed = RegInit(false.B)
  val failStatus = RegInit(PoolStatus.Success)
  val writeComplete = RegInit(false.B)

  val readPlan = Module(new PoolReadPlan(params, beatBytes))
  val readRequests = Module(new PoolQueue(new PoolReadRequest(params), params.maxInflight))
  val readDma = Module(new PoolReadDma(params, beatBits))
  val unpacker = Module(new PoolUnpacker(params, beatBits))
  val lineBuffer = Module(new PoolLineBuffer(params, beatBits))
  val inputQueue = Module(new PoolQueue(new PoolChunk(params, beatBits), params.fifoDepth))
  val reducer = Module(new PoolReducer(params, beatBits))
  val outputQueue = Module(new PoolQueue(new PoolOutput(params, beatBits), params.fifoDepth))
  val splitter = Module(new PoolWriteSplitter(params, beatBits))
  val writeDma = Module(new PoolWriteDma(params, beatBits))

  val paddedHeight = io.job.bits.inputHeight + (io.job.bits.padHeight << 1)
  val paddedWidth = io.job.bits.inputWidth + (io.job.bits.padWidth << 1)
  val nextOutputHeight =
    (paddedHeight - io.job.bits.kernelHeight) / io.job.bits.strideHeight + 1.U
  val nextOutputWidth =
    (paddedWidth - io.job.bits.kernelWidth) / io.job.bits.strideWidth + 1.U
  val nextGroups = (io.job.bits.channels + (laneCount - 1).U) / laneCount.U
  val inputElements = io.job.bits.inputHeight * io.job.bits.inputWidth * io.job.bits.channels
  val outputElements = nextOutputHeight * nextOutputWidth * io.job.bits.channels
  val inputBytes = inputElements << log2Ceil(params.elementBytes)
  val outputBytes = outputElements << log2Ceil(params.elementBytes)
  val sourceEnd = io.job.bits.source.pad(params.addressBits + 1) + inputBytes.pad(params.addressBits + 1)
  val destinationEnd =
    io.job.bits.destination.pad(params.addressBits + 1) + outputBytes.pad(params.addressBits + 1)
  val rangesOverlap = io.job.bits.source.pad(params.addressBits + 1) < destinationEnd &&
    io.job.bits.destination.pad(params.addressBits + 1) < sourceEnd
  val lineEntries = ((io.job.bits.kernelHeight - 1.U) * io.job.bits.inputWidth +
    io.job.bits.kernelWidth) * nextGroups
  val shapeValid = io.job.bits.inputHeight =/= 0.U &&
    io.job.bits.inputWidth =/= 0.U && io.job.bits.channels =/= 0.U &&
    io.job.bits.kernelHeight =/= 0.U && io.job.bits.kernelWidth =/= 0.U &&
    io.job.bits.strideHeight =/= 0.U && io.job.bits.strideWidth =/= 0.U &&
    paddedHeight >= io.job.bits.kernelHeight && paddedWidth >= io.job.bits.kernelWidth
  val addressValid =
    (io.job.bits.source & (params.elementBytes - 1).U) === 0.U &&
      (io.job.bits.destination & (params.elementBytes - 1).U) === 0.U
  val validJob = shapeValid && addressValid &&
    lineEntries <= params.lineBufferEntries.U && !rangesOverlap
  val supportedJob = io.job.bits.mode === PoolMode.Max
  val startJob = io.job.fire && validJob && supportedJob

  val inputOffset = io.job.bits.source(beatShift - 1, 0)
  val alignedSource = io.job.bits.source & (~(beatBytes - 1).U(params.addressBits.W))
  val readBytes = inputOffset + inputBytes
  val readBeats = (readBytes + (beatBytes - 1).U) >> beatShift

  io.job.ready := state === State.idle && !inputEventValid && !doneEventValid
  io.inputDone.valid := inputEventValid
  io.inputDone.bits.status := inputStatus
  io.done.valid := doneEventValid
  io.done.bits.status := doneStatus
  io.busy := state =/= State.idle || inputEventValid || doneEventValid

  when(io.inputDone.fire) {
    inputEventValid := false.B
  }
  when(io.done.fire) {
    doneEventValid := false.B
  }

  readPlan.io.start := startJob
  readPlan.io.base := alignedSource
  readPlan.io.beats := readBeats
  readPlan.io.output <> readRequests.io.enq
  readDma.io.start := startJob
  readDma.io.input <> readRequests.io.deq
  unpacker.io.start := startJob
  unpacker.io.offset := inputOffset
  unpacker.io.elementCount := inputElements
  unpacker.io.channels := io.job.bits.channels
  unpacker.io.input <> readDma.io.output
  lineBuffer.io.input <> unpacker.io.output

  reducer.io.mode := job.mode
  reducer.io.input <> inputQueue.io.deq
  outputQueue.io.enq <> reducer.io.output
  splitter.io.start := startJob
  splitter.io.destination := io.job.bits.destination
  splitter.io.elementCount := outputElements
  splitter.io.input.valid := outputQueue.io.deq.valid && state === State.waitOutput
  splitter.io.input.bits := outputQueue.io.deq.bits
  outputQueue.io.deq.ready := splitter.io.input.ready && state === State.waitOutput
  writeDma.io.start := startJob
  writeDma.io.input <> splitter.io.output

  val dmaError = state =/= State.idle &&
    (readDma.io.error.valid || writeDma.io.error.valid)
  val abort = failed || dmaError
  val clear = startJob || abort
  readPlan.io.cancel := abort
  readRequests.io.clear := clear
  readDma.io.cancel := abort
  unpacker.io.cancel := abort
  lineBuffer.io.clear := clear
  inputQueue.io.clear := clear
  reducer.io.clear := clear
  outputQueue.io.clear := clear
  splitter.io.cancel := abort
  writeDma.io.cancel := abort

  val paddedRow = outputRow * job.strideHeight + kernelRow
  val paddedColumn = outputColumn * job.strideWidth + kernelColumn
  val rowPadding = paddedRow < job.padHeight || paddedRow >= job.padHeight + job.inputHeight
  val columnPadding =
    paddedColumn < job.padWidth || paddedColumn >= job.padWidth + job.inputWidth
  val inputRow = paddedRow - job.padHeight
  val inputColumn = paddedColumn - job.padWidth
  val entryIndex = (inputRow * job.inputWidth + inputColumn) * channelGroups + group
  val channel = group * laneCount.U
  val remainingChannels = job.channels - channel
  val validLanes = Mux(
    remainingChannels < laneCount.U,
    remainingChannels(laneBits - 1, 0),
    laneCount.U(laneBits.W))
  val laneMask = ((1.U((laneCount + 1).W) << validLanes) - 1.U)(laneCount - 1, 0)
  val firstKernel = kernelRow === 0.U && kernelColumn === 0.U
  val lastKernel = kernelRow + 1.U === job.kernelHeight &&
    kernelColumn + 1.U === job.kernelWidth

  lineBuffer.io.index := entryIndex
  inputQueue.io.enq.valid := state === State.feed &&
    (rowPadding || columnPadding || lineBuffer.io.found)
  inputQueue.io.enq.bits.data := Mux(rowPadding || columnPadding, 0.U, lineBuffer.io.entry.data)
  inputQueue.io.enq.bits.lanes := laneMask
  inputQueue.io.enq.bits.sampleValid := !(rowPadding || columnPadding)
  inputQueue.io.enq.bits.first := firstKernel
  inputQueue.io.enq.bits.last := lastKernel

  val windowStartRow = outputRow * job.strideHeight
  val windowStartColumn = outputColumn * job.strideWidth
  val windowHasRow = windowStartRow + job.kernelHeight > job.padHeight &&
    windowStartRow < job.padHeight + job.inputHeight
  val windowHasColumn = windowStartColumn + job.kernelWidth > job.padWidth &&
    windowStartColumn < job.padWidth + job.inputWidth
  val firstInputRow = Mux(windowStartRow < job.padHeight, 0.U, windowStartRow - job.padHeight)
  val firstInputColumn =
    Mux(windowStartColumn < job.padWidth, 0.U, windowStartColumn - job.padWidth)
  val finalOutputRow = outputRow + 1.U === outputHeight
  val nextWindowStartRow = (outputRow + 1.U) * job.strideHeight
  val nextInputRow =
    Mux(nextWindowStartRow < job.padHeight, 0.U, nextWindowStartRow - job.padHeight)
  val releaseColumn = Mux(
    finalOutputRow || nextInputRow > firstInputRow,
    firstInputColumn,
    0.U)
  val windowBase = (firstInputRow * job.inputWidth + releaseColumn) * channelGroups
  lineBuffer.io.release.valid := state === State.prepare && windowHasRow && windowHasColumn
  lineBuffer.io.release.bits := windowBase

  val lastGroup = group + 1.U === channelGroups
  val lastColumn = outputColumn + 1.U === outputWidth
  val lastRow = outputRow + 1.U === outputHeight

  when(io.job.fire) {
    inputIssued := false.B
    inputStatus := PoolStatus.Success
    doneStatus := PoolStatus.Success
    failed := false.B
    failStatus := PoolStatus.Success
    writeComplete := false.B
    when(!supportedJob) {
      inputStatus := PoolStatus.UnsupportedMode
      doneStatus := PoolStatus.UnsupportedMode
      inputEventValid := true.B
      doneEventValid := true.B
    }.elsewhen(!validJob) {
      inputStatus := PoolStatus.BadJob
      doneStatus := PoolStatus.BadJob
      inputEventValid := true.B
      doneEventValid := true.B
    }.otherwise {
      job := io.job.bits
      outputHeight := nextOutputHeight
      outputWidth := nextOutputWidth
      channelGroups := nextGroups
      inputEntries := io.job.bits.inputHeight * io.job.bits.inputWidth * nextGroups
      outputRow := 0.U
      outputColumn := 0.U
      group := 0.U
      kernelRow := 0.U
      kernelColumn := 0.U
      state := State.prepare
    }
  }

  when(state === State.prepare) {
    state := State.feed
  }
  when(inputQueue.io.enq.fire) {
    when(lastKernel) {
      state := State.waitOutput
    }.elsewhen(kernelColumn + 1.U === job.kernelWidth) {
      kernelColumn := 0.U
      kernelRow := kernelRow + 1.U
    }.otherwise {
      kernelColumn := kernelColumn + 1.U
    }
  }
  when(splitter.io.input.fire) {
    kernelRow := 0.U
    kernelColumn := 0.U
    when(lastGroup && lastColumn && lastRow) {
      lineBuffer.io.release.valid := true.B
      lineBuffer.io.release.bits := inputEntries
      state := State.waitWrites
    }.elsewhen(!lastGroup) {
      group := group + 1.U
      state := State.feed
    }.elsewhen(!lastColumn) {
      group := 0.U
      outputColumn := outputColumn + 1.U
      state := State.prepare
    }.otherwise {
      group := 0.U
      outputColumn := 0.U
      outputRow := outputRow + 1.U
      state := State.prepare
    }
  }

  val inputFinished = lineBuffer.io.input.fire && lineBuffer.io.input.bits.last
  when(inputFinished) {
    inputIssued := true.B
    inputEventValid := true.B
    inputStatus := PoolStatus.Success
  }
  when(writeDma.io.complete) {
    writeComplete := true.B
  }
  when(state === State.waitWrites &&
    (inputIssued || inputFinished) && (writeComplete || writeDma.io.complete)) {
    doneEventValid := true.B
    doneStatus := PoolStatus.Success
    state := State.idle
  }

  when(dmaError && !failed) {
    failed := true.B
    failStatus := Mux(readDma.io.error.valid,
      readDma.io.error.bits.status,
      writeDma.io.error.bits.status)
    state := State.drain
  }
  when(state === State.drain && readDma.io.idle && writeDma.io.idle) {
    when(!inputIssued) {
      inputEventValid := true.B
      inputStatus := failStatus
      inputIssued := true.B
    }
    doneEventValid := true.B
    doneStatus := failStatus
    failed := false.B
    state := State.idle
  }

  val readSource = readDma.io.request.bits.source
  val writeSource = writeDma.io.request.bits.source + params.maxInflight.U
  val (getLegal, get) = edge.Get(readSource, readDma.io.request.bits.address, beatShift.U)
  val (putLegal, put) = edge.Put(
    writeSource,
    writeDma.io.request.bits.address,
    beatShift.U,
    writeDma.io.request.bits.data,
    writeDma.io.request.bits.mask)
  val a = Module(new RRArbiter(chiselTypeOf(tl.a.bits), 2))

  a.io.in(0).valid := readDma.io.request.valid
  a.io.in(0).bits := get
  readDma.io.request.ready := a.io.in(0).ready
  a.io.in(1).valid := writeDma.io.request.valid
  a.io.in(1).bits := put
  writeDma.io.request.ready := a.io.in(1).ready
  tl.a <> a.io.out

  when(a.io.in(0).fire) {
    assert(getLegal)
  }
  when(a.io.in(1).fire) {
    assert(putLegal)
  }

  val readResponse = tl.d.bits.source < params.maxInflight.U
  readDma.io.response.valid := tl.d.valid && readResponse
  readDma.io.response.bits.source := tl.d.bits.source
  readDma.io.response.bits.data := tl.d.bits.data
  readDma.io.response.bits.denied := tl.d.bits.denied
  readDma.io.response.bits.corrupt := tl.d.bits.corrupt
  writeDma.io.response.valid := tl.d.valid && !readResponse
  writeDma.io.response.bits.source := tl.d.bits.source - params.maxInflight.U
  writeDma.io.response.bits.data := tl.d.bits.data
  writeDma.io.response.bits.denied := tl.d.bits.denied
  writeDma.io.response.bits.corrupt := tl.d.bits.corrupt
  tl.d.ready := Mux(readResponse, readDma.io.response.ready, writeDma.io.response.ready)

  when(tl.d.fire) {
    assert(edge.done(tl.d))
    assert(tl.d.bits.opcode === Mux(
      readResponse,
      TLMessages.AccessAckData,
      TLMessages.AccessAck))
  }

  tl.b.ready := true.B
  tl.c.valid := false.B
  tl.c.bits := DontCare
  tl.e.valid := false.B
  tl.e.bits := DontCare
}
