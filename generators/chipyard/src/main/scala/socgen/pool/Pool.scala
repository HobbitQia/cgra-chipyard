package chipyard.socgen.pool

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.LazyModule

case class PoolParams(elementBits: Int = 32, fifoDepth: Int = 2) {
  require(Seq(8, 16, 32).contains(elementBits))
  require(fifoDepth > 0)

  val elementBytes: Int = elementBits / 8
  val addressBits: Int = 64
  val dimensionBits: Int = 16
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
    sourceId = IdRange(0, 1),
    requestFifo = true)))))

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
  val laneCount = beatBits / params.elementBits
  val laneCountWidth = log2Ceil(laneCount + 1)
  val beatShift = log2Ceil(beatBytes)
  val elementShift = log2Ceil(params.elementBytes)
  require(beatBits % params.elementBits == 0)

  object State {
    val idle :: prepare :: readRequest :: readResponse :: readSecondRequest :: readSecondResponse :: enqueueInput :: waitOutput :: writeRequest :: writeResponse :: writeSecondRequest :: writeSecondResponse :: finish :: Nil = Enum(13)
  }

  val state = RegInit(State.idle)
  val job = Reg(new PoolJob(params))
  val outputHeight = Reg(UInt(params.dimensionBits.W))
  val outputWidth = Reg(UInt(params.dimensionBits.W))
  val outputRow = RegInit(0.U(params.dimensionBits.W))
  val outputColumn = RegInit(0.U(params.dimensionBits.W))
  val channel = RegInit(0.U(params.dimensionBits.W))
  val kernelRow = RegInit(0.U(params.dimensionBits.W))
  val kernelColumn = RegInit(0.U(params.dimensionBits.W))
  val readAddress = Reg(UInt(params.addressBits.W))
  val readOffset = Reg(UInt(beatShift.W))
  val readCrosses = RegInit(false.B)
  val firstRead = Reg(UInt(beatBits.W))
  val chunkData = Reg(UInt(beatBits.W))
  val chunkLanes = Reg(UInt(laneCount.W))
  val chunkSampleValid = RegInit(false.B)
  val chunkFirst = RegInit(false.B)
  val chunkLast = RegInit(false.B)
  val writeAddress = Reg(UInt(params.addressBits.W))
  val writeData = Reg(UInt(beatBits.W))
  val writeMask = Reg(UInt(beatBytes.W))
  val writeCrosses = RegInit(false.B)
  val secondWriteData = Reg(UInt(beatBits.W))
  val secondWriteMask = Reg(UInt(beatBytes.W))
  val inputEventValid = RegInit(false.B)
  val inputStatus = RegInit(PoolStatus.Success)
  val doneEventValid = RegInit(false.B)
  val doneStatus = RegInit(PoolStatus.Success)
  val jobFailed = RegInit(false.B)
  val inputComplete = RegInit(false.B)

  val inputQueue = Module(new Queue(new PoolChunk(params, beatBits), params.fifoDepth))
  val outputQueue = Module(new Queue(new PoolOutput(params, beatBits), params.fifoDepth))
  val reducer = Module(new PoolReducer(params, beatBits))
  reducer.io.input <> inputQueue.io.deq
  outputQueue.io.enq <> reducer.io.output
  reducer.io.mode := Mux(state === State.idle, io.job.bits.mode, job.mode)

  val paddedHeight = io.job.bits.inputHeight + (io.job.bits.padHeight << 1)
  val paddedWidth = io.job.bits.inputWidth + (io.job.bits.padWidth << 1)
  val validShape = io.job.bits.inputHeight =/= 0.U &&
    io.job.bits.inputWidth =/= 0.U && io.job.bits.channels =/= 0.U &&
    io.job.bits.kernelHeight =/= 0.U && io.job.bits.kernelWidth =/= 0.U &&
    io.job.bits.strideHeight =/= 0.U && io.job.bits.strideWidth =/= 0.U &&
    paddedHeight >= io.job.bits.kernelHeight && paddedWidth >= io.job.bits.kernelWidth &&
    (io.job.bits.source & (params.elementBytes - 1).U) === 0.U &&
    (io.job.bits.destination & (params.elementBytes - 1).U) === 0.U

  io.job.ready := state === State.idle && !inputEventValid && !doneEventValid
  io.inputDone.valid := inputEventValid
  io.inputDone.bits.status := inputStatus
  io.done.valid := doneEventValid
  io.done.bits.status := doneStatus
  io.busy := state =/= State.idle || inputEventValid || doneEventValid

  inputQueue.io.enq.valid := state === State.enqueueInput
  inputQueue.io.enq.bits.data := chunkData
  inputQueue.io.enq.bits.lanes := chunkLanes
  inputQueue.io.enq.bits.sampleValid := chunkSampleValid
  inputQueue.io.enq.bits.first := chunkFirst
  inputQueue.io.enq.bits.last := chunkLast
  outputQueue.io.deq.ready := state === State.waitOutput

  val paddedRow = outputRow * job.strideHeight + kernelRow
  val paddedColumn = outputColumn * job.strideWidth + kernelColumn
  val rowPadding = paddedRow < job.padHeight || paddedRow >= job.padHeight + job.inputHeight
  val columnPadding = paddedColumn < job.padWidth || paddedColumn >= job.padWidth + job.inputWidth
  val inputRow = paddedRow - job.padHeight
  val inputColumn = paddedColumn - job.padWidth
  val inputIndex = (inputRow * job.inputWidth + inputColumn) * job.channels + channel
  val inputAddress = job.source + (inputIndex << elementShift)
  val remainingChannels = job.channels - channel
  val validLaneCount = Mux(
    remainingChannels < laneCount.U,
    remainingChannels(laneCountWidth - 1, 0),
    laneCount.U(laneCountWidth.W))
  val laneMask = ((1.U((laneCount + 1).W) << validLaneCount) - 1.U)(laneCount - 1, 0)
  val validByteCount = validLaneCount << elementShift
  val addressOffset = inputAddress(beatShift - 1, 0)
  val alignedInputAddress = inputAddress & (~(beatBytes - 1).U(params.addressBits.W))
  val crossesBeat = addressOffset + validByteCount > beatBytes.U
  val firstKernel = kernelRow === 0.U && kernelColumn === 0.U
  val lastKernel = kernelRow + 1.U === job.kernelHeight &&
    kernelColumn + 1.U === job.kernelWidth
  val lastInput = outputRow + 1.U === outputHeight &&
    outputColumn + 1.U === outputWidth && channel + laneCount.U >= job.channels && lastKernel
  val outputIndex = (outputRow * outputWidth + outputColumn) * job.channels + channel
  val outputAddress = job.destination + (outputIndex << elementShift)
  val outputOffset = outputAddress(beatShift - 1, 0)
  val alignedOutputAddress = outputAddress & (~(beatBytes - 1).U(params.addressBits.W))
  val outputByteCount = PopCount(outputQueue.io.deq.bits.lanes) << elementShift
  val outputCrosses = outputOffset + outputByteCount > beatBytes.U
  val outputByteMask = ((1.U((beatBytes + 1).W) << outputByteCount) - 1.U)(beatBytes - 1, 0)
  val shiftedOutputMask = (outputByteMask << outputOffset)(beatBytes - 1, 0)
  val shiftedOutputData = (outputQueue.io.deq.bits.data << (outputOffset << 3))(beatBits - 1, 0)
  val secondMask = outputByteMask >> (beatBytes.U - outputOffset)
  val secondData = outputQueue.io.deq.bits.data >> ((beatBytes.U - outputOffset) << 3)

  def fail(status: UInt): Unit = {
    when(!inputComplete) {
      inputStatus := status
      inputEventValid := true.B
    }
    doneStatus := status
    doneEventValid := true.B
    jobFailed := true.B
    state := State.finish
  }

  when(io.inputDone.fire) {
    inputEventValid := false.B
  }
  when(io.done.fire) {
    doneEventValid := false.B
    inputComplete := false.B
  }

  when(io.job.fire) {
    job := io.job.bits
    outputRow := 0.U
    outputColumn := 0.U
    channel := 0.U
    kernelRow := 0.U
    kernelColumn := 0.U
    jobFailed := false.B
    inputComplete := false.B
    inputStatus := PoolStatus.Success
    doneStatus := PoolStatus.Success
    when(!reducer.io.supported) {
      fail(PoolStatus.UnsupportedMode)
    }.elsewhen(validShape) {
      outputHeight := (paddedHeight - io.job.bits.kernelHeight) / io.job.bits.strideHeight + 1.U
      outputWidth := (paddedWidth - io.job.bits.kernelWidth) / io.job.bits.strideWidth + 1.U
      state := State.prepare
    }.otherwise {
      fail(PoolStatus.BadJob)
    }
  }

  when(state === State.prepare) {
    chunkLanes := laneMask
    chunkSampleValid := !(rowPadding || columnPadding)
    chunkFirst := firstKernel
    chunkLast := lastKernel
    when(rowPadding || columnPadding) {
      chunkData := 0.U
      state := State.enqueueInput
    }.otherwise {
      readAddress := alignedInputAddress
      readOffset := addressOffset
      readCrosses := crossesBeat
      state := State.readRequest
    }
  }

  val currentReadAddress = Mux(state === State.readSecondRequest, readAddress + beatBytes.U, readAddress)
  val (getLegal, get) = edge.Get(0.U, currentReadAddress, beatShift.U)
  val currentWriteAddress = Mux(state === State.writeSecondRequest, writeAddress + beatBytes.U, writeAddress)
  val currentWriteData = Mux(state === State.writeSecondRequest, secondWriteData, writeData)
  val currentWriteMask = Mux(state === State.writeSecondRequest, secondWriteMask, writeMask)
  val (putLegal, put) = edge.Put(0.U, currentWriteAddress, beatShift.U, currentWriteData, currentWriteMask)

  tl.a.valid := state === State.readRequest || state === State.readSecondRequest ||
    state === State.writeRequest || state === State.writeSecondRequest
  tl.a.bits := Mux(
    state === State.readRequest || state === State.readSecondRequest,
    get,
    put)
  when(tl.a.valid) {
    assert(Mux(
      state === State.readRequest || state === State.readSecondRequest,
      getLegal,
      putLegal))
  }
  when(tl.a.fire) {
    state := MuxLookup(state, state)(Seq(
      State.readRequest -> State.readResponse,
      State.readSecondRequest -> State.readSecondResponse,
      State.writeRequest -> State.writeResponse,
      State.writeSecondRequest -> State.writeSecondResponse))
  }

  tl.d.ready := state === State.readResponse || state === State.readSecondResponse ||
    state === State.writeResponse || state === State.writeSecondResponse
  when(tl.d.fire) {
    val readResponse = state === State.readResponse || state === State.readSecondResponse
    val expectedOpcode = Mux(readResponse, TLMessages.AccessAckData, TLMessages.AccessAck)
    assert(tl.d.bits.source === 0.U)
    assert(tl.d.bits.opcode === expectedOpcode)
    assert(edge.done(tl.d))
    when(tl.d.bits.denied) {
      fail(PoolStatus.Denied)
    }.elsewhen(tl.d.bits.corrupt) {
      fail(PoolStatus.Corrupt)
    }.elsewhen(state === State.readResponse) {
      firstRead := tl.d.bits.data
      when(readCrosses) {
        state := State.readSecondRequest
      }.otherwise {
        chunkData := (tl.d.bits.data >> (readOffset << 3))(beatBits - 1, 0)
        state := State.enqueueInput
      }
    }.elsewhen(state === State.readSecondResponse) {
      chunkData := (Cat(tl.d.bits.data, firstRead) >> (readOffset << 3))(beatBits - 1, 0)
      state := State.enqueueInput
    }.elsewhen(state === State.writeResponse) {
      state := Mux(writeCrosses, State.writeSecondRequest, State.finish)
    }.otherwise {
      state := State.finish
    }
  }

  when(inputQueue.io.enq.fire) {
    when(lastInput) {
      inputComplete := true.B
      inputEventValid := true.B
    }
    when(lastKernel) {
      state := State.waitOutput
    }.elsewhen(kernelColumn + 1.U === job.kernelWidth) {
      kernelColumn := 0.U
      kernelRow := kernelRow + 1.U
      state := State.prepare
    }.otherwise {
      kernelColumn := kernelColumn + 1.U
      state := State.prepare
    }
  }

  when(outputQueue.io.deq.fire) {
    writeAddress := alignedOutputAddress
    writeData := shiftedOutputData
    writeMask := shiftedOutputMask
    writeCrosses := outputCrosses
    secondWriteData := secondData
    secondWriteMask := secondMask
    state := State.writeRequest
  }

  when(state === State.finish && jobFailed) {
    when(!inputEventValid && !doneEventValid) {
      jobFailed := false.B
      state := State.idle
    }
  }.elsewhen(state === State.finish && !doneEventValid) {
    val lastChannel = channel + laneCount.U >= job.channels
    val lastColumn = outputColumn + 1.U === outputWidth
    val lastRow = outputRow + 1.U === outputHeight
    when(doneStatus =/= PoolStatus.Success || (lastChannel && lastColumn && lastRow)) {
      doneEventValid := true.B
      state := State.idle
    }.otherwise {
      kernelRow := 0.U
      kernelColumn := 0.U
      when(!lastChannel) {
        channel := channel + laneCount.U
      }.elsewhen(!lastColumn) {
        channel := 0.U
        outputColumn := outputColumn + 1.U
      }.otherwise {
        channel := 0.U
        outputColumn := 0.U
        outputRow := outputRow + 1.U
      }
      state := State.prepare
    }
  }

  tl.b.ready := true.B
  tl.c.valid := false.B
  tl.c.bits := DontCare
  tl.e.valid := false.B
  tl.e.bits := DontCare
}
