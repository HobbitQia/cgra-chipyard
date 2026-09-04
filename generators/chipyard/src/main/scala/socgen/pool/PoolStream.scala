package chipyard.socgen.pool

import chisel3._
import chisel3.util._

class PoolQueue[T <: Data](gen: T, entries: Int) extends Module {
  require(entries > 0)
  private val ptrBits = math.max(1, log2Ceil(entries))

  val io = IO(new Bundle {
    val clear = Input(Bool())
    val enq = Flipped(Decoupled(gen))
    val deq = Decoupled(gen)
  })

  val data = Reg(Vec(entries, gen))
  val enqueue = RegInit(0.U(ptrBits.W))
  val dequeue = RegInit(0.U(ptrBits.W))
  val count = RegInit(0.U(log2Ceil(entries + 1).W))

  def next(pointer: UInt): UInt =
    Mux(pointer === (entries - 1).U, 0.U, pointer + 1.U)

  io.enq.ready := count < entries.U
  io.deq.valid := count =/= 0.U
  io.deq.bits := data(dequeue)

  when(io.enq.fire) {
    data(enqueue) := io.enq.bits
    enqueue := next(enqueue)
  }
  when(io.deq.fire) {
    dequeue := next(dequeue)
  }
  switch(Cat(io.enq.fire, io.deq.fire)) {
    is("b10".U) { count := count + 1.U }
    is("b01".U) { count := count - 1.U }
  }
  when(io.clear) {
    enqueue := 0.U
    dequeue := 0.U
    count := 0.U
  }
}

class PoolLineEntry(params: PoolParams, beatBits: Int) extends Bundle {
  val data = UInt(beatBits.W)
  val lanes = UInt((beatBits / params.elementBits).W)
  val last = Bool()
}

class PoolLineBuffer(params: PoolParams, beatBits: Int) extends Module {
  private val indexBits = math.max(1, log2Ceil(params.lineBufferEntries))

  val io = IO(new Bundle {
    val clear = Input(Bool())
    val input = Flipped(Decoupled(new PoolLineEntry(params, beatBits)))
    val release = Flipped(Valid(UInt(params.addressBits.W)))
    val index = Input(UInt(params.addressBits.W))
    val found = Output(Bool())
    val entry = Output(new PoolLineEntry(params, beatBits))
  })

  val data = Reg(Vec(params.lineBufferEntries, new PoolLineEntry(params, beatBits)))
  val base = RegInit(0.U(params.addressBits.W))
  val written = RegInit(0.U(params.addressBits.W))
  val used = Mux(written > base, written - base, 0.U)
  val writeIndex = (written % params.lineBufferEntries.U)(indexBits - 1, 0)
  val readIndex = (io.index % params.lineBufferEntries.U)(indexBits - 1, 0)

  io.input.ready := written < base || used < params.lineBufferEntries.U
  io.found := io.index >= base && io.index < written
  io.entry := data(readIndex)

  when(io.input.fire) {
    when(written >= base) {
      data(writeIndex) := io.input.bits
    }
    written := written + 1.U
  }
  when(io.release.valid) {
    base := io.release.bits
  }
  when(io.clear) {
    base := 0.U
    written := 0.U
  }
}

class PoolUnpacker(params: PoolParams, beatBits: Int) extends Module {
  private val beatBytes = beatBits / 8
  private val laneCount = beatBits / params.elementBits
  private val laneBits = log2Ceil(laneCount + 1)
  private val byteCountBits = log2Ceil(2 * beatBytes + 1)

  val io = IO(new Bundle {
    val start = Input(Bool())
    val offset = Input(UInt(log2Ceil(beatBytes).W))
    val elementCount = Input(UInt(params.addressBits.W))
    val channels = Input(UInt(params.dimensionBits.W))
    val cancel = Input(Bool())
    val input = Flipped(Decoupled(new PoolReadBeat(beatBits)))
    val output = Decoupled(new PoolLineEntry(params, beatBits))
  })

  val active = RegInit(false.B)
  val firstBeat = RegInit(true.B)
  val buffer = RegInit(0.U((2 * beatBits).W))
  val bytes = RegInit(0.U(byteCountBits.W))
  val remaining = Reg(UInt(params.addressBits.W))
  val channel = Reg(UInt(params.dimensionBits.W))
  val channels = Reg(UInt(params.dimensionBits.W))
  val offset = Reg(UInt(log2Ceil(beatBytes).W))

  val channelsLeft = channels - channel
  val laneCountValue = Mux(
    channelsLeft < laneCount.U,
    channelsLeft(laneBits - 1, 0),
    laneCount.U(laneBits.W))
  val laneBytes = laneCountValue << log2Ceil(params.elementBytes)
  val laneMask = ((1.U((laneCount + 1).W) << laneCountValue) - 1.U)(laneCount - 1, 0)
  val canOutput = active && bytes >= laneBytes && laneCountValue =/= 0.U

  io.output.valid := canOutput && !io.cancel
  io.output.bits.data := buffer(beatBits - 1, 0)
  io.output.bits.lanes := laneMask
  io.output.bits.last := remaining === laneCountValue
  io.input.ready := active && !io.cancel && !canOutput && bytes <= beatBytes.U

  when(io.input.fire) {
    val dropBytes = Mux(firstBeat, offset, 0.U)
    val appendBytes = beatBytes.U - dropBytes
    val appendData = io.input.bits.data >> (dropBytes << 3)
    buffer := buffer | (appendData << (bytes << 3))
    bytes := bytes + appendBytes
    firstBeat := false.B
  }
  when(io.output.fire) {
    buffer := buffer >> (laneBytes << 3)
    bytes := bytes - laneBytes
    remaining := remaining - laneCountValue
    when(channel + laneCountValue === channels) {
      channel := 0.U
    }.otherwise {
      channel := channel + laneCountValue
    }
    when(remaining === laneCountValue) {
      active := false.B
    }
  }
  when(io.cancel) {
    active := false.B
    buffer := 0.U
    bytes := 0.U
  }
  when(io.start) {
    active := true.B
    firstBeat := true.B
    buffer := 0.U
    bytes := 0.U
    remaining := io.elementCount
    channel := 0.U
    channels := io.channels
    offset := io.offset
  }
}

class PoolWriteSplitter(params: PoolParams, beatBits: Int) extends Module {
  private val beatBytes = beatBits / 8
  private val beatShift = log2Ceil(beatBytes)
  private val elementShift = log2Ceil(params.elementBytes)

  val io = IO(new Bundle {
    val start = Input(Bool())
    val destination = Input(UInt(params.addressBits.W))
    val elementCount = Input(UInt(params.addressBits.W))
    val cancel = Input(Bool())
    val input = Flipped(Decoupled(new PoolOutput(params, beatBits)))
    val output = Decoupled(new PoolWriteBeat(params, beatBits))
  })

  val active = RegInit(false.B)
  val address = Reg(UInt(params.addressBits.W))
  val remaining = Reg(UInt(params.addressBits.W))
  val beatValid = RegInit(false.B)
  val second = RegInit(false.B)
  val crosses = RegInit(false.B)
  val logicalLast = RegInit(false.B)
  val firstAddress = Reg(UInt(params.addressBits.W))
  val firstData = Reg(UInt(beatBits.W))
  val firstMask = Reg(UInt(beatBytes.W))
  val secondData = Reg(UInt(beatBits.W))
  val secondMask = Reg(UInt(beatBytes.W))

  io.input.ready := active && !beatValid && !io.cancel
  io.output.valid := beatValid && !io.cancel
  io.output.bits.address := Mux(second, firstAddress + beatBytes.U, firstAddress)
  io.output.bits.data := Mux(second, secondData, firstData)
  io.output.bits.mask := Mux(second, secondMask, firstMask)
  io.output.bits.last := logicalLast && (second || !crosses)

  when(io.input.fire) {
    val laneCount = PopCount(io.input.bits.lanes)
    val byteCount = laneCount << elementShift
    val addressOffset = address(beatShift - 1, 0)
    val alignedAddress = address & (~(beatBytes - 1).U(params.addressBits.W))
    val byteMask = ((1.U((beatBytes + 1).W) << byteCount) - 1.U)(beatBytes - 1, 0)
    val crossesBeat = addressOffset + byteCount > beatBytes.U

    firstAddress := alignedAddress
    firstData := (io.input.bits.data << (addressOffset << 3))(beatBits - 1, 0)
    firstMask := (byteMask << addressOffset)(beatBytes - 1, 0)
    secondData := io.input.bits.data >> ((beatBytes.U - addressOffset) << 3)
    secondMask := byteMask >> (beatBytes.U - addressOffset)
    crosses := crossesBeat
    logicalLast := remaining === laneCount
    beatValid := true.B
    second := false.B
    address := address + byteCount
    remaining := remaining - laneCount
  }
  when(io.output.fire) {
    when(!second && crosses) {
      second := true.B
    }.otherwise {
      beatValid := false.B
      second := false.B
      when(logicalLast) {
        active := false.B
      }
    }
  }
  when(io.cancel) {
    active := false.B
    beatValid := false.B
  }
  when(io.start) {
    active := true.B
    address := io.destination
    remaining := io.elementCount
    beatValid := false.B
    second := false.B
  }
}
