package chipyard.socgen.pool

import chisel3._
import chisel3.util._

class PoolReducer(params: PoolParams, beatBits: Int) extends Module {
  private val laneCount = beatBits / params.elementBits
  private val countBits = 2 * params.dimensionBits
  private val sumBits = params.elementBits + countBits
  require(beatBits % params.elementBits == 0)

  val io = IO(new Bundle {
    val clear = Input(Bool())
    val mode = Input(UInt(PoolMode.Width.W))
    val supported = Output(Bool())
    val input = Flipped(Decoupled(new PoolChunk(params, beatBits)))
    val output = Decoupled(new PoolOutput(params, beatBits))
  })

  object State {
    val idle :: load :: divide :: Nil = Enum(3)
  }

  val state = RegInit(State.idle)
  val maxima = Reg(Vec(laneCount, SInt(params.elementBits.W)))
  val sums = Reg(Vec(laneCount, SInt(sumBits.W)))
  val count = Reg(UInt(countBits.W))
  val lane = Reg(UInt(math.max(1, log2Ceil(laneCount)).W))
  val bit = Reg(UInt(log2Ceil(params.elementBits).W))
  val remainder = Reg(UInt(countBits.W))
  val quotient = Reg(UInt(params.elementBits.W))
  val negative = Reg(Bool())
  val outputData = Reg(Vec(laneCount, SInt(params.elementBits.W)))
  val outputLanes = Reg(UInt(laneCount.W))
  val outputValid = RegInit(false.B)
  val inputValues = io.input.bits.data.asTypeOf(Vec(laneCount, SInt(params.elementBits.W)))
  val nextMaxima = Wire(Vec(laneCount, SInt(params.elementBits.W)))
  val nextSums = Wire(Vec(laneCount, SInt(sumBits.W)))
  val nextCount = Mux(io.input.bits.first, 0.U, count) + io.input.bits.sampleValid
  val minimum = (BigInt(1) << (params.elementBits - 1)).U.asSInt

  for (lane <- 0 until laneCount) {
    val prior = Mux(io.input.bits.first, minimum, maxima(lane))
    nextMaxima(lane) := Mux(
      io.input.bits.sampleValid && inputValues(lane) > prior,
      inputValues(lane),
      prior)
    nextSums(lane) := Mux(io.input.bits.first, 0.S, sums(lane)) +
      Mux(io.input.bits.sampleValid, inputValues(lane), 0.S)
  }

  io.supported := io.mode === PoolMode.Max || io.mode === PoolMode.Average
  io.input.ready := state === State.idle && !outputValid
  io.output.valid := outputValid
  io.output.bits.data := outputData.asUInt
  io.output.bits.lanes := outputLanes

  when(io.input.fire) {
    when(io.mode === PoolMode.Average) {
      sums := nextSums
      count := nextCount
    }.otherwise {
      maxima := nextMaxima
    }
    when(io.input.bits.last) {
      outputLanes := io.input.bits.lanes
      when(io.mode === PoolMode.Average) {
        lane := 0.U
        state := State.load
      }.otherwise {
        outputData := nextMaxima
        outputValid := true.B
      }
    }
  }

  when(state === State.load) {
    val sum = sums(lane)
    val magnitude = Mux(sum < 0.S, (-sum).asUInt, sum.asUInt)
    negative := sum < 0.S
    // The quotient fits one element; preload the high dividend bits as the remainder.
    remainder := magnitude(sumBits - 1, params.elementBits)
    quotient := magnitude(params.elementBits - 1, 0)
    bit := 0.U
    state := State.divide
  }
  when(state === State.divide) {
    val shifted = Cat(remainder, quotient(params.elementBits - 1))
    val subtract = shifted >= count
    val nextRemainder = Mux(subtract, shifted - count, shifted)
    val nextQuotient = Cat(quotient(params.elementBits - 2, 0), subtract)
    remainder := nextRemainder
    quotient := nextQuotient
    bit := bit + 1.U
    when(bit === (params.elementBits - 1).U) {
      val rounded = nextQuotient + ((nextRemainder << 1) >= count)
      outputData(lane) := Mux(negative, 0.U - rounded, rounded).asSInt
      when(lane === (laneCount - 1).U) {
        outputValid := true.B
        state := State.idle
      }.otherwise {
        lane := lane + 1.U
        state := State.load
      }
    }
  }
  when(io.output.fire) {
    outputValid := false.B
  }
  when(io.clear) {
    outputValid := false.B
    state := State.idle
  }
}
