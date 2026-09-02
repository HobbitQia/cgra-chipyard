package chipyard.socgen.pool

import chisel3._
import chisel3.util._

class PoolReducer(params: PoolParams, beatBits: Int) extends Module {
  private val laneCount = beatBits / params.elementBits
  require(beatBits % params.elementBits == 0)

  val io = IO(new Bundle {
    val mode = Input(UInt(PoolMode.Width.W))
    val supported = Output(Bool())
    val input = Flipped(Decoupled(new PoolChunk(params, beatBits)))
    val output = Decoupled(new PoolOutput(params, beatBits))
  })

  val maxima = Reg(Vec(laneCount, SInt(params.elementBits.W)))
  val outputData = Reg(UInt(beatBits.W))
  val outputLanes = Reg(UInt(laneCount.W))
  val outputValid = RegInit(false.B)
  val inputValues = io.input.bits.data.asTypeOf(Vec(laneCount, SInt(params.elementBits.W)))
  val nextMaxima = Wire(Vec(laneCount, SInt(params.elementBits.W)))
  val minimum = (BigInt(1) << (params.elementBits - 1)).U.asSInt

  for (lane <- 0 until laneCount) {
    val prior = Mux(io.input.bits.first, minimum, maxima(lane))
    nextMaxima(lane) := Mux(
      io.input.bits.sampleValid && inputValues(lane) > prior,
      inputValues(lane),
      prior)
  }

  io.supported := io.mode === PoolMode.Max
  io.input.ready := !outputValid
  io.output.valid := outputValid
  io.output.bits.data := outputData
  io.output.bits.lanes := outputLanes

  when(io.input.fire) {
    maxima := nextMaxima
    when(io.input.bits.last) {
      outputData := nextMaxima.asUInt
      outputLanes := io.input.bits.lanes
      outputValid := true.B
    }
  }
  when(io.output.fire) {
    outputValid := false.B
  }
}
