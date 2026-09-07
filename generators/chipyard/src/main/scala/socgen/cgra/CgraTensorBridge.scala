package chipyard.socgen.cgra

import chisel3._
import chisel3.util._

case class CgraRequantParams(multiplier: Int, shift: Int) {
  require(multiplier > 0)
  require(shift >= 0 && shift < 31)
}

case class CgraTensorBridgeParams(
    outboundSpmWord: Int,
    outboundWords: Int,
    inboundSpmWord: Int,
    inboundWords: Int,
    outboundScale: CgraRequantParams,
    inboundScale: CgraRequantParams) {
  require(outboundSpmWord >= 0 && outboundWords > 0)
  require(inboundSpmWord >= 0 && inboundWords > 0)
}

object CgraRequant {
  def apply(value: SInt, params: CgraRequantParams): SInt = {
    val product = value * params.multiplier.S(32.W)
    val shifted = if (params.shift == 0) {
      product
    } else {
      val negative = product < 0.S
      val magnitude = Mux(negative, (-product).asUInt, product.asUInt)
      val rounded = (magnitude + (BigInt(1) << (params.shift - 1)).U) >> params.shift
      Mux(negative, -rounded.asSInt, rounded.asSInt)
    }
    Mux(shifted > 127.S, 127.S, Mux(shifted < (-128).S, (-128).S, shifted))
  }

  def byte(value: SInt, params: CgraRequantParams): UInt =
    apply(value, params).asUInt(7, 0)
}

class CgraDmaTensorBridge(beatBits: Int, scale: CgraRequantParams) extends Module {
  require(beatBits > 0 && beatBits % 32 == 0)

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(beatBits.W)))
    val out = Decoupled(UInt(beatBits.W))
    val transform = Input(Bool())
  })

  val valid = RegInit(false.B)
  val data = Reg(UInt(beatBits.W))
  val converted = VecInit((0 until beatBits / 32).map { lane =>
    val word = io.in.bits(32 * lane + 31, 32 * lane).asSInt
    val value = CgraRequant.byte(word, scale)
    Cat(Fill(24, value(7)), value)
  }).asUInt

  io.in.ready := !valid || io.out.ready
  io.out.valid := valid
  io.out.bits := data

  when(io.in.fire) {
    data := Mux(io.transform, converted, io.in.bits)
    valid := true.B
  }.elsewhen(io.out.fire) {
    valid := false.B
  }
}
