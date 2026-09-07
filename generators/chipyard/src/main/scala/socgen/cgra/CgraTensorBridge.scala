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

class CgraReadRequest(addressWidth: Int, beatBits: Int) extends Bundle {
  val address = UInt(addressWidth.W)
  val lgSize = UInt(log2Ceil(log2Ceil(beatBits / 8) + 1).W)
}

class CgraPackedReader(beatBits: Int, addressWidth: Int, lengthWidth: Int) extends Module {
  require(beatBits >= 32 && isPow2(beatBits))

  private val beatBytes = beatBits / 8
  private val lanes = beatBits / 32
  private val lgBeatBytes = log2Ceil(beatBytes)
  private val countWidth = log2Ceil(beatBytes + 1)

  val io = IO(new Bundle {
    val packed = Input(Bool())
    val start = Flipped(Valid(new Bundle {
      val address = UInt(addressWidth.W)
      val bytes = UInt(lengthWidth.W)
    }))
    val nativeReq = Flipped(Decoupled(UInt(addressWidth.W)))
    val nativeResp = Decoupled(UInt(beatBits.W))
    val memoryReq = Decoupled(new CgraReadRequest(addressWidth, beatBits))
    val memoryResp = Flipped(Decoupled(UInt(beatBits.W)))
    val busy = Output(Bool())
  })

  val idle :: fetch :: waitData :: respond :: Nil = Enum(4)
  val state = RegInit(idle)
  val active = RegInit(false.B)
  val rawBusy = RegInit(false.B)
  val address = Reg(UInt(addressWidth.W))
  val nativeAddress = Reg(UInt(addressWidth.W))
  val remaining = Reg(UInt(lengthWidth.W))
  val cache = RegInit(0.U(beatBits.W))
  val cached = RegInit(0.U(countWidth.W))
  val requestBytes = Reg(UInt(countWidth.W))

  val size = WireDefault(0.U(io.memoryReq.bits.lgSize.getWidth.W))
  for (lg <- 1 to lgBeatBytes) {
    when(address(lg - 1, 0) === 0.U && remaining >= (1 << lg).U &&
      beatBytes.U - cached >= (1 << lg).U) {
      size := lg.U
    }
  }
  val fetchBytes = (1.U(countWidth.W) << size)(countWidth - 1, 0)
  val mask = ((1.U((beatBits + 1).W) << (requestBytes << 3)) - 1.U)(beatBits - 1, 0)
  val expanded = VecInit((0 until lanes).map { lane =>
    val value = cache(8 * lane + 7, 8 * lane)
    Mux(lane.U < cached, Cat(Fill(24, value(7)), value), 0.U(32.W))
  }).asUInt

  io.nativeReq.ready := io.memoryReq.ready
  io.nativeResp.valid := io.memoryResp.valid
  io.nativeResp.bits := io.memoryResp.bits
  io.memoryReq.valid := io.nativeReq.valid
  io.memoryReq.bits.address := io.nativeReq.bits
  io.memoryReq.bits.lgSize := lgBeatBytes.U
  io.memoryResp.ready := io.nativeResp.ready
  io.busy := Mux(io.packed, active, rawBusy)

  when(!io.packed) {
    when(io.nativeReq.fire) { rawBusy := true.B }
    when(io.nativeResp.fire) { rawBusy := false.B }
  }

  when(io.packed) {
    io.nativeReq.ready := state === idle && active
    io.nativeResp.valid := state === respond
    io.nativeResp.bits := expanded
    io.memoryReq.valid := state === fetch
    io.memoryReq.bits.address := address
    io.memoryReq.bits.lgSize := size
    io.memoryResp.ready := state === waitData

    when(io.nativeReq.fire) {
      assert(io.nativeReq.bits === nativeAddress, "Packed DMA requests must be sequential")
      nativeAddress := nativeAddress + beatBytes.U
      state := Mux(cached === 0.U, fetch, respond)
    }
    when(io.memoryReq.fire) {
      requestBytes := fetchBytes
      state := waitData
    }
    when(io.memoryResp.fire) {
      cache := cache | ((io.memoryResp.bits & mask) << (cached << 3))
      cached := cached + requestBytes
      address := address + requestBytes
      remaining := remaining - requestBytes
      state := Mux(cached + requestBytes === beatBytes.U || remaining === requestBytes,
        respond, fetch)
    }
    when(io.nativeResp.fire) {
      cache := cache >> (lanes * 8)
      cached := Mux(cached > lanes.U, cached - lanes.U, 0.U)
      state := idle
      when(remaining === 0.U && cached <= lanes.U) {
        active := false.B
      }
    }
  }

  when(io.start.valid) {
    address := io.start.bits.address
    nativeAddress := io.start.bits.address
    remaining := io.start.bits.bytes
    cache := 0.U
    cached := 0.U
    state := idle
    active := true.B
    rawBusy := false.B
  }
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
