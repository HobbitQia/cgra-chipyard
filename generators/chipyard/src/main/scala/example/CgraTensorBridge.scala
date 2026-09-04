package chipyard.example

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters

case class CgraRequantParams(multiplier: Int, shift: Int) {
  require(multiplier > 0)
  require(shift >= 0 && shift < 31)
}

case class CgraTensorBridgeParams(
    packedBaseAddress: BigInt,
    packedBytes: Int,
    packedWindowBytes: Int,
    outboundSpmWord: Int,
    outboundWords: Int,
    inboundSpmWord: Int,
    inboundWords: Int,
    outboundScale: CgraRequantParams,
    inboundScale: CgraRequantParams) {
  require(packedBaseAddress >= 0)
  require(packedBytes > 0 && packedWindowBytes >= packedBytes)
  require(isPow2(packedWindowBytes))
  require(outboundSpmWord >= 0 && outboundWords > 0)
  require(outboundWords == packedBytes)
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

class CgraSpmReadMux(params: CGRASpmReadParams) extends Module {
  val io = IO(new Bundle {
    val raw = Flipped(new CGRASpmReadIO(params))
    val packed = Flipped(new CGRASpmReadIO(params))
    val spm = new CGRASpmReadIO(params)
  })

  val locked = RegInit(false.B)
  val packed = RegInit(false.B)
  val selectPacked = !io.raw.req.valid && io.packed.req.valid

  io.spm.req.valid := !locked && (io.raw.req.valid || io.packed.req.valid)
  io.spm.req.bits := Mux(selectPacked, io.packed.req.bits, io.raw.req.bits)
  io.raw.req.ready := !locked && !selectPacked && io.spm.req.ready
  io.packed.req.ready := !locked && selectPacked && io.spm.req.ready

  io.raw.resp.valid := locked && !packed && io.spm.resp.valid
  io.raw.resp.bits := io.spm.resp.bits
  io.packed.resp.valid := locked && packed && io.spm.resp.valid
  io.packed.resp.bits := io.spm.resp.bits
  io.spm.resp.ready := locked && Mux(packed, io.packed.resp.ready, io.raw.resp.ready)
  io.spm.busy := io.raw.busy || io.packed.busy

  when(io.spm.req.fire) {
    packed := selectPacked
    locked := true.B
  }
  when(io.spm.resp.fire) {
    locked := false.B
  }
}

class CgraPackedSpmManager(
    params: CGRASpmReadParams,
    bridge: CgraTensorBridgeParams,
    beatBytes: Int,
    blockBytes: Int)(implicit p: Parameters)
    extends LazyModule {
  require(params.enabled && params.dataWidth == 32)
  require(isPow2(beatBytes) && isPow2(blockBytes))
  require(bridge.packedWindowBytes >= blockBytes)
  require(bridge.packedBaseAddress % bridge.packedWindowBytes == 0)

  private val device = new SimpleDevice("cgra-spm-packed", Seq("coredac,cgra-spm-packed"))
  val node = TLManagerNode(Seq(TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(bridge.packedBaseAddress, bridge.packedWindowBytes - 1)),
      resources = device.reg,
      regionType = RegionType.IDEMPOTENT,
      executable = false,
      supportsGet = TransferSizes(1, beatBytes),
      fifoId = Some(0))),
    beatBytes = beatBytes,
    minLatency = 1)))

  override lazy val module = new CgraPackedSpmManagerImp(this, params, bridge, beatBytes)
}

class CgraPackedSpmManagerImp(
    outer: CgraPackedSpmManager,
    params: CGRASpmReadParams,
    bridge: CgraTensorBridgeParams,
    beatBytes: Int)(implicit p: Parameters)
    extends LazyModuleImp(outer) {
  val io = IO(new CGRASpmReadIO(params))
  val (tl, edge) = outer.node.in(0)
  val beatShift = log2Ceil(beatBytes)
  val indexWidth = math.max(1, log2Ceil(beatBytes))

  val idle :: request :: response :: reply :: Nil = Enum(4)
  val state = RegInit(idle)
  val source = Reg(UInt(edge.bundle.sourceBits.W))
  val size = Reg(UInt(edge.bundle.sizeBits.W))
  val address = Reg(UInt(edge.bundle.addressBits.W))
  val lastByte = Reg(UInt(indexWidth.W))
  val byteIndex = RegInit(0.U(indexWidth.W))
  val bytes = Reg(Vec(beatBytes, UInt(8.W)))
  val firstLane = if (beatBytes == 1) 0.U else address(beatShift - 1, 0)

  tl.a.ready := state === idle
  tl.d.valid := state === reply
  tl.d.bits := edge.AccessAck(source, size, bytes.asUInt)
  tl.b.valid := false.B
  tl.b.bits := DontCare
  tl.c.ready := true.B
  tl.e.ready := true.B

  val aliasOffset = address - bridge.packedBaseAddress.U
  val tensorByte = aliasOffset + byteIndex
  val tensorByteValid = tensorByte < bridge.packedBytes.U
  io.req.valid := state === request && tensorByteValid
  io.req.bits := (bridge.outboundSpmWord.U + tensorByte).pad(params.addrWidth)
  io.resp.ready := state === response
  io.busy := state =/= idle

  when(tl.a.fire) {
    assert(tl.a.bits.opcode === TLMessages.Get)
    assert(tl.a.bits.size <= beatShift.U)
    val requestBytes = 1.U((beatShift + 1).W) << tl.a.bits.size
    source := tl.a.bits.source
    size := tl.a.bits.size
    address := tl.a.bits.address
    lastByte := requestBytes - 1.U
    byteIndex := 0.U
    bytes.foreach(_ := 0.U)
    state := request
  }
  when(io.req.fire) {
    state := response
  }
  when(state === request && !tensorByteValid) {
    bytes(firstLane + byteIndex) := 0.U
    when(byteIndex === lastByte) {
      state := reply
    }.otherwise {
      byteIndex := byteIndex + 1.U
    }
  }
  when(io.resp.fire) {
    bytes(firstLane + byteIndex) := CgraRequant.byte(io.resp.bits.asSInt, bridge.outboundScale)
    when(byteIndex === lastByte) {
      state := reply
    }.otherwise {
      byteIndex := byteIndex + 1.U
      state := request
    }
  }
  when(tl.d.fire) {
    state := idle
  }
}
