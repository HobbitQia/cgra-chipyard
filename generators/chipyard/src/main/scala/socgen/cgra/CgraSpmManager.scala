package chipyard.socgen.cgra

import chisel3._
import chisel3.util._
import chipyard.example.{CGRASpmReadIO, CGRASpmReadParams, CGRASpmWindowParams}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters

class CgraSpmManager(
    params: CGRASpmReadParams,
    window: CGRASpmWindowParams,
    beatBytes: Int,
    blockBytes: Int)(implicit p: Parameters)
    extends LazyModule {
  require(params.enabled)
  require(params.dataWidth % 8 == 0)
  require(params.words <= (BigInt(1) << params.addrWidth))
  val elementBits = if (window.quantize) 8 else params.dataWidth
  private val elementBytes = elementBits / 8
  require(isPow2(elementBytes))
  require(isPow2(beatBytes) && isPow2(blockBytes))
  require(beatBytes % elementBytes == 0)
  require(isPow2(window.sizeBytes))
  require((window.baseAddress & (window.sizeBytes - 1)) == 0)
  if (window.quantize) {
    require(params.dataWidth == 32)
    require(window.sizeBytes >= math.max(window.bridge.get.outboundWords, blockBytes))
  } else {
    require(window.sizeBytes == params.words * elementBytes)
  }

  private val device = new SimpleDevice("cgra-spm", Seq("coredac,cgra-spm"))
  val node = TLManagerNode(Seq(TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(window.baseAddress, window.sizeBytes - 1)),
      resources = device.reg,
      regionType = RegionType.IDEMPOTENT,
      executable = false,
      supportsGet = TransferSizes(1, beatBytes),
      fifoId = Some(0))),
    beatBytes = beatBytes,
    minLatency = 1)))

  override lazy val module = new CgraSpmManagerImp(this, params, window, beatBytes)
}

class CgraSpmManagerImp(
    outer: CgraSpmManager,
    params: CGRASpmReadParams,
    window: CGRASpmWindowParams,
    beatBytes: Int)(implicit p: Parameters)
    extends LazyModuleImp(outer) {
  val io = IO(new CGRASpmReadIO(params))
  val (tl, edge) = outer.node.in(0)
  val elementBytes = outer.elementBits / 8
  val elementsPerBeat = beatBytes / elementBytes
  val elementShift = log2Ceil(elementBytes)
  val indexWidth = math.max(1, log2Ceil(elementsPerBeat))
  val beatShift = log2Ceil(beatBytes)

  val idle :: request :: response :: reply :: Nil = Enum(4)
  val state = RegInit(idle)
  val source = Reg(UInt(edge.bundle.sourceBits.W))
  val size = Reg(UInt(edge.bundle.sizeBits.W))
  val address = Reg(UInt(edge.bundle.addressBits.W))
  val lastElement = Reg(UInt(indexWidth.W))
  val elementIndex = RegInit(0.U(indexWidth.W))
  val elements = Reg(Vec(elementsPerBeat, UInt(outer.elementBits.W)))
  val firstLane = if (elementsPerBeat == 1) 0.U else address(beatShift - 1, elementShift)

  tl.a.ready := state === idle
  tl.d.valid := state === reply
  tl.d.bits := edge.AccessAck(source, size, elements.asUInt)
  tl.b.valid := false.B
  tl.b.bits := DontCare
  tl.c.ready := true.B
  tl.e.ready := true.B

  val elementOffset = ((address - window.baseAddress.U) >> elementShift) + elementIndex
  val elementValid = window.bridge.map(bridge => elementOffset < bridge.outboundWords.U).getOrElse(true.B)
  val spmBase = window.bridge.map(_.outboundSpmWord).getOrElse(0)
  val padding = state === request && !elementValid
  val value = if (window.quantize) {
    CgraRequant.byte(io.resp.bits.asSInt, window.bridge.get.outboundScale)
  } else {
    io.resp.bits
  }
  io.req.valid := state === request && elementValid
  io.req.bits := (spmBase.U + elementOffset).pad(params.addrWidth)
  io.resp.ready := state === response
  io.busy := state =/= idle

  when(tl.a.fire) {
    assert(tl.a.bits.opcode === TLMessages.Get)
    assert(tl.a.bits.size <= beatShift.U)
    val requestBytes = 1.U((beatShift + 1).W) << tl.a.bits.size
    val requestElements = Mux(requestBytes < elementBytes.U, 1.U, requestBytes >> elementShift)
    source := tl.a.bits.source
    size := tl.a.bits.size
    address := tl.a.bits.address
    lastElement := requestElements - 1.U
    elementIndex := 0.U
    elements.foreach(_ := 0.U)
    state := request
  }
  when(io.req.fire) {
    state := response
  }
  when(io.resp.fire || padding) {
    elements(firstLane + elementIndex) := Mux(padding, 0.U, value)
    when(elementIndex === lastElement) {
      state := reply
    }.otherwise {
      elementIndex := elementIndex + 1.U
      state := request
    }
  }
  when(tl.d.fire) {
    state := idle
  }
}
