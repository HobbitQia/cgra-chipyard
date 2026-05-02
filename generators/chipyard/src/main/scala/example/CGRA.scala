package chipyard.example

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy._

// ============================================================================
// CGRA Parameters
// ============================================================================

case class CGRAParams(
  // IntraCgraPkt bit width (from PyMTL3 generated Verilog)
  intraPktWidth: Int = 182,
  // InterCgraPkt bit width
  interPktWidth: Int = 185,
  // Data payload bit width
  dataPayloadWidth: Int = 32,
  // DataType bit width (payload + predicate + bypass + delay)
  dataWidth: Int = 35,
  // CgraPayload bit width
  payloadWidth: Int = 157,
  // cgra_id width
  idWidth: Int = 2,
  // address width (clog2(128) = 7)
  addrWidth: Int = 7,
  // CGRA grid dimensions
  xTiles: Int = 2,
  yTiles: Int = 2,
  // cmd field width (clog2(32) = 5)
  cmdWidth: Int = 5,
  // Number of tiles
  numTiles: Int = 4,
  // Static single-CGRA address map
  addressLower: Int = 0,
  addressUpper: Int = 31,
  // Whether the generated PyMTL top exposes multi-CGRA boundary data ports
  hasBoundaryPorts: Boolean = true,
  // Generated Verilog resources
  topModuleName: String = "CgraRTL_2x2",
  wrapperModuleName: String = "CgraRTL_2x2_wrapper",
  rtlResource: String = "/vsrc/CgraRTL_2x2__pickled.v",
  wrapperResource: String = "/vsrc/CgraRTL_2x2_wrapper.v"
)

// ============================================================================
// CGRA BlackBox
// ============================================================================

class CgraRecvChannel(width: Int) extends Bundle {
  val `val` = Input(Bool())
  val msg = Input(UInt(width.W))
  val rdy = Output(Bool())
}

class CgraSendChannel(width: Int) extends Bundle {
  val `val` = Output(Bool())
  val msg = Output(UInt(width.W))
  val rdy = Input(Bool())
}

class CGRABlackBox(params: CGRAParams) extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk   = Input(Clock())
    val reset = Input(Bool())

    // CPU control interface (val-rdy, IntraCgraPkt = 182 bits flat)
    val recv_from_cpu_pkt_val = Input(Bool())
    val recv_from_cpu_pkt_msg = Input(UInt(params.intraPktWidth.W))
    val recv_from_cpu_pkt_rdy = Output(Bool())

    val send_to_cpu_pkt_val = Output(Bool())
    val send_to_cpu_pkt_msg = Output(UInt(params.intraPktWidth.W))
    val send_to_cpu_pkt_rdy = Input(Bool())

    // Inter-CGRA NoC interface (InterCgraPkt = 185 bits flat)
    val recv_from_inter_cgra_noc_val = Input(Bool())
    val recv_from_inter_cgra_noc_msg = Input(UInt(params.interPktWidth.W))
    val recv_from_inter_cgra_noc_rdy = Output(Bool())

    val send_to_inter_cgra_noc_val = Output(Bool())
    val send_to_inter_cgra_noc_msg = Output(UInt(params.interPktWidth.W))
    val send_to_inter_cgra_noc_rdy = Input(Bool())

    // Boundary data ports. Single-CGRA CgraTemplateRTL builds omit them.
    val recv_data_on_boundary_south =
      if (params.hasBoundaryPorts) Some(Vec(params.xTiles, new CgraRecvChannel(params.dataWidth))) else None
    val send_data_on_boundary_south =
      if (params.hasBoundaryPorts) Some(Vec(params.xTiles, new CgraSendChannel(params.dataWidth))) else None
    val recv_data_on_boundary_north =
      if (params.hasBoundaryPorts) Some(Vec(params.xTiles, new CgraRecvChannel(params.dataWidth))) else None
    val send_data_on_boundary_north =
      if (params.hasBoundaryPorts) Some(Vec(params.xTiles, new CgraSendChannel(params.dataWidth))) else None
    val recv_data_on_boundary_east =
      if (params.hasBoundaryPorts) Some(Vec(params.yTiles, new CgraRecvChannel(params.dataWidth))) else None
    val send_data_on_boundary_east =
      if (params.hasBoundaryPorts) Some(Vec(params.yTiles, new CgraSendChannel(params.dataWidth))) else None
    val recv_data_on_boundary_west =
      if (params.hasBoundaryPorts) Some(Vec(params.yTiles, new CgraRecvChannel(params.dataWidth))) else None
    val send_data_on_boundary_west =
      if (params.hasBoundaryPorts) Some(Vec(params.yTiles, new CgraSendChannel(params.dataWidth))) else None

    // Configuration
    val cgra_id       = Input(UInt(params.idWidth.W))
    val address_lower = Input(UInt(params.addrWidth.W))
    val address_upper = Input(UInt(params.addrWidth.W))
  })

  override def desiredName: String = params.wrapperModuleName
  addResource(params.rtlResource)
  addResource(params.wrapperResource)
}

object CGRACmd {
  // The wrapper forwards raw packets and only interprets commands needed for
  // host-side busy/completion tracking.
  def launch(width: Int): UInt = 0.U(width.W)
  def complete(width: Int): UInt = 14.U(width.W)
  def resume(width: Int): UInt = 15.U(width.W)
}

// ============================================================================
// RoCC Accelerator — LazyRoCC wrapper for the CGRA
// ============================================================================
//
// RoCC instruction encoding (funct7 field):
//   2 = STATUS:       Query status -> result in rd
//   4 = WAIT:         Block until the CGRA is no longer busy
//   5 = RAW_PKT_LO:   Stash low 64 bits of an IntraCgraPkt
//   6 = RAW_PKT_MID:  Stash middle 64 bits of an IntraCgraPkt
//   7 = RAW_PKT_HI:   Stash or send bits [191:128] of an IntraCgraPkt
//   8 = SET_EXPECTED_COMPLETES: rs1=number of CMD_COMPLETE packets to wait for
//   9 = RESULT:       Return the last 32-bit CMD_COMPLETE payload
//   10 = RAW_PKT_TOP: Send bits above 192 and trigger transmit when needed
//
// ============================================================================

class CGRAAccelerator(opcodes: OpcodeSet, params: CGRAParams = CGRAGenerated.params)(implicit p: Parameters)
    extends LazyRoCC(opcodes) {
  override lazy val module = new CGRAAcceleratorImp(this, params)
}

class CGRAAcceleratorImp(outer: CGRAAccelerator, params: CGRAParams)(implicit p: Parameters)
    extends LazyRoCCModuleImp(outer) with HasCoreParameters {

  // ---- CGRA BlackBox instantiation ----
  val cgra = Module(new CGRABlackBox(params))

  // Clock and reset
  cgra.io.clk   := clock
  cgra.io.reset := reset.asBool

  // Static configuration
  cgra.io.cgra_id       := 0.U  // Single CGRA, ID = 0
  cgra.io.address_lower := params.addressLower.U
  cgra.io.address_upper := params.addressUpper.U

  // ---- Tie off unused ports ----

  // Inter-CGRA NoC (not used in single-CGRA mode)
  cgra.io.recv_from_inter_cgra_noc_val := false.B
  cgra.io.recv_from_inter_cgra_noc_msg := 0.U
  cgra.io.send_to_inter_cgra_noc_rdy   := false.B

  // Boundary data ports — all tied off until a kernel needs external streams.
  def tieOffRecv(ch: CgraRecvChannel): Unit = {
    ch.`val` := false.B
    ch.msg := 0.U
  }

  def tieOffSend(ch: CgraSendChannel): Unit = {
    ch.rdy := false.B
  }

  cgra.io.recv_data_on_boundary_south.foreach(_.foreach(tieOffRecv))
  cgra.io.send_data_on_boundary_south.foreach(_.foreach(tieOffSend))
  cgra.io.recv_data_on_boundary_north.foreach(_.foreach(tieOffRecv))
  cgra.io.send_data_on_boundary_north.foreach(_.foreach(tieOffSend))
  cgra.io.recv_data_on_boundary_east.foreach(_.foreach(tieOffRecv))
  cgra.io.send_data_on_boundary_east.foreach(_.foreach(tieOffSend))
  cgra.io.recv_data_on_boundary_west.foreach(_.foreach(tieOffRecv))
  cgra.io.send_data_on_boundary_west.foreach(_.foreach(tieOffSend))

  // ---- RoCC Command Interface ----
  val cmd = Queue(io.cmd)

  val funct = cmd.bits.inst.funct
  val rs1   = cmd.bits.rs1

  // Funct7 command encoding
  val isStatus    = funct === 2.U
  val isWait      = funct === 4.U
  val isRawPktLo  = funct === 5.U
  val isRawPktMid = funct === 6.U
  val isRawPktHi  = funct === 7.U
  val isSetExpectedCompletes = funct === 8.U
  val isResult    = funct === 9.U
  val isRawPktTop = funct === 10.U

  // ---- State Machine ----
  val s_idle :: s_send_pkt :: s_wait_complete :: s_resp :: Nil = Enum(4)
  val state = RegInit(s_idle)

  // Status registers
  val cgraComplete = RegInit(false.B)
  val cgraBusy     = RegInit(false.B)
  val completeCount = RegInit(0.U(16.W))
  val expectedCompleteCount = RegInit(0.U(16.W))

  // Packet assembly registers
  val pktValid = RegInit(false.B)
  val pktData  = RegInit(0.U(params.intraPktWidth.W))
  val rawPktLo  = RegInit(0.U(64.W))
  val rawPktMid = RegInit(0.U(64.W))
  val rawPktHi = RegInit(0.U(64.W))
  val rawPktHiWidth = params.intraPktWidth - 128
  require(rawPktHiWidth > 0, s"intraPktWidth must be greater than 128, got ${params.intraPktWidth}")
  val needsRawPktTop = rawPktHiWidth > 64
  val rawPktTopWidth = params.intraPktWidth - 192
  if (needsRawPktTop) {
    require(rawPktTopWidth > 0 && rawPktTopWidth <= 64,
      s"intraPktWidth ${params.intraPktWidth} requires unsupported raw packet top width ${rawPktTopWidth}")
  }

  // Most CGRA kernels return scalar data in the payload bits of CMD_COMPLETE.
  val lastCompleteData = RegInit(0.U(params.dataPayloadWidth.W))

  // Response registers
  val respValid = RegInit(false.B)
  val respData  = RegInit(0.U(xLen.W))
  val respRd    = Reg(chiselTypeOf(io.resp.bits.rd))

  // PyMTL packs payload at the LSB side of IntraCgraPkt. Within the payload,
  // cmd is the MSB field and data immediately follows it.
  val pktCmdMsb = params.payloadWidth - 1
  val pktCmdLsb = params.payloadWidth - params.cmdWidth
  val pktDataPayloadMsb = pktCmdLsb - 1
  val pktDataPayloadLsb = pktDataPayloadMsb - params.dataPayloadWidth + 1
  require(params.payloadWidth <= params.intraPktWidth,
    s"payloadWidth ${params.payloadWidth} exceeds intraPktWidth ${params.intraPktWidth}")
  require(pktDataPayloadLsb >= 0,
    s"data payload does not fit payloadWidth=${params.payloadWidth}, cmdWidth=${params.cmdWidth}")

  def acceptAssembledPkt(assembledPkt: UInt): Unit = {
    val assembledCmd = assembledPkt(pktCmdMsb, pktCmdLsb)
    pktData := assembledPkt
    pktValid := true.B
    when (assembledCmd === CGRACmd.launch(params.cmdWidth) ||
          assembledCmd === CGRACmd.resume(params.cmdWidth)) {
      noteLaunchIssued()
    }
    state := s_send_pkt
  }

  def noteLaunchIssued(): Unit = {
    when (expectedCompleteCount === 0.U) {
      when (cgraBusy) {
        expectedCompleteCount := expectedCompleteCount + 1.U
      } .otherwise {
        completeCount := 0.U
        expectedCompleteCount := 1.U
      }
    }
    when (!cgraBusy && expectedCompleteCount === 0.U) {
      completeCount := 0.U
    }
    cgraBusy := true.B
    cgraComplete := false.B
  }

  // ---- Packet Construction Logic ----
  when (state === s_idle && cmd.valid) {
    respRd := cmd.bits.inst.rd

    when (isStatus) {
      respData := Cat(0.U((xLen - 17).W), completeCount, cgraComplete)
      respValid := true.B
      state := s_resp
    } .elsewhen (isResult) {
      respData := lastCompleteData
      respValid := true.B
      state := s_resp
    } .elsewhen (isWait) {
      when (!cgraBusy) {
        respData := 1.U
        respValid := true.B
        state := s_resp
      } .otherwise {
        state := s_wait_complete
      }
    } .elsewhen (isSetExpectedCompletes) {
      expectedCompleteCount := rs1(15, 0)
      completeCount := 0.U
      cgraComplete := false.B
    } .elsewhen (isRawPktLo) {
      rawPktLo := rs1
    } .elsewhen (isRawPktMid) {
      rawPktMid := rs1
    } .elsewhen (isRawPktHi) {
      if (needsRawPktTop) {
        rawPktHi := rs1
      } else {
        acceptAssembledPkt(Cat(rs1(rawPktHiWidth - 1, 0), rawPktMid, rawPktLo))
      }
    } .elsewhen (isRawPktTop) {
      if (needsRawPktTop) {
        acceptAssembledPkt(Cat(rs1(rawPktTopWidth - 1, 0), rawPktHi, rawPktMid, rawPktLo))
      }
    }
  }

  // ---- Send packet to CGRA ----
  cgra.io.recv_from_cpu_pkt_val := pktValid && (state === s_send_pkt)
  cgra.io.recv_from_cpu_pkt_msg := pktData

  when (state === s_send_pkt) {
    when (cgra.io.recv_from_cpu_pkt_rdy && pktValid) {
      pktValid := false.B
      state := s_idle
    }
  }

  // ---- Wait for completion ----
  when (state === s_wait_complete && !cgraBusy) {
    respData := 1.U
    respValid := true.B
    state := s_resp
  }

  // ---- Monitor CGRA output (send_to_cpu_pkt) ----
  cgra.io.send_to_cpu_pkt_rdy := true.B  // Always ready to receive from CGRA

  when (cgra.io.send_to_cpu_pkt_val) {
    val recvPkt = cgra.io.send_to_cpu_pkt_msg
    val recvCmd = recvPkt(pktCmdMsb, pktCmdLsb)
    when (recvCmd === CGRACmd.complete(params.cmdWidth)) {
      when (expectedCompleteCount =/= 0.U) {
        lastCompleteData := recvPkt(pktDataPayloadMsb, pktDataPayloadLsb)
        completeCount := completeCount + 1.U
        when (completeCount + 1.U >= expectedCompleteCount) {
          cgraComplete := true.B
          cgraBusy := false.B
          expectedCompleteCount := 0.U
        }
      }
    }
  }

  // ---- RoCC Command Ready ----
  cmd.ready := (state === s_idle) && !respValid

  // ---- RoCC Response Interface ----
  io.resp.valid     := respValid
  io.resp.bits.rd   := respRd
  io.resp.bits.data := respData

  when (io.resp.fire) {
    respValid := false.B
    state := s_idle
  }

  // ---- RoCC Busy / Interrupt ----
  io.busy := cmd.valid || cgraBusy || pktValid || (state =/= s_idle)
  io.interrupt := false.B

  // ---- Unused memory interface ----
  io.mem.req.valid := false.B
}
