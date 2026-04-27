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
  // DataType bit width (32 payload + 1 predicate + 1 bypass + 1 delay)
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
  numTiles: Int = 4
)

// ============================================================================
// CGRA BlackBox — wraps CgraRTL_2x2_wrapper.v which in turn wraps the
// PyMTL3-generated CgraRTL_2x2. The wrapper flattens SystemVerilog structs
// and unpacked arrays into plain logic ports for Chisel compatibility.
// ============================================================================

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

    // Boundary data ports — south [0] and [1] (DataType = 35 bits each)
    val recv_data_on_boundary_south_0_val = Input(Bool())
    val recv_data_on_boundary_south_0_msg = Input(UInt(params.dataWidth.W))
    val recv_data_on_boundary_south_0_rdy = Output(Bool())
    val recv_data_on_boundary_south_1_val = Input(Bool())
    val recv_data_on_boundary_south_1_msg = Input(UInt(params.dataWidth.W))
    val recv_data_on_boundary_south_1_rdy = Output(Bool())

    val send_data_on_boundary_south_0_val = Output(Bool())
    val send_data_on_boundary_south_0_msg = Output(UInt(params.dataWidth.W))
    val send_data_on_boundary_south_0_rdy = Input(Bool())
    val send_data_on_boundary_south_1_val = Output(Bool())
    val send_data_on_boundary_south_1_msg = Output(UInt(params.dataWidth.W))
    val send_data_on_boundary_south_1_rdy = Input(Bool())

    // Boundary data ports — north [0] and [1]
    val recv_data_on_boundary_north_0_val = Input(Bool())
    val recv_data_on_boundary_north_0_msg = Input(UInt(params.dataWidth.W))
    val recv_data_on_boundary_north_0_rdy = Output(Bool())
    val recv_data_on_boundary_north_1_val = Input(Bool())
    val recv_data_on_boundary_north_1_msg = Input(UInt(params.dataWidth.W))
    val recv_data_on_boundary_north_1_rdy = Output(Bool())

    val send_data_on_boundary_north_0_val = Output(Bool())
    val send_data_on_boundary_north_0_msg = Output(UInt(params.dataWidth.W))
    val send_data_on_boundary_north_0_rdy = Input(Bool())
    val send_data_on_boundary_north_1_val = Output(Bool())
    val send_data_on_boundary_north_1_msg = Output(UInt(params.dataWidth.W))
    val send_data_on_boundary_north_1_rdy = Input(Bool())

    // Boundary data ports — east [0] and [1]
    val recv_data_on_boundary_east_0_val = Input(Bool())
    val recv_data_on_boundary_east_0_msg = Input(UInt(params.dataWidth.W))
    val recv_data_on_boundary_east_0_rdy = Output(Bool())
    val recv_data_on_boundary_east_1_val = Input(Bool())
    val recv_data_on_boundary_east_1_msg = Input(UInt(params.dataWidth.W))
    val recv_data_on_boundary_east_1_rdy = Output(Bool())

    val send_data_on_boundary_east_0_val = Output(Bool())
    val send_data_on_boundary_east_0_msg = Output(UInt(params.dataWidth.W))
    val send_data_on_boundary_east_0_rdy = Input(Bool())
    val send_data_on_boundary_east_1_val = Output(Bool())
    val send_data_on_boundary_east_1_msg = Output(UInt(params.dataWidth.W))
    val send_data_on_boundary_east_1_rdy = Input(Bool())

    // Boundary data ports — west [0] and [1]
    val recv_data_on_boundary_west_0_val = Input(Bool())
    val recv_data_on_boundary_west_0_msg = Input(UInt(params.dataWidth.W))
    val recv_data_on_boundary_west_0_rdy = Output(Bool())
    val recv_data_on_boundary_west_1_val = Input(Bool())
    val recv_data_on_boundary_west_1_msg = Input(UInt(params.dataWidth.W))
    val recv_data_on_boundary_west_1_rdy = Output(Bool())

    val send_data_on_boundary_west_0_val = Output(Bool())
    val send_data_on_boundary_west_0_msg = Output(UInt(params.dataWidth.W))
    val send_data_on_boundary_west_0_rdy = Input(Bool())
    val send_data_on_boundary_west_1_val = Output(Bool())
    val send_data_on_boundary_west_1_msg = Output(UInt(params.dataWidth.W))
    val send_data_on_boundary_west_1_rdy = Input(Bool())

    // Configuration
    val cgra_id       = Input(UInt(params.idWidth.W))
    val address_lower = Input(UInt(params.addrWidth.W))
    val address_upper = Input(UInt(params.addrWidth.W))
  })

  override def desiredName: String = "CgraRTL_2x2_wrapper"
  addResource("/vsrc/CgraRTL_2x2_wrapper.v")
  addResource("/vsrc/CgraRTL_2x2__pickled.v")
}

// ============================================================================
// CGRA Command Types (matching VectorCGRA/lib/cmd_type.py)
// ============================================================================

object CGRACmd {
  val CMD_LAUNCH                  = 0.U(5.W)
  val CMD_PAUSE                   = 1.U(5.W)
  val CMD_TERMINATE               = 2.U(5.W)
  val CMD_CONFIG                  = 3.U(5.W)
  val CMD_CONFIG_TOTAL_CTRL_COUNT = 7.U(5.W)
  val CMD_CONFIG_COUNT_PER_ITER   = 8.U(5.W)
  val CMD_COMPLETE                = 14.U(5.W)
}

// ============================================================================
// RoCC Accelerator — LazyRoCC wrapper for the CGRA
// ============================================================================
//
// RoCC instruction encoding (funct7 field):
//   0 = LAUNCH:      Start CGRA execution
//   1 = CONFIG:       Configure tile — rs1=tile_id, rs2=config_word
//   2 = STATUS:       Query status → result in rd
//   3 = CONFIG_LOOP:  Configure loop — rs1=count_per_iter, rs2=total_count
//   4 = WAIT:         Block until the CGRA is no longer busy
//
// ============================================================================

class CGRAAccelerator(opcodes: OpcodeSet, params: CGRAParams = CGRAParams())(implicit p: Parameters)
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
  cgra.io.address_lower := 0.U
  cgra.io.address_upper := 31.U // per_cgra_data_size - 1 = 128/4 - 1 = 31

  // ---- Tie off unused ports ----

  // Inter-CGRA NoC (not used in single-CGRA mode)
  cgra.io.recv_from_inter_cgra_noc_val := false.B
  cgra.io.recv_from_inter_cgra_noc_msg := 0.U
  cgra.io.send_to_inter_cgra_noc_rdy   := false.B

  // Boundary data ports — all tied off (Phase 1: no external data connections)
  cgra.io.recv_data_on_boundary_south_0_val := false.B
  cgra.io.recv_data_on_boundary_south_0_msg := 0.U
  cgra.io.send_data_on_boundary_south_0_rdy := false.B
  cgra.io.recv_data_on_boundary_south_1_val := false.B
  cgra.io.recv_data_on_boundary_south_1_msg := 0.U
  cgra.io.send_data_on_boundary_south_1_rdy := false.B

  cgra.io.recv_data_on_boundary_north_0_val := false.B
  cgra.io.recv_data_on_boundary_north_0_msg := 0.U
  cgra.io.send_data_on_boundary_north_0_rdy := false.B
  cgra.io.recv_data_on_boundary_north_1_val := false.B
  cgra.io.recv_data_on_boundary_north_1_msg := 0.U
  cgra.io.send_data_on_boundary_north_1_rdy := false.B

  cgra.io.recv_data_on_boundary_east_0_val := false.B
  cgra.io.recv_data_on_boundary_east_0_msg := 0.U
  cgra.io.send_data_on_boundary_east_0_rdy := false.B
  cgra.io.recv_data_on_boundary_east_1_val := false.B
  cgra.io.recv_data_on_boundary_east_1_msg := 0.U
  cgra.io.send_data_on_boundary_east_1_rdy := false.B

  cgra.io.recv_data_on_boundary_west_0_val := false.B
  cgra.io.recv_data_on_boundary_west_0_msg := 0.U
  cgra.io.send_data_on_boundary_west_0_rdy := false.B
  cgra.io.recv_data_on_boundary_west_1_val := false.B
  cgra.io.recv_data_on_boundary_west_1_msg := 0.U
  cgra.io.send_data_on_boundary_west_1_rdy := false.B

  // ---- RoCC Command Interface ----
  val cmd = Queue(io.cmd)

  val funct = cmd.bits.inst.funct
  val rs1   = cmd.bits.rs1
  val rs2   = cmd.bits.rs2

  // Funct7 command encoding
  val isLaunch    = funct === 0.U
  val isConfig    = funct === 1.U
  val isStatus    = funct === 2.U
  val isLoopCfg   = funct === 3.U
  val isWait      = funct === 4.U

  // ---- State Machine ----
  val s_idle :: s_send_pkt :: s_wait_rdy :: s_wait_complete :: s_resp :: Nil = Enum(5)
  val state = RegInit(s_idle)

  // Status registers
  val cgraComplete = RegInit(false.B)
  val cgraBusy     = RegInit(false.B)
  val completeCount = RegInit(0.U(16.W))

  // Packet assembly registers
  val pktValid = RegInit(false.B)
  val pktData  = RegInit(0.U(params.intraPktWidth.W))

  // Response registers
  val respValid = RegInit(false.B)
  val respData  = RegInit(0.U(xLen.W))
  val respRd    = Reg(chiselTypeOf(io.resp.bits.rd))

  // ---- IntraCgraPkt Field Layout ----
  // From PyMTL3 generated struct (MSB first in packed struct):
  //   src[2:0]         — bits [181:179]
  //   dst[2:0]         — bits [178:176]
  //   src_cgra_id[1:0] — bits [175:174]
  //   dst_cgra_id[1:0] — bits [173:172]
  //   src_cgra_x[1:0]  — bits [171:170]
  //   src_cgra_y[0:0]  — bits [169:169]
  //   dst_cgra_x[1:0]  — bits [168:167]
  //   dst_cgra_y[0:0]  — bits [166:166]
  //   opaque[7:0]      — bits [165:158]
  //   vc_id[0:0]       — bits [157:157]
  //   payload[156:0]   — bits [156:0]
  //
  // Payload layout (CgraPayload, 157 bits):
  //   cmd[4:0]         — bits [156:152]
  //   data[34:0]       — bits [151:117]
  //   data_addr[6:0]   — bits [116:110]
  //   ctrl[106:0]      — bits [109:3]
  //   ctrl_addr[2:0]   — bits [2:0]

  def mkIntraPkt(src: UInt, dst: UInt, cmd: UInt, data: UInt = 0.U,
                 dataAddr: UInt = 0.U, ctrl: UInt = 0.U, ctrlAddr: UInt = 0.U): UInt = {
    val srcCgraId = 0.U(2.W)
    val dstCgraId = 0.U(2.W)
    val srcCgraX  = 0.U(2.W)
    val srcCgraY  = 0.U(1.W)
    val dstCgraX  = 0.U(2.W)
    val dstCgraY  = 0.U(1.W)
    val opaque    = 0.U(8.W)
    val vcId      = 0.U(1.W)

    // Build payload (157 bits)
    val payload = Cat(cmd(4, 0), data(34, 0), dataAddr(6, 0), ctrl(106, 0), ctrlAddr(2, 0))

    // Build full packet (182 bits)
    Cat(src(2, 0), dst(2, 0), srcCgraId, dstCgraId, srcCgraX, srcCgraY,
        dstCgraX, dstCgraY, opaque, vcId, payload)
  }

  // Build DataType from a 32-bit value: {payload[31:0], predicate=1, bypass=0, delay=0}
  def mkDataType(value: UInt): UInt = {
    Cat(value(31, 0), 1.U(1.W), 0.U(1.W), 0.U(1.W))
  }

  // ---- Packet Construction Logic ----
  when (state === s_idle && cmd.valid) {
    respRd := cmd.bits.inst.rd

    when (isConfig) {
      // CONFIG: rs1 = tile_id, rs2 = config_word (low 64 bits of ctrl, zero-extended)
      val tileId = rs1(2, 0)
      pktData := mkIntraPkt(
        src = 0.U, dst = tileId,
        cmd = CGRACmd.CMD_CONFIG,
        ctrl = Cat(0.U(43.W), rs2), ctrlAddr = 0.U
      )
      pktValid := true.B
      state := s_send_pkt
    } .elsewhen (isLaunch) {
      pktData := mkIntraPkt(
        src = 0.U, dst = 0.U,
        cmd = CGRACmd.CMD_LAUNCH
      )
      pktValid := true.B
      cgraBusy := true.B
      cgraComplete := false.B
      completeCount := 0.U
      state := s_send_pkt
    } .elsewhen (isStatus) {
      respData := Cat(0.U((xLen - 17).W), completeCount, cgraComplete)
      respValid := true.B
      state := s_resp
    } .elsewhen (isLoopCfg) {
      pktData := mkIntraPkt(
        src = 0.U, dst = 0.U,
        cmd = CGRACmd.CMD_CONFIG_COUNT_PER_ITER,
        data = mkDataType(rs1)
      )
      pktValid := true.B
      state := s_send_pkt
    } .elsewhen (isWait) {
      when (!cgraBusy) {
        respData := 1.U
        respValid := true.B
        state := s_resp
      } .otherwise {
        state := s_wait_complete
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
    val recvCmd = recvPkt(156, 152)
    when (recvCmd === CGRACmd.CMD_COMPLETE) {
      completeCount := completeCount + 1.U
      when (completeCount + 1.U >= params.numTiles.U) {
        cgraComplete := true.B
        cgraBusy := false.B
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
