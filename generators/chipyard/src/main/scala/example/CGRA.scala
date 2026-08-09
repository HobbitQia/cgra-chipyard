package chipyard.example

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

// ============================================================================
// CGRA Parameters
// ============================================================================

case class CGRAPacketLayout(
  cmdLsb: Int = 0,
  dataPayloadLsb: Int = 0,
  dataPredicateLsb: Int = 0,
  dataAddrLsb: Int = 0,
  opaqueLsb: Int = 0,
  dstTileLsb: Int = 0
)

case class CGRADMAParams(
  enabled: Boolean = false,
  dramAddrWidth: Int = 0,
  dramDataWidth: Int = 0,
  dramMaskWidth: Int = 0,
  spmAddrWidth: Int = 0,
  nbytesWidth: Int = 0,
  tagWidth: Int = 0,
  spmWords: Int = 0,
  writeReqAddrLsb: Int = 0,
  writeReqDataLsb: Int = 0,
  writeReqMaskLsb: Int = 0,
  descriptorSpmLsb: Int = 0,
  descriptorNbytesLsb: Int = 0,
  descriptorTagLsb: Int = 0,
  descriptorWidth: Int = 0,
  cmdConfigDramAddrLo: Int = 0,
  cmdConfigDramAddrHi: Int = 0,
  cmdConfigSpmAddr: Int = 0,
  cmdConfigBytes: Int = 0,
  cmdConfigTag: Int = 0,
  cmdMvin: Int = 0,
  cmdMvout: Int = 0,
  cmdDone: Int = 0,
  packetTemplates: Seq[BigInt] = Seq.empty
)

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
  // Generated packet and DMA protocol layouts
  packetLayout: CGRAPacketLayout = CGRAPacketLayout(),
  dma: CGRADMAParams = CGRADMAParams(),
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

    // Integrated CGRA DMA external-memory interface. The generator requires
    // the full group; no individual signal is optional within a DMA build.
    val send_to_dram_rd_req_val =
      if (params.dma.enabled) Some(Output(Bool())) else None
    val send_to_dram_rd_req_addr =
      if (params.dma.enabled) Some(Output(UInt(params.dma.dramAddrWidth.W))) else None
    val send_to_dram_rd_req_rdy =
      if (params.dma.enabled) Some(Input(Bool())) else None

    val recv_from_dram_rd_resp_val =
      if (params.dma.enabled) Some(Input(Bool())) else None
    val recv_from_dram_rd_resp_data =
      if (params.dma.enabled) Some(Input(UInt(params.dma.dramDataWidth.W))) else None
    val recv_from_dram_rd_resp_rdy =
      if (params.dma.enabled) Some(Output(Bool())) else None

    val send_to_dram_wr_req_val =
      if (params.dma.enabled) Some(Output(Bool())) else None
    val send_to_dram_wr_req_addr =
      if (params.dma.enabled) Some(Output(UInt(params.dma.dramAddrWidth.W))) else None
    val send_to_dram_wr_req_data =
      if (params.dma.enabled) Some(Output(UInt(params.dma.dramDataWidth.W))) else None
    val send_to_dram_wr_req_mask =
      if (params.dma.enabled) Some(Output(UInt(params.dma.dramMaskWidth.W))) else None
    val send_to_dram_wr_req_rdy =
      if (params.dma.enabled) Some(Input(Bool())) else None

    val recv_from_dram_wr_resp_val =
      if (params.dma.enabled) Some(Input(Bool())) else None
    val recv_from_dram_wr_resp_msg =
      if (params.dma.enabled) Some(Input(Bool())) else None
    val recv_from_dram_wr_resp_rdy =
      if (params.dma.enabled) Some(Output(Bool())) else None

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

// ============================================================================
// Dedicated physical-address-only TileLink DMA master
// ============================================================================

class CGRADmaWriteRequest(params: CGRAParams) extends Bundle {
  val address = UInt(params.dma.dramAddrWidth.W)
  val data = UInt(params.dma.dramDataWidth.W)
  val mask = UInt(params.dma.dramMaskWidth.W)
}

class CGRATileLinkDmaAdapterIO(params: CGRAParams) extends Bundle {
  val readReq = Flipped(Decoupled(UInt(params.dma.dramAddrWidth.W)))
  val readResp = Decoupled(UInt(params.dma.dramDataWidth.W))
  val writeReq = Flipped(Decoupled(new CGRADmaWriteRequest(params)))
  val writeResp = Decoupled(Bool())
  val busy = Output(Bool())
}

class CGRATileLinkDmaAdapter(params: CGRAParams)(implicit p: Parameters)
    extends LazyModule {
  val node = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLClientParameters(
    name = "cgra-dma",
    sourceId = IdRange(0, 1),
    requestFifo = true)))))

  override lazy val module = new CGRATileLinkDmaAdapterImp(this, params)
}

class CGRATileLinkDmaAdapterImp(
    outer: CGRATileLinkDmaAdapter,
    params: CGRAParams)(implicit p: Parameters)
    extends LazyModuleImp(outer) {
    val io = IO(new CGRATileLinkDmaAdapterIO(params))
    val (tl, edge) = outer.node.out(0)

    val negotiatedDataWidth = tl.a.bits.data.getWidth
    require(negotiatedDataWidth == params.dma.dramDataWidth &&
            negotiatedDataWidth == 128,
      s"CGRA DMA requires a negotiated 128-bit TileLink beat, got $negotiatedDataWidth bits")

    val addressWidth = tl.a.bits.address.getWidth
    val beatBytes = params.dma.dramDataWidth / 8
    val lgBeatBytes = log2Ceil(beatBytes)
    val fullMask = Fill(params.dma.dramMaskWidth, 1.U(1.W))

    val idle :: sendA :: waitD :: holdRead :: holdWrite :: Nil = Enum(5)
    val state = RegInit(idle)
    val requestIsWrite = RegInit(false.B)
    val requestAddress = Reg(UInt(params.dma.dramAddrWidth.W))
    val requestData = Reg(UInt(params.dma.dramDataWidth.W))
    val requestMask = Reg(UInt(params.dma.dramMaskWidth.W))
    val readResponseData = Reg(UInt(params.dma.dramDataWidth.W))

    io.readReq.ready := state === idle && !io.writeReq.valid
    io.writeReq.ready := state === idle && !io.readReq.valid
    io.readResp.valid := state === holdRead
    io.readResp.bits := readResponseData
    io.writeResp.valid := state === holdWrite
    io.writeResp.bits := false.B
    io.busy := state =/= idle

    when (io.readReq.valid && io.writeReq.valid) {
      assert(false.B,
        "CGRA DMA asserted read and write DRAM requests simultaneously")
    }

    when (io.readReq.fire) {
      assert(io.readReq.bits(lgBeatBytes - 1, 0) === 0.U,
        "CGRA DMA read address must be 16-byte aligned")
      if (params.dma.dramAddrWidth > addressWidth) {
        assert(!io.readReq.bits(params.dma.dramAddrWidth - 1, addressWidth).orR,
          "CGRA DMA read physical address exceeds TileLink address width")
      }
      requestIsWrite := false.B
      requestAddress := io.readReq.bits
      state := sendA
    }

    when (io.writeReq.fire) {
      assert(io.writeReq.bits.address(lgBeatBytes - 1, 0) === 0.U,
        "CGRA DMA write address must be 16-byte aligned")
      assert(io.writeReq.bits.mask.orR,
        "CGRA DMA write byte mask must contain at least one enabled byte")
      if (params.dma.dramAddrWidth > addressWidth) {
        assert(!io.writeReq.bits.address(
          params.dma.dramAddrWidth - 1, addressWidth).orR,
          "CGRA DMA write physical address exceeds TileLink address width")
      }
      requestIsWrite := true.B
      requestAddress := io.writeReq.bits.address
      requestData := io.writeReq.bits.data
      requestMask := io.writeReq.bits.mask
      state := sendA
    }

    val tlAddress = requestAddress(addressWidth - 1, 0)
    val (getLegal, get) = edge.Get(
      fromSource = 0.U,
      toAddress = tlAddress,
      lgSize = lgBeatBytes.U)
    val (putFullLegal, putFull) = edge.Put(
      fromSource = 0.U,
      toAddress = tlAddress,
      lgSize = lgBeatBytes.U,
      data = requestData)
    val (putPartialLegal, putPartial) = edge.Put(
      fromSource = 0.U,
      toAddress = tlAddress,
      lgSize = lgBeatBytes.U,
      data = requestData,
      mask = requestMask)
    val writeIsFull = requestMask === fullMask
    val selectedWrite = Mux(writeIsFull, putFull, putPartial)
    val requestLegal = Mux(requestIsWrite,
      Mux(writeIsFull, putFullLegal, putPartialLegal), getLegal)

    tl.a.valid := state === sendA
    tl.a.bits := Mux(requestIsWrite, selectedWrite, get)
    when (tl.a.valid) {
      assert(requestLegal,
        "CGRA DMA request is unsupported by the selected TileLink manager")
    }
    when (tl.a.fire) {
      state := waitD
    }

    tl.d.ready := state === waitD
    when (tl.d.fire) {
      val expectedOpcode = Mux(
        requestIsWrite, TLMessages.AccessAck, TLMessages.AccessAckData)
      val responseOk = tl.d.bits.source === 0.U &&
        tl.d.bits.opcode === expectedOpcode &&
        !tl.d.bits.denied && !tl.d.bits.corrupt && edge.done(tl.d)
      assert(tl.d.bits.source === 0.U,
        "CGRA DMA TileLink D response source mismatch")
      assert(tl.d.bits.opcode === expectedOpcode,
        "CGRA DMA TileLink D response opcode mismatch")
      assert(!tl.d.bits.denied,
        "CGRA DMA TileLink D response was denied")
      assert(!tl.d.bits.corrupt,
        "CGRA DMA TileLink D response was corrupt")
      assert(edge.done(tl.d),
        "CGRA DMA TileLink D response did not end the transaction")
      when (responseOk) {
        when (requestIsWrite) {
          state := holdWrite
        } .otherwise {
          readResponseData := tl.d.bits.data
          state := holdRead
        }
      }
    }

    when (io.readResp.fire || io.writeResp.fire) {
      state := idle
    }

    // This is a TL-UL client: it never probes, releases, or grants ownership.
    tl.b.ready := true.B
    tl.c.valid := false.B
    tl.c.bits := DontCare
    tl.e.valid := false.B
    tl.e.bits := DontCare
    when (tl.b.valid) {
      assert(false.B, "CGRA DMA received an unexpected TileLink B probe")
    }
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
//   11 = LOAD_RESULT: Block until a CMD_LOAD_RESPONSE arrives, then return data
//
// ============================================================================

class CGRAAccelerator(opcodes: OpcodeSet, params: CGRAParams = CGRAGenerated.params)(implicit p: Parameters)
    extends LazyRoCC(opcodes) {
  val dmaAdapter = LazyModule(new CGRATileLinkDmaAdapter(params))
  override val tlNode: TLNode = dmaAdapter.node
  override lazy val module = new CGRAAcceleratorImp(this, params)
}

class CGRAAcceleratorImp(outer: CGRAAccelerator, params: CGRAParams)(implicit p: Parameters)
    extends LazyRoCCModuleImp(outer) with HasCoreParameters {

  require(params.dma.enabled, "default single-CGRA SoC requires generated DMA ports")
  require(params.dma.packetTemplates.length == 7,
    s"DMA protocol requires seven generated templates (five config plus MVIN/MVOUT), got ${params.dma.packetTemplates.length}")
  require(params.dma.descriptorWidth <= xLen,
    s"DMA descriptor width ${params.dma.descriptorWidth} exceeds xLen=$xLen")
  require(params.dma.dramAddrWidth == xLen,
    s"DMA DRAM address width ${params.dma.dramAddrWidth} must equal xLen=$xLen")
  require(params.dma.descriptorSpmLsb == 0,
    "DMA descriptor must start with the SPM word address")
  require(params.dma.descriptorNbytesLsb == params.dma.spmAddrWidth,
    "DMA descriptor nbytes offset does not follow SPM address")
  require(params.dma.descriptorTagLsb == params.dma.spmAddrWidth + params.dma.nbytesWidth,
    "DMA descriptor tag offset does not follow nbytes")
  require(params.dma.dramAddrWidth == 2 * params.dataPayloadWidth,
    "six-packet DMA protocol requires two packet payloads for the DRAM address")
  require(params.dma.nbytesWidth <= params.dataPayloadWidth,
    "DMA nbytes does not fit the generated packet data payload")
  require(params.dma.tagWidth <= params.dataPayloadWidth,
    "DMA tag does not fit the generated packet data payload")
  require(params.dataPayloadWidth % 8 == 0,
    "CGRA data payload must contain a whole number of bytes")
  require(params.dma.dramDataWidth % params.dataPayloadWidth == 0,
    "DMA DRAM beat must contain a whole number of CGRA words")
  require(params.dma.dramMaskWidth == params.dma.dramDataWidth / 8,
    "DMA mask must contain one bit per DRAM byte")
  require(params.dma.dramDataWidth == 128,
    "Phase-1 CGRA DMA requires a 128-bit DRAM interface")

  // ---- CGRA BlackBox instantiation ----
  val cgra = Module(new CGRABlackBox(params))

  // Clock and reset
  cgra.io.clk   := clock
  cgra.io.reset := reset.asBool

  // Static configuration
  cgra.io.cgra_id       := 0.U  // Single CGRA, ID = 0
  cgra.io.address_lower := params.addressLower.U
  cgra.io.address_upper := params.addressUpper.U

  val dmaRdReqVal = cgra.io.send_to_dram_rd_req_val.get
  val dmaRdReqAddr = cgra.io.send_to_dram_rd_req_addr.get
  val dmaRdReqRdy = cgra.io.send_to_dram_rd_req_rdy.get
  val dmaRdRespVal = cgra.io.recv_from_dram_rd_resp_val.get
  val dmaRdRespData = cgra.io.recv_from_dram_rd_resp_data.get
  val dmaRdRespRdy = cgra.io.recv_from_dram_rd_resp_rdy.get
  val dmaWrReqVal = cgra.io.send_to_dram_wr_req_val.get
  val dmaWrReqAddr = cgra.io.send_to_dram_wr_req_addr.get
  val dmaWrReqData = cgra.io.send_to_dram_wr_req_data.get
  val dmaWrReqMask = cgra.io.send_to_dram_wr_req_mask.get
  val dmaWrReqRdy = cgra.io.send_to_dram_wr_req_rdy.get
  val dmaWrRespVal = cgra.io.recv_from_dram_wr_resp_val.get
  val dmaWrRespMsg = cgra.io.recv_from_dram_wr_resp_msg.get
  val dmaWrRespRdy = cgra.io.recv_from_dram_wr_resp_rdy.get
  val dmaAdapter = outer.dmaAdapter.module.io

  dmaAdapter.readReq.valid := dmaRdReqVal
  dmaAdapter.readReq.bits := dmaRdReqAddr
  dmaRdReqRdy := dmaAdapter.readReq.ready
  dmaRdRespVal := dmaAdapter.readResp.valid
  dmaRdRespData := dmaAdapter.readResp.bits
  dmaAdapter.readResp.ready := dmaRdRespRdy

  dmaAdapter.writeReq.valid := dmaWrReqVal
  dmaAdapter.writeReq.bits.address := dmaWrReqAddr
  dmaAdapter.writeReq.bits.data := dmaWrReqData
  dmaAdapter.writeReq.bits.mask := dmaWrReqMask
  dmaWrReqRdy := dmaAdapter.writeReq.ready
  dmaWrRespVal := dmaAdapter.writeResp.valid
  dmaWrRespMsg := dmaAdapter.writeResp.bits
  dmaAdapter.writeResp.ready := dmaWrRespRdy

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
  val cmd = Queue(io.cmd, entries = 2)

  val funct = cmd.bits.inst.funct
  val rs1   = cmd.bits.rs1
  val rs2   = cmd.bits.rs2

  // Funct7 command encoding
  val isStatus    = funct === CGRARoCCGenerated.STATUS.U
  val isWait      = funct === CGRARoCCGenerated.WAIT.U
  val isRawPktLo  = funct === CGRARoCCGenerated.RAW_PKT_LO.U
  val isRawPktMid = funct === CGRARoCCGenerated.RAW_PKT_MID.U
  val isRawPktHi  = funct === CGRARoCCGenerated.RAW_PKT_HI.U
  val isSetExpectedCompletes = funct === CGRARoCCGenerated.SET_EXPECTED_COMPLETES.U
  val isResult    = funct === CGRARoCCGenerated.RESULT.U
  val isRawPktTop = funct === CGRARoCCGenerated.RAW_PKT_TOP.U
  val isLoadResult = funct === CGRARoCCGenerated.LOAD_RESULT.U
  val isDmaMvin = funct === CGRARoCCGenerated.DMA_MVIN_ASYNC.U
  val isDmaMvout = funct === CGRARoCCGenerated.DMA_MVOUT_ASYNC.U
  val isDmaIssue = isDmaMvin || isDmaMvout
  val isDmaWait = funct === CGRARoCCGenerated.DMA_WAIT.U

  // ---- State Machine ----
  val s_idle :: s_wait_complete :: s_wait_load_response :: s_wait_dma :: s_resp :: Nil = Enum(5)
  val state = RegInit(s_idle)

  // Status registers
  val cgraComplete = RegInit(false.B)
  val cgraBusy     = RegInit(false.B)
  val completeCount = RegInit(0.U(16.W))
  val expectedCompleteCount = RegInit(0.U(16.W))

  // Packet assembly registers
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

  // CPU-triggered readback uses CMD_LOAD_REQUEST and returns CMD_LOAD_RESPONSE
  // through the same send_to_cpu_pkt channel as CMD_COMPLETE.
  val expectLoadResponse = RegInit(false.B)
  val loadRespValid = RegInit(false.B)
  val lastLoadData = RegInit(0.U(params.dataPayloadWidth.W))

  // Response registers
  val respValid = RegInit(false.B)
  val respData  = RegInit(0.U(xLen.W))
  val respRd    = Reg(chiselTypeOf(io.resp.bits.rd))

  // All packet field locations come from the generated PyMTL typedef layout.
  val pktCmdLsb = params.packetLayout.cmdLsb
  val pktCmdMsb = pktCmdLsb + params.cmdWidth - 1
  val pktDataPayloadLsb = params.packetLayout.dataPayloadLsb
  val pktDataPayloadMsb = pktDataPayloadLsb + params.dataPayloadWidth - 1
  val pktOpaqueLsb = params.packetLayout.opaqueLsb
  val pktOpaqueMsb = pktOpaqueLsb + params.dma.tagWidth - 1
  val pktTileIdWidth = log2Ceil(params.numTiles + 1)
  val pktDstTileLsb = params.packetLayout.dstTileLsb
  val pktDstTileMsb = pktDstTileLsb + pktTileIdWidth - 1
  val cpuTileId = params.numTiles.U(pktTileIdWidth.W)
  require(params.payloadWidth <= params.intraPktWidth,
    s"payloadWidth ${params.payloadWidth} exceeds intraPktWidth ${params.intraPktWidth}")
  require(pktCmdMsb < params.intraPktWidth && pktDataPayloadMsb < params.intraPktWidth,
    "generated command/data payload fields exceed the packet width")
  require(pktOpaqueMsb < params.intraPktWidth && pktDstTileMsb < params.intraPktWidth,
    "generated opaque/destination fields exceed the packet width")

  val packetFifoEntries = 8
  val packetFifo = Module(new Queue(UInt(params.intraPktWidth.W), entries = packetFifoEntries))
  packetFifo.io.enq.valid := false.B
  packetFifo.io.enq.bits := 0.U

  cgra.io.recv_from_cpu_pkt_val := packetFifo.io.deq.valid
  cgra.io.recv_from_cpu_pkt_msg := packetFifo.io.deq.bits
  packetFifo.io.deq.ready := cgra.io.recv_from_cpu_pkt_rdy

  val packetFifoEmpty = !packetFifo.io.deq.valid
  val completesPacket = if (needsRawPktTop) isRawPktTop else isRawPktHi

  def acceptAssembledPkt(assembledPkt: UInt): Unit = {
    val assembledCmd = assembledPkt(pktCmdMsb, pktCmdLsb)
    packetFifo.io.enq.valid := true.B
    packetFifo.io.enq.bits := assembledPkt
    when (assembledCmd === CGRACmdGenerated.CMD_LAUNCH.U(params.cmdWidth.W) ||
          assembledCmd === CGRACmdGenerated.CMD_RESUME.U(params.cmdWidth.W)) {
      noteLaunchIssued()
    }
    when (assembledCmd === CGRACmdGenerated.CMD_LOAD_REQUEST.U(params.cmdWidth.W)) {
      loadRespValid := false.B
      expectLoadResponse := true.B
    }
  }

  def noteLaunchIssued(): Unit = {
    when (expectedCompleteCount === 0.U && cgraComplete) {
      completeCount := 0.U
    }
    cgraComplete := false.B
  }

  // ---- Semantic DMA command packet sequencer ----
  val dmaSeqActive = RegInit(false.B)
  val dmaSeqPhase = RegInit(0.U(3.W))
  val dmaSeqDramAddr = Reg(UInt(params.dma.dramAddrWidth.W))
  val dmaSeqDescriptor = Reg(UInt(params.dma.descriptorWidth.W))
  val dmaSeqIsMvin = RegInit(false.B)
  val dmaInFlight = RegInit(false.B)
  val dmaDoneValid = RegInit(false.B)
  val dmaDoneTag = Reg(UInt(params.dma.tagWidth.W))
  val dmaWaitExpectedTag = Reg(UInt(params.dma.tagWidth.W))

  val issueSpmAddr = rs2(
    params.dma.descriptorSpmLsb + params.dma.spmAddrWidth - 1,
    params.dma.descriptorSpmLsb)
  val issueNbytes = rs2(
    params.dma.descriptorNbytesLsb + params.dma.nbytesWidth - 1,
    params.dma.descriptorNbytesLsb)
  val issueTag = rs2(
    params.dma.descriptorTagLsb + params.dma.tagWidth - 1,
    params.dma.descriptorTagLsb)
  val cgraWordBytes = params.dataPayloadWidth / 8
  val cgraWordByteShift = log2Ceil(cgraWordBytes)
  val dmaBeatBytes = params.dma.dramDataWidth / 8
  val dmaBeatByteShift = log2Ceil(dmaBeatBytes)
  require((1 << cgraWordByteShift) == cgraWordBytes,
    s"CGRA word bytes must be a power of two, got $cgraWordBytes")
  require((1 << dmaBeatByteShift) == dmaBeatBytes,
    s"DMA beat bytes must be a power of two, got $dmaBeatBytes")

  val dmaPacketTemplates = VecInit(
    params.dma.packetTemplates.map(_.U(params.intraPktWidth.W)))
  val dmaSeqPacket = Wire(UInt(params.intraPktWidth.W))
  dmaSeqPacket := 0.U
  switch (dmaSeqPhase) {
    is (0.U) {
      dmaSeqPacket := dmaPacketTemplates(0) |
        (dmaSeqDramAddr(params.dataPayloadWidth - 1, 0) << params.packetLayout.dataPayloadLsb)
    }
    is (1.U) {
      dmaSeqPacket := dmaPacketTemplates(1) |
        (dmaSeqDramAddr(params.dma.dramAddrWidth - 1, params.dataPayloadWidth) <<
          params.packetLayout.dataPayloadLsb)
    }
    is (2.U) {
      dmaSeqPacket := dmaPacketTemplates(2) |
        (dmaSeqDescriptor(params.dma.spmAddrWidth - 1, 0) << params.packetLayout.dataAddrLsb)
    }
    is (3.U) {
      dmaSeqPacket := dmaPacketTemplates(3) |
        (dmaSeqDescriptor(
          params.dma.descriptorNbytesLsb + params.dma.nbytesWidth - 1,
          params.dma.descriptorNbytesLsb) << params.packetLayout.dataPayloadLsb)
    }
    is (4.U) {
      dmaSeqPacket := dmaPacketTemplates(4) |
        (dmaSeqDescriptor(
          params.dma.descriptorTagLsb + params.dma.tagWidth - 1,
          params.dma.descriptorTagLsb) << params.packetLayout.dataPayloadLsb)
    }
    is (5.U) {
      dmaSeqPacket := Mux(dmaSeqIsMvin, dmaPacketTemplates(5), dmaPacketTemplates(6))
    }
  }

  when (dmaSeqActive) {
    packetFifo.io.enq.valid := true.B
    packetFifo.io.enq.bits := dmaSeqPacket
  }

  when (dmaSeqActive && packetFifo.io.enq.fire) {
    when (dmaSeqPhase === 5.U) {
      dmaSeqActive := false.B
      dmaSeqPhase := 0.U
    } .otherwise {
      dmaSeqPhase := dmaSeqPhase + 1.U
    }
  }

  // ---- Packet Construction Logic ----
  when (state === s_idle && cmd.fire) {
    respRd := cmd.bits.inst.rd

    when (isDmaIssue) {
      val issueWords = issueNbytes >> cgraWordByteShift
      val issueSpmEnd = issueSpmAddr +& issueWords
      val issueDramEnd = rs1 +& issueNbytes
      assert(!dmaInFlight && !dmaDoneValid && !dmaSeqActive,
        "only one DMA command may be outstanding")
      assert(!dmaAdapter.busy,
        "new DMA command issued while the TileLink adapter is active")
      if (params.dma.descriptorWidth < xLen) {
        assert(!rs2(xLen - 1, params.dma.descriptorWidth).orR,
          "DMA descriptor has nonzero bits outside the generated layout")
      }
      assert(issueNbytes =/= 0.U, "DMA byte count must be nonzero")
      assert(issueNbytes(dmaBeatByteShift - 1, 0) === 0.U,
        "DMA byte count must be a multiple of 16 bytes")
      assert(issueSpmEnd <= params.dma.spmWords.U,
        "DMA descriptor exceeds the software-visible SPM range")
      assert(rs1(dmaBeatByteShift - 1, 0) === 0.U,
        "DMA DRAM address must be 16-byte aligned")
      assert(!issueDramEnd(xLen), "DMA address plus length overflows xLen")

      dmaSeqDramAddr := rs1
      dmaSeqDescriptor := rs2(params.dma.descriptorWidth - 1, 0)
      dmaSeqIsMvin := isDmaMvin
      dmaSeqPhase := 0.U
      dmaSeqActive := true.B
      dmaInFlight := true.B
    } .elsewhen (isDmaWait) {
      assert(dmaInFlight || dmaDoneValid,
        "DMA_WAIT issued without an in-flight or completed DMA command")
      if (params.dma.tagWidth < xLen) {
        assert(!rs1(xLen - 1, params.dma.tagWidth).orR,
          "DMA_WAIT expected tag exceeds the generated tag width")
      }
      dmaWaitExpectedTag := rs1(params.dma.tagWidth - 1, 0)
      when (dmaDoneValid) {
        assert(dmaDoneTag === rs1(params.dma.tagWidth - 1, 0),
          "DMA_WAIT observed a completion tag mismatch")
        respData := dmaDoneTag
        dmaDoneValid := false.B
        respValid := true.B
        state := s_resp
      } .otherwise {
        state := s_wait_dma
      }
    } .elsewhen (isStatus) {
      respData := Cat(0.U((xLen - 17).W), completeCount, cgraComplete)
      respValid := true.B
      state := s_resp
    } .elsewhen (isResult) {
      respData := lastCompleteData
      respValid := true.B
      state := s_resp
    } .elsewhen (isLoadResult) {
      when (loadRespValid) {
        respData := lastLoadData
        loadRespValid := false.B
        respValid := true.B
        state := s_resp
      } .otherwise {
        state := s_wait_load_response
      }
    } .elsewhen (isWait) {
      when (packetFifoEmpty && (expectedCompleteCount === 0.U || cgraComplete)) {
        respData := 1.U
        respValid := true.B
        state := s_resp
      } .otherwise {
        cgraBusy := true.B
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

  // ---- Wait for completion ----
  when (state === s_wait_complete &&
        packetFifoEmpty &&
        (expectedCompleteCount === 0.U || cgraComplete)) {
    respData := 1.U
    respValid := true.B
    cgraBusy := false.B
    state := s_resp
  }

  when (state === s_wait_load_response && loadRespValid) {
    respData := lastLoadData
    loadRespValid := false.B
    respValid := true.B
    state := s_resp
  }

  when (state === s_wait_dma && dmaDoneValid) {
    assert(dmaDoneTag === dmaWaitExpectedTag,
      "DMA_WAIT observed a completion tag mismatch")
    respData := dmaDoneTag
    dmaDoneValid := false.B
    respValid := true.B
    state := s_resp
  }

  // ---- Monitor CGRA output (send_to_cpu_pkt) ----
  cgra.io.send_to_cpu_pkt_rdy := true.B  // Always ready to receive from CGRA

  when (cgra.io.send_to_cpu_pkt_val) {
    val recvPkt = cgra.io.send_to_cpu_pkt_msg
    val recvCmd = recvPkt(pktCmdMsb, pktCmdLsb)
    val recvDstTile = recvPkt(pktDstTileMsb, pktDstTileLsb)
    when (recvCmd === CGRACmdGenerated.CMD_DMA_DONE.U(params.cmdWidth.W)) {
      val payloadTag = recvPkt(pktDataPayloadLsb + params.dma.tagWidth - 1,
                               pktDataPayloadLsb)
      val opaqueTag = recvPkt(pktOpaqueMsb, pktOpaqueLsb)
      assert(dmaInFlight, "CMD_DMA_DONE observed without an in-flight DMA")
      assert(!dmaDoneValid, "CMD_DMA_DONE would overwrite an unconsumed completion")
      assert(payloadTag === opaqueTag,
        "CMD_DMA_DONE opaque and payload tags differ")
      if (params.dataPayloadWidth > params.dma.tagWidth) {
        assert(!recvPkt(pktDataPayloadMsb,
                        pktDataPayloadLsb + params.dma.tagWidth).orR,
          "CMD_DMA_DONE payload contains non-tag bits")
      }
      dmaDoneTag := payloadTag
      dmaDoneValid := true.B
      dmaInFlight := false.B
    } .elsewhen (recvCmd === CGRACmdGenerated.CMD_COMPLETE.U(params.cmdWidth.W)) {
      when (expectedCompleteCount =/= 0.U) {
        lastCompleteData := recvPkt(pktDataPayloadMsb, pktDataPayloadLsb)
        completeCount := completeCount + 1.U
        when (completeCount + 1.U >= expectedCompleteCount) {
          cgraComplete := true.B
          cgraBusy := false.B
          expectedCompleteCount := 0.U
        }
      }
    } .elsewhen (recvCmd === CGRACmdGenerated.CMD_LOAD_RESPONSE.U(params.cmdWidth.W) &&
                 recvDstTile === cpuTileId && expectLoadResponse) {
      lastLoadData := recvPkt(pktDataPayloadMsb, pktDataPayloadLsb)
      loadRespValid := true.B
      expectLoadResponse := false.B
    }
  }

  // ---- RoCC Command Ready ----
  val dmaIssueReady = !dmaInFlight && !dmaDoneValid && !dmaSeqActive &&
                      !dmaAdapter.busy
  cmd.ready := (state === s_idle) && !respValid && !dmaSeqActive &&
               (!completesPacket || packetFifo.io.enq.ready) &&
               (!isDmaIssue || dmaIssueReady)

  // ---- RoCC Response Interface ----
  io.resp.valid     := respValid
  io.resp.bits.rd   := respRd
  io.resp.bits.data := respData

  when (io.resp.fire) {
    respValid := false.B
    state := s_idle
  }

  // ---- RoCC Busy / Interrupt ----
  io.busy := cmd.valid || cgraBusy || packetFifo.io.deq.valid ||
             dmaSeqActive || dmaInFlight || dmaAdapter.busy ||
             (state =/= s_idle)
  io.interrupt := false.B

  // LazyRoCC always exposes io.mem, but CGRA DMA never uses the DCache path.
  io.mem.req.valid := false.B
  io.mem.req.bits := 0.U.asTypeOf(io.mem.req.bits)
  io.mem.s1_kill := false.B
  io.mem.s1_data := 0.U.asTypeOf(io.mem.s1_data)
  io.mem.s2_kill := false.B
  io.mem.keep_clock_enabled := false.B
}
