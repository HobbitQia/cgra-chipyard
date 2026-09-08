package chipyard.socgen.cgra

import chisel3._
import chisel3.util._
import chipyard.example.{CGRADmaWriteRequest, CGRASpmReadIO, CGRASpmReadParams, CGRASpmWindowParams}
import chipyard.socgen.link.{AutoLinkStatus, AutoTile}

class CgraWritebackConfig extends Bundle {
  val enabled = Bool()
  val address = UInt(64.W)
  val word = UInt(32.W)
  val slotStride = UInt(32.W)
  val channels = UInt(32.W)
  val rowStride = UInt(32.W)
}

class CgraWritebackRequest(params: CgraLinkParams) extends Bundle {
  val config = new CgraWritebackConfig
  val tile = new AutoTile(params.auto.lengthWidth)
}

class CgraSpmReadArbiter(params: CGRASpmReadParams) extends Module {
  val io = IO(new Bundle {
    val clients = Vec(2, Flipped(new CGRASpmReadIO(params)))
    val spm = new CGRASpmReadIO(params)
  })

  val requests = Module(new RRArbiter(UInt(params.addrWidth.W), 2))
  val pending = RegInit(false.B)
  val owner = Reg(UInt(1.W))
  for (index <- 0 until 2) {
    requests.io.in(index) <> io.clients(index).req
    io.clients(index).resp.valid := pending && owner === index.U && io.spm.resp.valid
    io.clients(index).resp.bits := io.spm.resp.bits
  }
  io.spm.req.valid := requests.io.out.valid && !pending
  io.spm.req.bits := requests.io.out.bits
  requests.io.out.ready := io.spm.req.ready && !pending
  io.spm.resp.ready := pending && io.clients(owner).resp.ready
  io.spm.busy := pending || io.clients.map(_.busy).reduce(_ || _)

  when(io.spm.req.fire) {
    owner := requests.io.chosen
    pending := true.B
  }
  when(io.spm.resp.fire) { pending := false.B }
}

class CgraWriteback(params: CgraLinkParams, window: CGRASpmWindowParams) extends Module {
  private val cgra = params.cgra
  private val beatBytes = cgra.dma.dramDataWidth / 8
  private val beatShift = log2Ceil(beatBytes)
  private val elementBytes = if (window.quantize) 1 else cgra.spmRead.dataWidth / 8
  private val wordCountWidth = log2Ceil(cgra.spmRead.words + 1)
  private val addressWidth = cgra.dma.dramAddrWidth
  require(cgra.dma.enabled && cgra.spmRead.enabled)
  require(isPow2(beatBytes) && isPow2(elementBytes) && elementBytes <= beatBytes)
  require(cgra.dma.dramMaskWidth == beatBytes && addressWidth <= 64)
  require(cgra.spmRead.words > 0 && cgra.spmRead.dataWidth % 8 == 0)
  if (window.quantize) { require(cgra.spmRead.dataWidth == 32) }

  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new CgraWritebackRequest(params)))
    val done = Decoupled(UInt(AutoLinkStatus.Width.W))
    val spm = new CGRASpmReadIO(cgra.spmRead)
    val writeReq = Decoupled(new CGRADmaWriteRequest(cgra))
    val writeResp = Flipped(Decoupled(Bool()))
    val busy = Output(Bool())
  })

  val idle :: collect :: send :: drain :: report :: Nil = Enum(5)
  val state = RegInit(idle)
  val pendingRead = RegInit(false.B)
  val source = Reg(UInt(cgra.spmRead.addrWidth.W))
  val address = Reg(UInt(addressWidth.W))
  val nextRow = Reg(UInt(addressWidth.W))
  val rowStride = Reg(UInt(32.W))
  val rowWords = Reg(UInt(wordCountWidth.W))
  val wordsLeft = Reg(UInt(wordCountWidth.W))
  val rowsLeft = Reg(UInt(params.auto.lengthWidth.W))
  val data = RegInit(0.U(cgra.dma.dramDataWidth.W))
  val mask = RegInit(0.U(beatBytes.W))
  val beatAddress = Reg(UInt(addressWidth.W))
  val finalBeat = RegInit(false.B)
  val status = RegInit(AutoLinkStatus.Success)

  val config = io.request.bits.config
  val tile = io.request.bits.tile
  val slot = tile.id % params.auto.bufferSlots.U
  val firstWord = config.word +& (slot * config.slotStride)
  val requestedRowWords = tile.columns * config.channels
  val totalWords = tile.rows * requestedRowWords
  val sourceEnd = firstWord +& totalWords
  val rowEndBytes = (tile.column +& tile.columns) * config.channels * elementBytes.U
  val firstAddress = config.address +& (tile.row * config.rowStride) +&
    (tile.column * config.channels * elementBytes.U)
  val lastAddress = config.address +& ((tile.row +& tile.rows - 1.U) * config.rowStride) +& rowEndBytes - 1.U
  val valid = config.enabled && tile.rows =/= 0.U && tile.columns =/= 0.U &&
    config.channels =/= 0.U && sourceEnd <= cgra.spmRead.words.U &&
    rowEndBytes <= config.rowStride &&
    (config.address & (elementBytes - 1).U) === 0.U &&
    (config.rowStride & (elementBytes - 1).U) === 0.U &&
    lastAddress <= ((BigInt(1) << addressWidth) - 1).U

  io.request.ready := state === idle
  io.done.valid := state === report
  io.done.bits := status
  io.busy := state =/= idle
  io.spm.busy := io.busy
  io.spm.req.valid := state === collect && !pendingRead
  io.spm.req.bits := source
  io.spm.resp.ready := state === collect && pendingRead
  io.writeReq.valid := state === send
  io.writeReq.bits.address := beatAddress
  io.writeReq.bits.data := data
  io.writeReq.bits.mask := mask
  io.writeResp.ready := state === drain

  when(io.request.fire) {
    source := firstWord
    address := firstAddress
    nextRow := firstAddress + config.rowStride
    rowStride := config.rowStride
    rowWords := requestedRowWords
    wordsLeft := requestedRowWords
    rowsLeft := tile.rows
    data := 0.U
    mask := 0.U
    pendingRead := false.B
    status := Mux(valid, AutoLinkStatus.Success, AutoLinkStatus.ConfigFailure)
    state := Mux(valid, collect, report)
  }

  val lane = address(beatShift - 1, 0)
  val value = window.bridge.map(bridge => CgraRequant.byte(io.spm.resp.bits.asSInt, bridge.outboundScale))
    .getOrElse(io.spm.resp.bits)
  val elementMask = ((BigInt(1) << elementBytes) - 1).U(beatBytes.W)
  val endRow = wordsLeft === 1.U
  val endBeat = lane === (beatBytes - elementBytes).U

  when(io.spm.req.fire) { pendingRead := true.B }
  when(io.spm.resp.fire) {
    pendingRead := false.B
    source := source + 1.U
    data := data | (value.pad(cgra.dma.dramDataWidth) << (lane << 3))
    mask := mask | (elementMask << lane)
    beatAddress := (address >> beatShift) << beatShift
    wordsLeft := Mux(endRow, rowWords, wordsLeft - 1.U)
    address := Mux(endRow, nextRow, address + elementBytes.U)
    when(endRow) {
      rowsLeft := rowsLeft - 1.U
      nextRow := nextRow + rowStride
    }
    when(endRow || endBeat) {
      finalBeat := endRow && rowsLeft === 1.U
      state := send
    }
  }
  when(io.writeReq.fire) {
    data := 0.U
    mask := 0.U
    state := drain
  }
  when(io.writeResp.fire) {
    when(io.writeResp.bits) { status := AutoLinkStatus.SinkFailure }
    state := Mux(io.writeResp.bits || finalBeat, report, collect)
  }
  when(io.done.fire) { state := idle }
}
