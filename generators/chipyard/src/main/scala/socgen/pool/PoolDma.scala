package chipyard.socgen.pool

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.LazyModule

class PoolReadRequest(params: PoolParams) extends Bundle {
  val address = UInt(params.addressBits.W)
  val last = Bool()
}

class PoolReadIssue(params: PoolParams) extends Bundle {
  val address = UInt(params.addressBits.W)
  val source = UInt(params.sourceBits.W)
}

class PoolWriteBeat(params: PoolParams, beatBits: Int) extends Bundle {
  val address = UInt(params.addressBits.W)
  val data = UInt(beatBits.W)
  val mask = UInt((beatBits / 8).W)
  val last = Bool()
}

class PoolWriteIssue(params: PoolParams, beatBits: Int) extends Bundle {
  val address = UInt(params.addressBits.W)
  val data = UInt(beatBits.W)
  val mask = UInt((beatBits / 8).W)
  val source = UInt(params.sourceBits.W)
}

class PoolDmaResponse(params: PoolParams, beatBits: Int) extends Bundle {
  val source = UInt(params.sourceBits.W)
  val data = UInt(beatBits.W)
  val denied = Bool()
  val corrupt = Bool()
}

class PoolDmaIO(params: PoolParams, beatBits: Int) extends Bundle {
  val readRequest = Decoupled(new PoolReadIssue(params))
  val readResponse = Flipped(Decoupled(new PoolDmaResponse(params, beatBits)))
  val writeRequest = Decoupled(new PoolWriteIssue(params, beatBits))
  val writeResponse = Flipped(Decoupled(new PoolDmaResponse(params, beatBits)))
}

class PoolReadBeat(beatBits: Int) extends Bundle {
  val data = UInt(beatBits.W)
  val last = Bool()
}

class PoolReadPlan(params: PoolParams, beatBytes: Int) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val base = Input(UInt(params.addressBits.W))
    val beats = Input(UInt(params.addressBits.W))
    val cancel = Input(Bool())
    val output = Decoupled(new PoolReadRequest(params))
  })

  val active = RegInit(false.B)
  val address = Reg(UInt(params.addressBits.W))
  val remaining = Reg(UInt(params.addressBits.W))

  io.output.valid := active && !io.cancel
  io.output.bits.address := address
  io.output.bits.last := remaining === 1.U

  when(io.output.fire) {
    address := address + beatBytes.U
    remaining := remaining - 1.U
    when(remaining === 1.U) {
      active := false.B
    }
  }
  when(io.cancel) {
    active := false.B
  }
  when(io.start) {
    active := true.B
    address := io.base
    remaining := io.beats
  }
}

class PoolReadDma(params: PoolParams, beatBits: Int) extends Module {
  private val ptrBits = math.max(1, log2Ceil(params.maxInflight))

  val io = IO(new Bundle {
    val start = Input(Bool())
    val cancel = Input(Bool())
    val input = Flipped(Decoupled(new PoolReadRequest(params)))
    val request = Decoupled(new PoolReadIssue(params))
    val response = Flipped(Decoupled(new PoolDmaResponse(params, beatBits)))
    val output = Decoupled(new PoolReadBeat(beatBits))
    val error = Valid(new PoolEvent)
    val idle = Output(Bool())
  })

  val active = RegInit(false.B)
  val stopped = RegInit(false.B)
  val errorValid = RegInit(false.B)
  val errorStatus = RegInit(PoolStatus.Success)
  val issue = RegInit(0.U(ptrBits.W))
  val retire = RegInit(0.U(ptrBits.W))
  val outstanding = RegInit(0.U(log2Ceil(params.maxInflight + 1).W))
  val valid = RegInit(VecInit(Seq.fill(params.maxInflight)(false.B)))
  val returned = RegInit(VecInit(Seq.fill(params.maxInflight)(false.B)))
  val data = Reg(Vec(params.maxInflight, UInt(beatBits.W)))
  val last = Reg(Vec(params.maxInflight, Bool()))

  def next(pointer: UInt): UInt =
    Mux(pointer === (params.maxInflight - 1).U, 0.U, pointer + 1.U)

  val canIssue = active && !stopped && !io.cancel && !valid(issue)
  io.request.valid := io.input.valid && canIssue
  io.request.bits.address := io.input.bits.address
  io.request.bits.source := issue
  io.input.ready := io.request.ready && canIssue

  val responseIndex = io.response.bits.source(ptrBits - 1, 0)
  io.response.ready := active
  io.output.valid := active && !stopped && valid(retire) && returned(retire)
  io.output.bits.data := data(retire)
  io.output.bits.last := last(retire)
  io.error.valid := errorValid
  io.error.bits.status := errorStatus
  io.idle := !active

  val requestFire = io.request.fire
  val responseFire = io.response.fire
  val responseBypass = requestFire && responseIndex === issue
  val responseError = responseFire && (io.response.bits.denied || io.response.bits.corrupt)
  val drainsNow = responseFire && !requestFire && outstanding === 1.U

  when(requestFire) {
    valid(issue) := true.B
    returned(issue) := false.B
    last(issue) := io.input.bits.last
    issue := next(issue)
  }
  when(responseFire) {
    assert((valid(responseIndex) && !returned(responseIndex)) || responseBypass)
    returned(responseIndex) := true.B
    data(responseIndex) := io.response.bits.data
    when(responseError && !errorValid) {
      errorValid := true.B
      errorStatus := Mux(io.response.bits.denied, PoolStatus.Denied, PoolStatus.Corrupt)
    }
    when(responseError) {
      stopped := true.B
    }
  }
  when(io.output.fire) {
    valid(retire) := false.B
    returned(retire) := false.B
    retire := next(retire)
    when(last(retire)) {
      active := false.B
    }
  }

  switch(Cat(requestFire, responseFire)) {
    is("b10".U) { outstanding := outstanding + 1.U }
    is("b01".U) { outstanding := outstanding - 1.U }
  }

  when((stopped || io.cancel || responseError) &&
    (outstanding === 0.U || drainsNow)) {
    active := false.B
    valid.foreach(_ := false.B)
    returned.foreach(_ := false.B)
  }
  when(io.cancel) {
    stopped := true.B
  }
  when(io.start) {
    active := true.B
    stopped := false.B
    errorValid := false.B
    errorStatus := PoolStatus.Success
    issue := 0.U
    retire := 0.U
    outstanding := 0.U
    valid.foreach(_ := false.B)
    returned.foreach(_ := false.B)
  }
}

class PoolWriteDma(params: PoolParams, beatBits: Int) extends Module {
  private val ptrBits = math.max(1, log2Ceil(params.maxInflight))

  val io = IO(new Bundle {
    val start = Input(Bool())
    val cancel = Input(Bool())
    val input = Flipped(Decoupled(new PoolWriteBeat(params, beatBits)))
    val request = Decoupled(new PoolWriteIssue(params, beatBits))
    val response = Flipped(Decoupled(new PoolDmaResponse(params, beatBits)))
    val error = Valid(new PoolEvent)
    val complete = Output(Bool())
    val idle = Output(Bool())
  })

  val active = RegInit(false.B)
  val stopped = RegInit(false.B)
  val errorValid = RegInit(false.B)
  val errorStatus = RegInit(PoolStatus.Success)
  val complete = RegInit(false.B)
  val issue = RegInit(0.U(ptrBits.W))
  val outstanding = RegInit(0.U(log2Ceil(params.maxInflight + 1).W))
  val valid = RegInit(VecInit(Seq.fill(params.maxInflight)(false.B)))
  val last = Reg(Vec(params.maxInflight, Bool()))
  val lastReturned = RegInit(false.B)

  def next(pointer: UInt): UInt =
    Mux(pointer === (params.maxInflight - 1).U, 0.U, pointer + 1.U)

  val canIssue = active && !stopped && !io.cancel && !valid(issue)
  io.request.valid := io.input.valid && canIssue
  io.request.bits.address := io.input.bits.address
  io.request.bits.data := io.input.bits.data
  io.request.bits.mask := io.input.bits.mask
  io.request.bits.source := issue
  io.input.ready := io.request.ready && canIssue

  val responseIndex = io.response.bits.source(ptrBits - 1, 0)
  io.response.ready := active
  io.error.valid := errorValid
  io.error.bits.status := errorStatus
  io.complete := complete
  io.idle := !active

  val requestFire = io.request.fire
  val responseFire = io.response.fire
  val responseBypass = requestFire && responseIndex === issue
  val responseError = responseFire && (io.response.bits.denied || io.response.bits.corrupt)
  val responseLast = Mux(responseBypass, io.input.bits.last, last(responseIndex))
  val finalResponse = responseFire && responseLast
  val drainsNow = responseFire && Mux(requestFire, outstanding === 0.U, outstanding === 1.U)
  val finishesNow = drainsNow && (lastReturned || finalResponse)

  complete := false.B
  when(requestFire) {
    valid(issue) := true.B
    last(issue) := io.input.bits.last
    issue := next(issue)
  }
  when(responseFire) {
    assert(valid(responseIndex) || responseBypass)
    valid(responseIndex) := false.B
    when(finalResponse) {
      lastReturned := true.B
    }
    when(responseError && !errorValid) {
      errorValid := true.B
      errorStatus := Mux(io.response.bits.denied, PoolStatus.Denied, PoolStatus.Corrupt)
    }
    when(responseError) {
      stopped := true.B
    }
  }

  switch(Cat(requestFire, responseFire)) {
    is("b10".U) { outstanding := outstanding + 1.U }
    is("b01".U) { outstanding := outstanding - 1.U }
  }

  when(!stopped && !io.cancel && !responseError && finishesNow) {
    active := false.B
    complete := true.B
  }
  when((stopped || io.cancel || responseError) &&
    (outstanding === 0.U || drainsNow)) {
    active := false.B
    valid.foreach(_ := false.B)
  }
  when(io.cancel) {
    stopped := true.B
  }
  when(io.start) {
    active := true.B
    stopped := false.B
    errorValid := false.B
    errorStatus := PoolStatus.Success
    complete := false.B
    issue := 0.U
    outstanding := 0.U
    valid.foreach(_ := false.B)
    lastReturned := false.B
  }
}

class PoolTileLink(params: PoolParams)(implicit p: Parameters) extends LazyModule {
  val node = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name = "pool-dma",
    sourceId = IdRange(0, 2 * params.maxInflight),
    requestFifo = false)))))

  override lazy val module = new PoolTileLinkImp(this, params)
}

class PoolTileLinkImp(outer: PoolTileLink, params: PoolParams)(implicit p: Parameters)
    extends LazyModuleImp(outer) {
  val (tl, edge) = outer.node.out(0)
  val beatBits = tl.a.bits.data.getWidth
  val beatShift = log2Ceil(beatBits / 8)
  val io = IO(Flipped(new PoolDmaIO(params, beatBits)))

  val readSource = io.readRequest.bits.source
  val writeSource = io.writeRequest.bits.source + params.maxInflight.U
  val (getLegal, get) = edge.Get(readSource, io.readRequest.bits.address, beatShift.U)
  val (putLegal, put) = edge.Put(
    writeSource,
    io.writeRequest.bits.address,
    beatShift.U,
    io.writeRequest.bits.data,
    io.writeRequest.bits.mask)
  val a = Module(new RRArbiter(chiselTypeOf(tl.a.bits), 2))

  a.io.in(0).valid := io.readRequest.valid
  a.io.in(0).bits := get
  io.readRequest.ready := a.io.in(0).ready
  a.io.in(1).valid := io.writeRequest.valid
  a.io.in(1).bits := put
  io.writeRequest.ready := a.io.in(1).ready
  tl.a <> a.io.out

  when(a.io.in(0).fire) {
    assert(getLegal)
  }
  when(a.io.in(1).fire) {
    assert(putLegal)
  }

  val readResponse = tl.d.bits.source < params.maxInflight.U
  io.readResponse.valid := tl.d.valid && readResponse
  io.readResponse.bits.source := tl.d.bits.source
  io.readResponse.bits.data := tl.d.bits.data
  io.readResponse.bits.denied := tl.d.bits.denied
  io.readResponse.bits.corrupt := tl.d.bits.corrupt
  io.writeResponse.valid := tl.d.valid && !readResponse
  io.writeResponse.bits.source := tl.d.bits.source - params.maxInflight.U
  io.writeResponse.bits.data := tl.d.bits.data
  io.writeResponse.bits.denied := tl.d.bits.denied
  io.writeResponse.bits.corrupt := tl.d.bits.corrupt
  tl.d.ready := Mux(readResponse, io.readResponse.ready, io.writeResponse.ready)

  when(tl.d.fire) {
    assert(edge.done(tl.d))
    assert(tl.d.bits.opcode === Mux(
      readResponse,
      TLMessages.AccessAckData,
      TLMessages.AccessAck))
  }

  tl.b.ready := true.B
  tl.c.valid := false.B
  tl.c.bits := DontCare
  tl.e.valid := false.B
  tl.e.bits := DontCare
}
