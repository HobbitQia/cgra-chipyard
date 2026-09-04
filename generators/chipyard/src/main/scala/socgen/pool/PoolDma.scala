package chipyard.socgen.pool

import chisel3._
import chisel3.util._

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
