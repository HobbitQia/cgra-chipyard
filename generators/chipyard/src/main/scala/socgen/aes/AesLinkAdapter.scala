package chipyard.socgen.aes

import chisel3._
import chisel3.util._
import chipyard.socgen.link._

object AesLinkStatus {
  val BadAddress = 1
  val BadLength = 2
}

case class AesLinkParams(
  auto: AutoLinkParams,
  key: BigInt,
  encrypt: Boolean,
  ciphertextAddress: BigInt,
  completionAddress: BigInt) {
  require(key > 0 && key.bitLength <= 256)
  require(ciphertextAddress >= 0 && ciphertextAddress.bitLength <= auto.addressWidth)
  require(completionAddress >= 0 && completionAddress.bitLength <= auto.addressWidth)
}

class AesLinkAdapter(params: AesLinkParams) extends Module {
  val io = IO(new Bundle {
    val autoLink = new AutoEndpointIO(params.auto)
    val rootJob = Flipped(Decoupled(new _root_.aes.AesJob))
    val job = Decoupled(new _root_.aes.AesJob)
    val inputReadDone = Flipped(Decoupled(Bool()))
    val jobDone = Flipped(Decoupled(Bool()))
  })

  object Role {
    val idle :: root :: downstream :: Nil = Enum(3)
  }

  val role = RegInit(Role.idle)
  val watch = Reg(new AutoWatch(params.auto))
  val watchArmed = RegInit(false.B)
  val outputValid = RegInit(false.B)
  val outputDetail = RegInit(0.U(params.auto.detailWidth.W))
  val copy = Reg(new AutoCopyRequest(params.auto))
  val readDone = RegInit(false.B)
  val copyReported = RegInit(false.B)
  val done = RegInit(false.B)
  val computeAccepted = RegInit(false.B)

  val idle = role === Role.idle && !outputValid
  val rootAddressValid = io.rootJob.bits.destination.op === watch.address
  val rootLengthValid = io.rootJob.bits.source.isize === watch.bytes
  val rootValid = rootAddressValid && rootLengthValid
  val downstreamLaunch = io.autoLink.requestCopy.valid && idle
  val rootReady = idle && watchArmed && !io.autoLink.requestCopy.valid &&
    !io.autoLink.requestCompute.valid

  io.autoLink.watchOutput.ready := !watchArmed && !outputValid
  io.autoLink.reportOutput.valid := outputValid
  io.autoLink.reportOutput.bits.stage := 0.U
  io.autoLink.reportOutput.bits.job := watch.job
  io.autoLink.reportOutput.bits.status := Mux(
    outputDetail === 0.U,
    AutoLinkStatus.Success,
    AutoLinkStatus.SourceFailure)
  io.autoLink.reportOutput.bits.detail := outputDetail
  io.autoLink.reportOutput.bits.data := 0.U

  val downstreamJob = Wire(new _root_.aes.AesJob)
  downstreamJob.source.ip := io.autoLink.requestCopy.bits.sourceAddress
  downstreamJob.source.isize := io.autoLink.requestCopy.bits.bytes
  // Caliptra consumes the first key byte from the low UInt byte.
  val keyBytes = params.key.U(_root_.aes.AES256Consts.KEY_SZ_BITS.W)
    .asTypeOf(Vec(_root_.aes.AES256Consts.KEY_SZ_BYTES, UInt(8.W)))
  downstreamJob.key := Cat(keyBytes)
  downstreamJob.encrypt := params.encrypt.B
  downstreamJob.destination.op := params.ciphertextAddress.U
  downstreamJob.destination.cmpflag := params.completionAddress.U

  io.job.valid := downstreamLaunch || (io.rootJob.valid && rootReady && rootValid)
  io.job.bits := Mux(downstreamLaunch, downstreamJob, io.rootJob.bits)
  io.rootJob.ready := rootReady && Mux(rootValid, io.job.ready, true.B)
  io.autoLink.requestCopy.ready := io.job.ready && idle

  io.inputReadDone.ready := role === Role.root || (role === Role.downstream && !readDone)
  io.jobDone.ready := (role === Role.root && !outputValid) ||
    (role === Role.downstream && !done)

  io.autoLink.reportCopy.valid := role === Role.downstream && readDone && !copyReported
  io.autoLink.reportCopy.bits.task := copy.task
  io.autoLink.reportCopy.bits.status := AutoLinkStatus.Success
  io.autoLink.reportCopy.bits.detail := 0.U

  io.autoLink.requestCompute.ready := Mux(
    io.autoLink.requestCompute.bits.start,
    role === Role.downstream && copyReported && !computeAccepted,
    idle)
  io.autoLink.reportCompute.valid := role === Role.downstream && computeAccepted && done
  io.autoLink.reportCompute.bits.stage := 0.U
  io.autoLink.reportCompute.bits.job := copy.job
  io.autoLink.reportCompute.bits.status := AutoLinkStatus.Success
  io.autoLink.reportCompute.bits.detail := 0.U
  io.autoLink.reportCompute.bits.data := 0.U

  when(io.autoLink.watchOutput.fire) {
    watch := io.autoLink.watchOutput.bits
    watchArmed := true.B
  }
  when(io.rootJob.fire && !rootValid) {
    outputValid := true.B
    outputDetail := Mux(
      !rootAddressValid,
      AesLinkStatus.BadAddress.U,
      AesLinkStatus.BadLength.U)
  }
  when(io.job.fire && !downstreamLaunch) {
    role := Role.root
  }
  when(io.autoLink.requestCopy.fire) {
    role := Role.downstream
    copy := io.autoLink.requestCopy.bits
    readDone := false.B
    copyReported := false.B
    done := false.B
    computeAccepted := false.B
  }
  when(io.inputReadDone.fire) {
    when(role === Role.downstream) {
      readDone := true.B
    }
  }
  when(io.jobDone.fire) {
    when(role === Role.root) {
      outputValid := true.B
      outputDetail := 0.U
    }.otherwise {
      done := true.B
    }
  }
  when(io.autoLink.reportOutput.fire) {
    role := Role.idle
    watchArmed := false.B
    outputValid := false.B
  }
  when(io.autoLink.reportCopy.fire) {
    copyReported := true.B
  }
  when(io.autoLink.requestCompute.fire) {
    when(io.autoLink.requestCompute.bits.start) {
      computeAccepted := true.B
    }.otherwise {
      watchArmed := false.B
    }
  }
  when(io.autoLink.reportCompute.fire) {
    role := Role.idle
    readDone := false.B
    copyReported := false.B
    done := false.B
    computeAccepted := false.B
  }
}
