package chipyard.example

import chisel3._
import chisel3.util._

/** Queues MMIO control writes and admits a job only after the CGRA has stored
  * its complete launch sequence.
  */
class CgraSpmControl(params: CgraSpmParams) extends Module {
  val io = IO(new Bundle {
    val headerSubmit = Flipped(Decoupled(new CgraSpmHeader(params)))
    val packetSubmit = Flipped(Decoupled(UInt(params.cgra.intraPktWidth.W)))
    val jobSubmit = Flipped(Decoupled(new SpmDmaJob(params.protocol)))
    val stepSubmit = Flipped(Decoupled(new SpmDmaCommand(params.protocol)))
    val configOut = Decoupled(new CgraSpmConfig(params))
    val configAck = Flipped(Decoupled(new CgraSpmConfigAck(params)))
    val jobOut = Decoupled(new SpmDmaJob(params.protocol))
    val stepOut = Decoupled(new SpmDmaCommand(params.protocol))
    val resultIn = Flipped(Decoupled(new SpmDmaResult(params.protocol)))
    val resultOut = Decoupled(new SpmDmaResult(params.protocol))
  })

  val configArbiter = Module(new Arbiter(new CgraSpmConfig(params), 2))
  configArbiter.io.in(0).valid := io.headerSubmit.valid
  configArbiter.io.in(0).bits.kind := CgraSpmConfigKind.Header
  configArbiter.io.in(0).bits.header := io.headerSubmit.bits
  configArbiter.io.in(0).bits.packet := 0.U
  io.headerSubmit.ready := configArbiter.io.in(0).ready

  configArbiter.io.in(1).valid := io.packetSubmit.valid
  configArbiter.io.in(1).bits.kind := CgraSpmConfigKind.Packet
  configArbiter.io.in(1).bits.header := 0.U.asTypeOf(new CgraSpmHeader(params))
  configArbiter.io.in(1).bits.packet := io.packetSubmit.bits
  io.packetSubmit.ready := configArbiter.io.in(1).ready
  io.configOut <> configArbiter.io.out

  val ack = Module(new Queue(new CgraSpmConfigAck(params), 1))
  ack.io.enq <> io.configAck
  val ackMatches = ack.io.deq.bits.jobId === io.jobSubmit.bits.jobId &&
    ack.io.deq.bits.slot === io.jobSubmit.bits.slot &&
    ack.io.deq.bits.bytes === io.jobSubmit.bits.bytes
  val ackSuccess = ackMatches && ack.io.deq.bits.status === SpmDmaStatus.Success
  val submitFailure = ack.io.deq.valid && io.jobSubmit.valid && !ackSuccess

  val results = Module(new Queue(new SpmDmaResult(params.protocol), 2))
  val resultArbiter = Module(new Arbiter(new SpmDmaResult(params.protocol), 2))
  resultArbiter.io.in(0) <> io.resultIn
  resultArbiter.io.in(1).valid := submitFailure
  resultArbiter.io.in(1).bits.jobId := io.jobSubmit.bits.jobId
  resultArbiter.io.in(1).bits.slot := io.jobSubmit.bits.slot
  resultArbiter.io.in(1).bits.bytes := io.jobSubmit.bits.bytes
  resultArbiter.io.in(1).bits.stage := SpmDmaStage.Consumer
  resultArbiter.io.in(1).bits.status := SpmDmaStatus.StageFailure
  resultArbiter.io.in(1).bits.detail := Mux(
    ackMatches,
    ack.io.deq.bits.detail,
    CgraSpmStatus.IdentityMismatch.U)
  resultArbiter.io.in(1).bits.data := 0.U
  results.io.enq <> resultArbiter.io.out
  io.resultOut <> results.io.deq

  io.jobOut.valid := io.jobSubmit.valid && ack.io.deq.valid && ackSuccess
  io.jobOut.bits := io.jobSubmit.bits
  io.jobSubmit.ready := ack.io.deq.valid && Mux(ackSuccess, io.jobOut.ready, resultArbiter.io.in(1).ready)
  ack.io.deq.ready := io.jobSubmit.fire

  io.stepOut <> io.stepSubmit
}
