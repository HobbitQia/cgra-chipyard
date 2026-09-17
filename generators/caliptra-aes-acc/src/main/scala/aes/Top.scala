// See LICENSE for license details

package aes

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.tile._
import freechips.rocketchip.rocket.{TLBConfig}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.{SystemBusKey}
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{AsyncQueueParams, FromAsyncBundle, ToAsyncBundle}
import roccaccutils._

case object AES256AccelTLB extends Field[Option[TLBConfig]](None)

class AES256ECBAccel(opcodes: OpcodeSet)(implicit p: Parameters) extends MemStreamerAccel(
  opcodes = opcodes) {

  override lazy val module = new AES256ECBAccelImp(this)

  require(p(SystemBusKey).beatBytes == 32, "Only tested on 32B SBUS width") // TODO: should work for 128b

  lazy val tlbConfig = p(AES256AccelTLB).get
  lazy val xbarBetweenMem = p(AES256ECBAccelInsertXbarBetweenMemory)
  lazy val logger = AES256ECBLogger
  val jobNode = if (p(AESJobPortKey))
    Some(BundleBridgeSink[AesJobAsyncLink]()) else None
}

class AES256ECBAccelImp(outer: AES256ECBAccel)(implicit p: Parameters)
  extends MemStreamerAccelImp(outer) {

  lazy val queueDepth = p(AES256ECBAccelCmdQueueDepth)

  lazy val cmd_router = Module(new CommandRouter(queueDepth))
  lazy val streamer = Module(new AES256ECB(outer.logger))

  val job = Wire(Decoupled(new AesJob))
  outer.jobNode match {
    case Some(node) => job <> FromAsyncBundle(node.in.head._1.job)
    case None =>
      job.valid := false.B
      job.bits := 0.U.asTypeOf(new AesJob)
  }

  val hwActive = RegInit(false.B)
  val readDoneSeen = RegInit(false.B)

  cmd_router.io.rocc_in.valid := io.cmd.valid && !hwActive
  cmd_router.io.rocc_in.bits := io.cmd.bits
  io.cmd.ready := cmd_router.io.rocc_in.ready && !hwActive

  val cpuIdle = memwriter.io.bufs_completed === cmd_router.io.issuedJobs &&
    memwriter.io.no_writes_inflight
  cmd_router.io.hwJob.valid := job.valid && !hwActive && cpuIdle
  cmd_router.io.hwJob.bits := job.bits
  job.ready := cmd_router.io.hwJob.ready && !hwActive && cpuIdle

  when(job.fire) {
    hwActive := true.B
    readDoneSeen := false.B
  }

  val inputReadDone = hwActive && !readDoneSeen && streamer.io.inputReadDone
  when(inputReadDone) {
    readDoneSeen := true.B
  }

  val jobDone = hwActive && cpuIdle
  when(jobDone) {
    hwActive := false.B
  }

  outer.jobNode.foreach { node =>
    val link = node.in.head._1
    val readDone = Module(new Queue(Bool(), 1))
    val done = Module(new Queue(Bool(), 1))
    readDone.io.enq.valid := inputReadDone
    readDone.io.enq.bits := true.B
    done.io.enq.valid := jobDone
    done.io.enq.bits := true.B
    assert(!inputReadDone || readDone.io.enq.ready)
    assert(!jobDone || done.io.enq.ready)
    link.inputReadDone <> ToAsyncBundle(readDone.io.deq, AsyncQueueParams.singleton())
    link.jobDone <> ToAsyncBundle(done.io.deq, AsyncQueueParams.singleton())
  }

  streamer.io.key <> cmd_router.io.key
  streamer.io.mode <> cmd_router.io.mode

  val keyReady = RegInit(false.B)
  val modeReady = RegInit(false.B)
  val destReady = RegInit(false.B)
  val sourceReady = keyReady && modeReady && destReady

  // Keep the queued source out of DMA until this job's configuration is applied.
  memloader.io.src_info.valid := cmd_router.io.src_info.valid && sourceReady
  cmd_router.io.src_info.ready := memloader.io.src_info.ready && sourceReady

  when(streamer.io.key.valid) {
    keyReady := true.B
  }
  when(streamer.io.mode.valid) {
    modeReady := true.B
  }
  when(memwriter.io.decompress_dest_info.fire) {
    destReady := true.B
  }
  when(memloader.io.src_info.fire) {
    keyReady := false.B
    modeReady := false.B
    destReady := false.B
  }
}
