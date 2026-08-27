// See LICENSE for license details

package aes

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.util.DecoupledHelper
import roccaccutils._

class CommandRouter(val cmd_queue_depth: Int)(implicit val p: Parameters) extends StreamingCommandRouter {
  class AesStreamerCmdBundle()(implicit p: Parameters) extends MemStreamerCmdBundle {
    val key = Valid(UInt(AES256Consts.KEY_SZ_BITS.W))
    val mode = Valid(Bool())
    val hwJob = Flipped(Decoupled(new AesJob))
    val cpuJobs = Output(UInt(64.W))
  }
  lazy val io = IO(new AesStreamerCmdBundle) // lazy matters

  val FUNCT_MODE                          = 4.U
  val FUNCT_KEY_0                         = 5.U
  val FUNCT_KEY_1                         = 6.U

  val cpuJobs = RegInit(0.U(64.W))
  when (io.rocc_in.fire && cur_funct === FUNCT_SRC_INFO) {
    cpuJobs := cpuJobs + 1.U
  }
  io.cpuJobs := cpuJobs

  // Mode interface
  val mode_queue = Module(new Queue(Bool(), cmd_queue_depth))
  mode_queue.io.enq.bits := cur_rs1
  val mode_fire = DecoupledHelper(
    io.rocc_in.valid,
    cur_funct === FUNCT_MODE,
    mode_queue.io.enq.ready
  )
  mode_queue.io.enq.valid := mode_fire.fire(mode_queue.io.enq.ready)
  io.mode.bits <> mode_queue.io.deq.bits
  io.mode.valid <> mode_queue.io.deq.valid
  mode_queue.io.deq.ready := true.B

  // Key interface
  val key_queue = Module(new Queue(UInt(AES256Consts.KEY_SZ_BITS.W), cmd_queue_depth))
  val key_lower_128 = RegInit(0.U(128.W))
  val key0_fire = DecoupledHelper(
    io.rocc_in.valid,
    cur_funct === FUNCT_KEY_0,
    key_queue.io.enq.ready
  )
  when (key0_fire.fire(key_queue.io.enq.ready)) {
    key_lower_128 := Cat(cur_rs1, cur_rs2)
  }
  val key1_fire = DecoupledHelper(
    io.rocc_in.valid,
    cur_funct === FUNCT_KEY_1,
    key_queue.io.enq.ready
  )
  key_queue.io.enq.valid := key1_fire.fire(key_queue.io.enq.ready)
  key_queue.io.enq.bits := Cat(Cat(cur_rs1, cur_rs2), key_lower_128)
  io.key.bits <> key_queue.io.deq.bits
  io.key.valid <> key_queue.io.deq.valid
  key_queue.io.deq.ready := true.B

  // streaming_fire provided by StreamingCommandRouter
  val hwReady = !io.rocc_in.valid &&
    src_info_queue.io.enq.ready &&
    dest_info_queue.io.enq.ready &&
    key_queue.io.enq.ready &&
    mode_queue.io.enq.ready
  io.hwJob.ready := hwReady
  val hwFire = io.hwJob.fire

  src_info_queue.io.enq.valid := src_info_fire.fire(src_info_queue.io.enq.ready) || hwFire
  src_info_queue.io.enq.bits.ip := Mux(hwFire, io.hwJob.bits.source.ip, cur_rs1)
  src_info_queue.io.enq.bits.isize := Mux(hwFire, io.hwJob.bits.source.isize, cur_rs2)
  dest_info_queue.io.enq.valid := dest_info_fire.fire(dest_info_queue.io.enq.ready) || hwFire
  dest_info_queue.io.enq.bits.op := Mux(hwFire, io.hwJob.bits.destination.op, cur_rs1)
  dest_info_queue.io.enq.bits.cmpflag := Mux(hwFire, io.hwJob.bits.destination.cmpflag, cur_rs2)
  key_queue.io.enq.valid := key1_fire.fire(key_queue.io.enq.ready) || hwFire
  key_queue.io.enq.bits := Mux(hwFire, io.hwJob.bits.key, Cat(Cat(cur_rs1, cur_rs2), key_lower_128))
  mode_queue.io.enq.valid := mode_fire.fire(mode_queue.io.enq.ready) || hwFire
  mode_queue.io.enq.bits := Mux(hwFire, io.hwJob.bits.encrypt, cur_rs1(0))

  io.rocc_in.ready := streaming_fire ||
    mode_fire.fire(io.rocc_in.valid) ||
    key0_fire.fire(io.rocc_in.valid) ||
    key1_fire.fire(io.rocc_in.valid)
}
