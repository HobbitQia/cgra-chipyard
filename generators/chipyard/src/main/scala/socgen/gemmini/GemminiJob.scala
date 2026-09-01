package chipyard.socgen.gemmini

import chisel3._
import chisel3.util._
import chipyard.socgen.link._
import freechips.rocketchip.tile.{OpcodeSet, RoCCCommand}
import gemmini.GemminiISA._
import org.chipsalliance.cde.config.Parameters

class GemminiJobDesc extends Bundle {
  val aRow = UInt(32.W)
  val bRow = UInt(32.W)
  val accAddress = UInt(32.W)
  val outputRow = UInt(32.W)
  val outputRows = UInt(32.W)
}

/** Runs one local-SPM, weight-stationary Gemmini tile. */
class GemminiJobAdapter(
  config: gemmini.GemminiArrayConfig[SInt, gemmini.Float, gemmini.Float],
  auto: AutoLinkParams)(implicit p: Parameters)
    extends Module {
  val io = IO(new Bundle {
    val configIn = Flipped(Decoupled(new GemminiJobDesc))
    val requestCopy = Flipped(Decoupled(new AutoCopyRequest(auto)))
    val reportCopy = Decoupled(new AutoCopyResult(auto))
    val requestCompute = Flipped(Decoupled(new AutoComputeRequest))
    val reportCompute = Decoupled(new AutoEvent(auto))
    val publication = Flipped(Valid(new AutoEvent(auto)))
    val command = Decoupled(new GemminiAutoCommand)
  })

  object State {
    val idle :: reportCopy :: waitCompute :: issue :: waitOutput :: reportCompute :: Nil = Enum(6)
  }

  val state = RegInit(State.idle)
  val desc = Reg(new GemminiJobDesc)
  val descValid = RegInit(false.B)
  val copyTask = Reg(UInt(auto.taskWidth.W))
  val commandIndex = RegInit(0.U(3.W))
  val result = Reg(new AutoEvent(auto))

  val dim = config.meshRows * config.tileRows
  val outputStride = config.accType.getWidth / config.inputType.getWidth
  val storeStride = dim * config.accType.getWidth / 8
  require(config.dataflow == gemmini.Dataflow.WS)
  require(config.acc_scale_args.isEmpty)

  val commands = Wire(Vec(5, new RoCCCommand))
  commands.foreach { command =>
    command := 0.U.asTypeOf(new RoCCCommand)
    command.inst.opcode := OpcodeSet.custom3.opcodes.head
    command.inst.xs1 := true.B
    command.inst.xs2 := true.B
  }

  val configExRs1 = Wire(new ConfigExRs1(config.acc_scale_t_bits))
  configExRs1 := 0.U.asTypeOf(configExRs1)
  configExRs1.a_stride := 1.U
  configExRs1.dataflow := gemmini.Dataflow.WS.id.U
  configExRs1.cmd_type := CONFIG_EX
  val configExRs2 = Wire(new ConfigExRs2)
  configExRs2 := 0.U.asTypeOf(configExRs2)
  configExRs2.c_stride := 1.U
  commands(0).inst.funct := CONFIG_CMD
  commands(0).rs1 := configExRs1.asUInt
  commands(0).rs2 := configExRs2.asUInt

  val configStRs1 = Wire(new ConfigMvoutRs1)
  configStRs1 := 0.U.asTypeOf(configStRs1)
  configStRs1.cmd_type := CONFIG_STORE
  val configStRs2 = Wire(new ConfigMvoutRs2(config.acc_scale_t_bits, 32))
  configStRs2 := 0.U.asTypeOf(configStRs2)
  configStRs2.stride := storeStride.U
  commands(1).inst.funct := CONFIG_CMD
  commands(1).rs1 := configStRs1.asUInt
  commands(1).rs2 := configStRs2.asUInt

  val preloadRs1 = Wire(new PreloadRs(config.mvin_rows_bits, config.mvin_cols_bits, config.local_addr_t))
  preloadRs1 := 0.U.asTypeOf(preloadRs1)
  preloadRs1.num_rows := dim.U
  preloadRs1.num_cols := dim.U
  preloadRs1.local_addr := gemmini.LocalAddr.cast_to_sp_addr(preloadRs1.local_addr, desc.bRow)
  val preloadRs2 = Wire(new PreloadRs(config.mvout_rows_bits, config.mvout_cols_bits, config.local_addr_t))
  preloadRs2 := 0.U.asTypeOf(preloadRs2)
  preloadRs2.num_rows := dim.U
  preloadRs2.num_cols := dim.U
  preloadRs2.local_addr := gemmini.LocalAddr.cast_to_acc_addr(
    preloadRs2.local_addr,
    desc.accAddress,
    accumulate = false.B,
    read_full = false.B)
  commands(2).inst.funct := PRELOAD_CMD
  commands(2).rs1 := preloadRs1.asUInt
  commands(2).rs2 := preloadRs2.asUInt

  val computeRs1 = Wire(new ComputeRs(config.mvin_rows_bits, config.mvin_cols_bits, config.local_addr_t))
  computeRs1 := 0.U.asTypeOf(computeRs1)
  computeRs1.num_rows := dim.U
  computeRs1.num_cols := dim.U
  computeRs1.local_addr := gemmini.LocalAddr.cast_to_sp_addr(computeRs1.local_addr, desc.aRow)
  val computeRs2 = Wire(new ComputeRs(config.mvin_rows_bits, config.mvin_cols_bits, config.local_addr_t))
  computeRs2 := 0.U.asTypeOf(computeRs2)
  computeRs2.num_rows := dim.U
  computeRs2.num_cols := dim.U
  computeRs2.local_addr := gemmini.LocalAddr.garbage_addr(computeRs2.local_addr)
  commands(3).inst.funct := COMPUTE_AND_FLIP_CMD
  commands(3).rs1 := computeRs1.asUInt
  commands(3).rs2 := computeRs2.asUInt

  val mvoutRs1 = Wire(new MvoutSpadRs1(32, config.local_addr_t))
  mvoutRs1 := 0.U.asTypeOf(mvoutRs1)
  mvoutRs1.stride := outputStride.U
  mvoutRs1.local_addr := gemmini.LocalAddr.cast_to_sp_addr(mvoutRs1.local_addr, desc.outputRow)
  val mvoutRs2 = Wire(new MvoutRs2(config.mvout_rows_bits, config.mvout_cols_bits, config.local_addr_t))
  mvoutRs2 := 0.U.asTypeOf(mvoutRs2)
  mvoutRs2.num_rows := desc.outputRows
  mvoutRs2.num_cols := dim.U
  mvoutRs2.local_addr := gemmini.LocalAddr.cast_to_acc_addr(
    mvoutRs2.local_addr,
    desc.accAddress,
    accumulate = false.B,
    read_full = true.B)
  commands(4).inst.funct := STORE_SPAD_CMD
  commands(4).rs1 := mvoutRs1.asUInt
  commands(4).rs2 := mvoutRs2.asUInt

  io.configIn.ready := state === State.idle && !descValid
  io.requestCopy.ready := state === State.idle && descValid
  io.reportCopy.valid := state === State.reportCopy
  io.reportCopy.bits.task := copyTask
  io.reportCopy.bits.status := AutoLinkStatus.Success
  io.reportCopy.bits.detail := 0.U
  io.requestCompute.ready := Mux(
    io.requestCompute.bits.start,
    state === State.waitCompute,
    state === State.idle || state === State.waitCompute)
  io.reportCompute.valid := state === State.reportCompute
  io.reportCompute.bits := result
  io.command.valid := state === State.issue
  io.command.bits.command := commands(commandIndex)
  io.command.bits.last := commandIndex === (commands.length - 1).U

  when(io.configIn.fire) {
    desc := io.configIn.bits
    descValid := true.B
  }
  when(io.requestCopy.fire) {
    copyTask := io.requestCopy.bits.task
    state := State.reportCopy
  }
  when(io.reportCopy.fire) {
    state := State.waitCompute
  }
  when(io.requestCompute.fire) {
    when(io.requestCompute.bits.start) {
      commandIndex := 0.U
      state := State.issue
    }.otherwise {
      descValid := false.B
      state := State.idle
    }
  }
  when(io.command.fire) {
    when(commandIndex === (commands.length - 1).U) {
      state := State.waitOutput
    }.otherwise {
      commandIndex := commandIndex + 1.U
    }
  }
  when(state === State.waitOutput && io.publication.valid) {
    result := io.publication.bits
    state := State.reportCompute
  }
  when(io.reportCompute.fire) {
    descValid := false.B
    state := State.idle
  }
}
