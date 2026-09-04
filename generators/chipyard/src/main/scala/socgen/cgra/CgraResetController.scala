package chipyard.socgen.cgra

import chisel3._
import chisel3.util._
import chipyard.example.{CGRACmdGenerated, CGRAParams}

/** Resets CGRA execution state between complete kernel jobs. */
class CgraResetController(params: CGRAParams) extends Module {
  val io = IO(new Bundle {
    val cpuIn = Flipped(Decoupled(UInt(params.intraPktWidth.W)))
    val cpuOut = Decoupled(UInt(params.intraPktWidth.W))
    val autoRequest = Flipped(Decoupled(Bool()))
    val manualComplete = Input(Bool())
    val safe = Input(Bool())
    val localReset = Output(Bool())
    val holdCpu = Output(Bool())
  })

  object State {
    val idle :: waitSafe :: sendHeld :: Nil = Enum(3)
  }

  val state = RegInit(State.idle)
  val heldPacket = Reg(UInt(params.intraPktWidth.W))
  val manualActive = RegInit(false.B)
  val manualDone = RegInit(false.B)
  val command = io.cpuIn.bits(
    params.packetLayout.cmdLsb + params.cmdWidth - 1,
    params.packetLayout.cmdLsb)
  val outputCommand = io.cpuOut.bits(
    params.packetLayout.cmdLsb + params.cmdWidth - 1,
    params.packetLayout.cmdLsb)
  val baseConfig = command >= CGRACmdGenerated.CMD_CONFIG.U &&
    command <= CGRACmdGenerated.CMD_CONFIG_CTRL_LOWER_BOUND.U
  val streamConfig = command >= CGRACmdGenerated.CMD_CONFIG_STREAMING_LD_START_ADDR.U &&
    command <= CGRACmdGenerated.CMD_CONFIG_STREAMING_LD_END_ADDR.U
  val loopConfig = command >= CGRACmdGenerated.CMD_CONFIG_LOOP_LOWER.U &&
    command <= CGRACmdGenerated.CMD_CONFIG_LOOP_STEP.U
  val controllerConfig = command >= CGRACmdGenerated.CMD_LC_CONFIG_LOWER.U &&
    command <= CGRACmdGenerated.CMD_LC_CONFIG_PARENT.U
  val kernelConfig = io.cpuIn.valid &&
    (baseConfig || streamConfig || loopConfig || controllerConfig ||
      command === CGRACmdGenerated.CMD_CONST.U ||
      command === CGRACmdGenerated.CMD_RECORD_PHI_ADDR.U ||
      command === CGRACmdGenerated.CMD_CONFIG_GEP_STRIDE.U)
  val manualSwitch = (manualDone || (io.manualComplete && manualActive)) && kernelConfig

  io.cpuIn.ready := false.B
  io.cpuOut.valid := false.B
  io.cpuOut.bits := heldPacket
  io.autoRequest.ready := false.B
  io.localReset := false.B
  io.holdCpu := state =/= State.idle

  switch(state) {
    is(State.idle) {
      when(io.autoRequest.valid) {
        io.autoRequest.ready := io.safe
        io.localReset := io.safe
      }.elsewhen(manualSwitch) {
        io.cpuIn.ready := true.B
        when(io.cpuIn.fire) {
          heldPacket := io.cpuIn.bits
          state := State.waitSafe
        }
      }.otherwise {
        io.cpuOut.valid := io.cpuIn.valid
        io.cpuOut.bits := io.cpuIn.bits
        io.cpuIn.ready := io.cpuOut.ready
      }
    }
    is(State.waitSafe) {
      when(io.safe) {
        io.localReset := true.B
        state := State.sendHeld
      }
    }
    is(State.sendHeld) {
      io.cpuOut.valid := true.B
      when(io.cpuOut.fire) {
        state := State.idle
      }
    }
  }

  when(io.cpuOut.fire &&
      (outputCommand === CGRACmdGenerated.CMD_LAUNCH.U ||
        outputCommand === CGRACmdGenerated.CMD_RESUME.U)) {
    manualActive := true.B
  }
  when(io.manualComplete && manualActive) {
    manualActive := false.B
    manualDone := true.B
  }
  when(io.localReset) {
    manualActive := false.B
    manualDone := false.B
  }
}
