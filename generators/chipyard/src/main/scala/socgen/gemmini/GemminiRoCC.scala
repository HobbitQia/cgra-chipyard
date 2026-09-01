package chipyard.socgen.gemmini

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.BundleBridgeSink
import freechips.rocketchip.tile.{BuildRoCC, LazyRoCC, LazyRoCCModuleImp, RoCCCommand}
import freechips.rocketchip.util.{AsyncBundle, FromAsyncBundle}
import org.chipsalliance.cde.config.{Config, Parameters}
import org.chipsalliance.diplomacy.lazymodule.LazyModule

class GemminiAutoCommand(implicit p: Parameters) extends Bundle {
  val command = new RoCCCommand
  val last = Bool()
}

/** Keeps automatic command injection outside the Gemmini IP. */
class GemminiRoCC(
  val config: gemmini.GemminiArrayConfig[SInt, gemmini.Float, gemmini.Float],
  auto: Boolean)(implicit p: Parameters)
    extends LazyRoCC(
      opcodes = config.opcodes,
      nPTWPorts = if (config.use_shared_tlb) 1 else 2) {
  val accelerator = LazyModule(new gemmini.Gemmini(config))
  val cmdNode = if (auto) {
    Some(BundleBridgeSink[AsyncBundle[GemminiAutoCommand]]())
  } else {
    None
  }

  override val atlNode = accelerator.atlNode
  override val tlNode = accelerator.tlNode
  override val stlNode = accelerator.stlNode
  override lazy val module = new GemminiRoCCModule(this)
}

class GemminiRoCCModule(outer: GemminiRoCC)(implicit p: Parameters)
    extends LazyRoCCModuleImp(outer) {
  private val gemmini = outer.accelerator.module
  private val autoCmd = Wire(Decoupled(new GemminiAutoCommand))
  private val owned = RegInit(false.B)
  private val last = RegInit(false.B)

  outer.cmdNode match {
    case Some(node) => autoCmd <> FromAsyncBundle(node.in.head._1)
    case None =>
      autoCmd.valid := false.B
      autoCmd.bits := DontCare
  }

  val selectAuto = owned || (autoCmd.valid && !gemmini.io.busy)
  gemmini.io.cmd.valid := Mux(
    selectAuto,
    autoCmd.valid,
    io.cmd.valid && !autoCmd.valid)
  gemmini.io.cmd.bits := Mux(selectAuto, autoCmd.bits.command, io.cmd.bits)
  autoCmd.ready := selectAuto && gemmini.io.cmd.ready
  io.cmd.ready := !owned && !autoCmd.valid && gemmini.io.cmd.ready

  when(autoCmd.fire) {
    owned := true.B
    last := autoCmd.bits.last
  }
  when(owned && last && !gemmini.io.busy) {
    owned := false.B
    last := false.B
  }

  io.resp <> gemmini.io.resp
  io.mem <> gemmini.io.mem
  io.ptw <> gemmini.io.ptw
  io.fpu_req <> gemmini.io.fpu_req
  io.fpu_resp <> gemmini.io.fpu_resp
  io.csrs <> gemmini.io.csrs
  gemmini.io.exception := io.exception
  io.busy := gemmini.io.busy || owned || autoCmd.valid
  io.interrupt := gemmini.io.interrupt
}

class WithGemminiRoCC(
  config: gemmini.GemminiArrayConfig[SInt, gemmini.Float, gemmini.Float],
  auto: Boolean = false)
    extends Config((_, _, up) => {
      case BuildRoCC => up(BuildRoCC) :+ { p: Parameters =>
        implicit val q: Parameters = p
        LazyModule(new GemminiRoCC(config, auto))
      }
    })
