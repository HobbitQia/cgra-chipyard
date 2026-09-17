package chipyard.socgen.link

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.{LazyRoCC, LazyRoCCModuleImp, OpcodeSet, RoCCResponse}
import org.chipsalliance.cde.config.Parameters

case class AccelSpec(name: String, kind: String, id: Int, spmBase: BigInt, spmBytes: Int, controlAddress: BigInt)

object RoCCGroup {
  val Select = 127

  def flatten(roccs: Seq[LazyRoCC]): Seq[LazyRoCC] = roccs.flatMap {
    case group: RoCCGroup => group.members
    case rocc => Seq(rocc)
  }
}

/** Shares CPU command access; each member keeps its own TileLink DMA paths. */
class RoCCGroup(factories: Seq[Parameters => LazyRoCC])(implicit p: Parameters)
    extends LazyRoCC(OpcodeSet.all) {
  val members: Seq[LazyRoCC] = factories.map(_(p))
  require(members.nonEmpty)
  require(members.forall(member => !member.usesFPU && member.roccCSRs.isEmpty),
    "RoCCGroup does not provide FPU or custom CSR access")

  override val nPTWPorts: Int = members.map(_.nPTWPorts).sum
  members.foreach { member =>
    atlNode :=* member.atlNode
    tlNode :=* member.tlNode
    member.stlNode :*= stlNode
  }

  override lazy val module = new RoCCGroupModule(this)
}

class RoCCGroupModule(outer: RoCCGroup)(implicit p: Parameters)
    extends LazyRoCCModuleImp(outer) {
  private val members = outer.members.map(_.module.io)
  private val target = Reg(UInt(math.max(1, log2Ceil(members.size)).W))
  private val cmd = io.cmd
  private val select = OpcodeSet.custom0.matches(cmd.bits.inst.opcode) &&
    cmd.bits.inst.funct === RoCCGroup.Select.U
  private val responses = Module(new RRArbiter(new RoCCResponse, members.size))

  // Target prefixes and native commands retain the Rocket command queue's order.
  when(cmd.fire && select) {
    target := cmd.bits.rs1
  }
  members.zipWithIndex.foreach { case (member, index) =>
    member.cmd.valid := cmd.valid && !select && target === index.U
    member.cmd.bits := cmd.bits
    responses.io.in(index) <> member.resp
    member.exception := io.exception

    // These accelerators use TileLink DMA, not the RoCC DCache or FPU ports.
    member.mem := DontCare
    member.mem.req.ready := false.B
    member.mem.resp.valid := false.B
    member.mem.uncached_resp.foreach(_.valid := false.B)
    member.mem.ordered := true.B
    member.mem.store_pending := false.B
    member.mem.clock_enabled := false.B
    member.fpu_req.ready := false.B
    member.fpu_resp.valid := false.B
    member.fpu_resp.bits := DontCare
  }
  cmd.ready := select || members.zipWithIndex.map { case (member, index) =>
    member.cmd.ready && target === index.U
  }.reduce(_ || _)

  io.resp <> responses.io.out
  members.flatMap(_.ptw).zip(io.ptw).foreach { case (member, port) => port <> member }
  io.busy := cmd.valid || members.map(_.busy).reduce(_ || _)
  io.interrupt := members.map(_.interrupt).reduce(_ || _)

  io.mem.req.valid := false.B
  io.mem.req.bits := 0.U.asTypeOf(io.mem.req.bits)
  io.mem.s1_kill := false.B
  io.mem.s1_data := 0.U.asTypeOf(io.mem.s1_data)
  io.mem.s2_kill := false.B
  io.mem.keep_clock_enabled := false.B
  io.mem.uncached_resp.foreach(_.ready := false.B)
  io.fpu_req.valid := false.B
  io.fpu_req.bits := DontCare
  io.fpu_resp.ready := false.B
}
