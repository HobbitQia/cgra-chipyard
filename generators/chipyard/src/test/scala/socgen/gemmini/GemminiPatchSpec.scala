package chipyard.socgen.gemmini

import chisel3._
import chiseltest._
import chipyard.socgen.link._
import freechips.rocketchip.tile.{RoCCCommand, RocketTileParams, TileKey}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class GemminiPatchSpec extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = Parameters.empty.alterPartial {
    case TileKey => RocketTileParams()
  }
  private val auto = AutoLinkParams(
    stages = Seq(AutoStageSpec("conv", "gemmini", 0), AutoStageSpec("relu", "cgra", 0)),
    dependencies = Seq(AutoDependencySpec(Some(0), 1, Some(AutoCopySpec(0, 0, 64)))),
    endpoints = Seq(
      AutoEndpointSpec("gemmini", Some(AutoBuffer(0x10000, 65536)), 65536, bufferSlots = 2),
      AutoEndpointSpec("cgra", None, 256)),
    beatBytes = 32,
    controlAddress = 0x30000,
    controlBytes = 4096)
  private val params = GemminiLinkParams(auto, beatBytes = 32, commandCapacity = 16)
  private case class Command(funct: Int, rs1: BigInt, rs2: BigInt)
  private case class Patch(command: Int, operand: Boolean, lsb: Int, width: Int,
      source: Int, scale: Int = 1, offset: Int = 0)
  private case class Tile(id: Int, row: Int, column: Int, rows: Int, columns: Int)
  private case class Window(rows: Int = 3, columns: Int = 5, rowStep: Int = 1,
      columnStep: Int = 1, top: Int = 1, bottom: Int = 1, left: Int = 1, right: Int = 1,
      address: BigInt = 0x40000, pixelBytes: Int = 3)

  private def pack(fields: (Int, Int)*): BigInt =
    fields.foldLeft(BigInt(0)) { case (value, (field, shift)) => value | (BigInt(field) << shift) }

  private val commands = Seq(
    Command(0, pack(1 -> 16, 1 -> 2), pack(1 -> 48)),
    Command(0, 2, pack(0x3f800000 -> 32, 8 -> 0)),
    Command(16, pack(8 -> 48, 3 -> 32, 3 -> 16, 1 -> 0), pack(1 -> 56, 1 -> 48, 1 -> 32, 1 -> 16, 1 -> 0)),
    Command(17, pack(3 -> 48, 1 -> 32, 1 -> 16, 1 -> 8), pack(1 -> 48, 1 -> 32, 1 -> 16, 8 -> 0)),
    Command(18, pack(3 -> 48, 3 -> 32, 3 -> 16, 1 -> 0), pack(1 -> 32, 5 -> 0)),
    Command(19, pack(1 -> 48, 1 -> 0), pack(3 -> 48, 8 -> 32, 8 -> 16, 1 -> 0)),
    Command(20, 0x50000, 0x1ffe0),
    Command(21, 1, 0x40000),
    Command(15, pack(1 -> 18, 1 -> 16, 1 -> 8, 1 -> 0), 1),
    Command(37, 0x11110000, BigInt("222200000000", 16)))

  private val patches = Seq(
    Patch(2, false, 16, 16, GemminiValue.InputRows),
    Patch(2, true, 32, 16, GemminiValue.Columns),
    Patch(2, true, 16, 16, GemminiValue.Rows),
    Patch(2, true, 0, 16, GemminiValue.Rows),
    Patch(3, false, 32, 16, GemminiValue.Columns),
    Patch(3, true, 32, 16, GemminiValue.Rows),
    Patch(3, true, 16, 16, GemminiValue.Columns),
    Patch(4, false, 0, 16, GemminiValue.Left),
    Patch(4, true, 48, 16, GemminiValue.Right),
    Patch(4, true, 32, 16, GemminiValue.Top),
    Patch(4, true, 24, 8, GemminiValue.Bottom),
    Patch(4, true, 0, 16, GemminiValue.InputColumns),
    Patch(5, false, 48, 16, GemminiValue.Rows),
    Patch(5, true, 48, 16, GemminiValue.InputStride),
    Patch(5, true, 0, 16, GemminiValue.Columns),
    Patch(6, true, 0, 64, GemminiValue.OutputAddress),
    Patch(7, true, 0, 64, GemminiValue.InputAddress),
    Patch(9, false, 0, 16, GemminiValue.Slot, scale = 64, offset = 32),
    Patch(9, true, 0, 32, GemminiValue.TileId, scale = 3, offset = -2))

  private def pokeTile(port: AutoTile, tile: Tile): Unit = {
    port.id.poke(tile.id.U)
    port.row.poke(tile.row.U)
    port.column.poke(tile.column.U)
    port.rows.poke(tile.rows.U)
    port.columns.poke(tile.columns.U)
    port.last.poke(false.B)
  }

  private def pokeCommand(port: RoCCCommand, command: Command): Unit = {
    port.inst.funct.poke(command.funct.U)
    port.inst.opcode.poke(0x7b.U)
    port.inst.rd.poke(9.U)
    port.inst.rs1.poke(10.U)
    port.inst.rs2.poke(11.U)
    port.inst.xd.poke(false.B)
    port.inst.xs1.poke(true.B)
    port.inst.xs2.poke(true.B)
    port.rs1.poke(command.rs1.U)
    port.rs2.poke(command.rs2.U)
  }

  private def pokePatch(port: GemminiPatchEntry, patch: Patch): Unit = {
    port.command.poke(patch.command.U)
    port.operand.poke(patch.operand.B)
    port.lsb.poke(patch.lsb.U)
    port.bitCount.poke(patch.width.U)
    port.source.poke(patch.source.U)
    port.scale.poke(patch.scale.U)
    port.offset.poke((BigInt(patch.offset) & ((BigInt(1) << 32) - 1)).U)
  }

  private def pokeConfig(port: GemminiLinkConfig, window: Window,
      entries: Seq[Patch], job: Int = 0): Unit = {
    port.job.poke(job.U)
    port.commandCount.poke(commands.size.U)
    port.patchCount.poke(entries.size.U)
    port.window.region.rows.poke(window.rows.U)
    port.window.region.columns.poke(window.columns.U)
    port.window.region.rowStep.poke(window.rowStep.U)
    port.window.region.columnStep.poke(window.columnStep.U)
    port.window.region.top.poke(window.top.U)
    port.window.region.bottom.poke(window.bottom.U)
    port.window.region.left.poke(window.left.U)
    port.window.region.right.poke(window.right.U)
    port.window.address.poke(window.address.U)
    port.window.pixelBytes.poke(window.pixelBytes.U)
    port.window.rowBytes.poke((window.columns * window.pixelBytes).U)
  }

  private def init(dut: GemminiPatch): Unit = {
    dut.io.begin.valid.poke(false.B)
    dut.io.patch.valid.poke(false.B)
    dut.io.copy.valid.poke(false.B)
    dut.io.start.poke(false.B)
    dut.io.request.job.poke(0.U)
    dut.io.request.start.poke(true.B)
    dut.io.watch.job.poke(0.U)
    dut.io.job.poke(0.U)
    dut.io.index.poke(0.U)
  }

  private def capture(dut: GemminiPatch, window: Window = Window(),
      entries: Seq[Patch] = patches, job: Int = 0): Unit = {
    pokeConfig(dut.io.begin.bits, window, entries, job)
    dut.io.begin.valid.poke(true.B)
    dut.clock.step()
    dut.io.begin.valid.poke(false.B)
    for (entry <- entries) {
      dut.io.configured.expect(false.B)
      dut.io.patch.ready.expect(true.B)
      pokePatch(dut.io.patch.bits, entry)
      dut.io.patch.valid.poke(true.B)
      dut.clock.step()
    }
    dut.io.patch.valid.poke(false.B)
    dut.io.patch.ready.expect(false.B)
    dut.io.configured.expect(true.B)
  }

  private def request(dut: GemminiPatch, tile: Tile, slot: Int = 0,
      output: BigInt = 0x1ffc0, job: Int = 0): Unit = {
    dut.io.request.job.poke(job.U)
    dut.io.request.slot.poke(slot.U)
    pokeTile(dut.io.request.tile, tile)
    dut.io.watch.job.poke(job.U)
    dut.io.watch.slot.poke(slot.U)
    pokeTile(dut.io.watch.tile, tile)
    dut.io.watch.address.poke(output.U)
    dut.io.watch.bytes.poke((tile.rows * tile.columns * 8).U)
  }

  private def start(dut: GemminiPatch): Unit = {
    dut.io.ready.expect(true.B)
    dut.io.start.poke(true.B)
    dut.clock.step()
    dut.io.start.poke(false.B)
    var cycles = 0
    while (!dut.io.ready.peek().litToBoolean && cycles < params.patchCapacity + 1) {
      dut.clock.step()
      cycles += 1
    }
    dut.io.ready.expect(true.B)
  }

  private def check(dut: GemminiPatch, index: Int, rs1: BigInt, rs2: BigInt): Unit = {
    val command = commands(index)
    dut.io.index.poke(index.U)
    pokeCommand(dut.io.command, command)
    dut.io.patched.inst.funct.expect(command.funct.U)
    dut.io.patched.inst.opcode.expect(0x7b.U)
    dut.io.patched.inst.rd.expect(9.U)
    dut.io.patched.inst.rs1.expect(10.U)
    dut.io.patched.inst.rs2.expect(11.U)
    dut.io.patched.inst.xd.expect(false.B)
    dut.io.patched.inst.xs1.expect(true.B)
    dut.io.patched.inst.xs2.expect(true.B)
    dut.io.patched.rs1.expect(rs1.U)
    dut.io.patched.rs2.expect(rs2.U)
  }

  private def copy(dut: GemminiPatch, tile: Tile, view: Tile, bytes: Int,
      address: BigInt = 0x60000): Unit = {
    dut.io.copy.bits.task.poke(0.U)
    dut.io.copy.bits.job.poke(0.U)
    pokeTile(dut.io.copy.bits.tile, tile)
    pokeTile(dut.io.copy.bits.sourceTile, view)
    dut.io.copy.bits.sourceSlot.poke(1.U)
    dut.io.copy.bits.destinationSlot.poke(0.U)
    dut.io.copy.bits.sourceAddress.poke(address.U)
    dut.io.copy.bits.destinationOffset.poke(0.U)
    dut.io.copy.bits.bytes.poke(bytes.U)
    dut.io.copy.bits.destinationBytes.poke(bytes.U)
    dut.io.copy.valid.poke(true.B)
    dut.clock.step()
    dut.io.copy.valid.poke(false.B)
  }

  behavior of "Gemmini field patches"

  it should "bind rectangular tiles without changing unrelated native fields" in {
    test(new GemminiPatch(params)) { dut =>
      init(dut)
      capture(dut)
      for ((tile, slot, top, left, bottom, right, input, output) <- Seq(
        (Tile(5, 0, 0, 2, 2), 0, 1, 1, 0, 0, 0x40000, 0x1ffc0),
        (Tile(8, 0, 2, 2, 2), 1, 1, 0, 0, 0, 0x40003, 0x1ffe0),
        (Tile(11, 2, 4, 1, 1), 0, 0, 0, 1, 1, 0x40018, 0x1ffc0))) {
        request(dut, tile, slot, output)
        start(dut)
        // Changing the next request must not change a stalled command's binding.
        request(dut, Tile(99, 99, 99, 1, 1), 1)
        val expected = commands.updated(2, commands(2).copy(
          rs2 = pack(1 -> 56, 1 -> 48, tile.columns -> 32, tile.rows -> 16, tile.rows -> 0)))
          .updated(3, commands(3).copy(
            rs1 = pack(3 -> 48, tile.columns -> 32, 1 -> 16, 1 -> 8),
            rs2 = pack(1 -> 48, tile.rows -> 32, tile.columns -> 16, 8 -> 0)))
          .updated(4, commands(4).copy(
            rs1 = pack(3 -> 48, 3 -> 32, 3 -> 16, left -> 0),
            rs2 = pack(right -> 48, top -> 32, bottom -> 24, 5 -> 0)))
          .updated(5, commands(5).copy(
            rs1 = pack(tile.rows -> 48, 1 -> 0),
            rs2 = pack(3 -> 48, 8 -> 32, 8 -> 16, tile.columns -> 0)))
          .updated(6, commands(6).copy(rs2 = output))
          .updated(7, commands(7).copy(rs2 = input))
          .updated(9, commands(9).copy(
            rs1 = 0x11110000 | (slot * 64 + 32),
            rs2 = BigInt("222200000000", 16) | (tile.id * 3 - 2)))
        for ((command, index) <- expected.zipWithIndex) {
          check(dut, index, command.rs1, command.rs2)
          dut.clock.step(2)
          check(dut, index, command.rs1, command.rs2)
        }
      }
      capture(dut, Window(rows = 7, columns = 9, rowStep = 2, columnStep = 2, bottom = 3, right = 3))
      request(dut, Tile(3, 2, 3, 1, 1))
      start(dut)
      check(dut, 4, pack(3 -> 48, 3 -> 32, 3 -> 16), pack(2 -> 48, 2 -> 24, 9 -> 0))
      check(dut, 7, 1, 0x40000 + (3 * 9 + 5) * 3)
    }
  }

  it should "bind compact upstream views" in {
    val linked = auto.copy(
      stages = Seq(AutoStageSpec("input", "cgra", 0), AutoStageSpec("conv", "gemmini", 0),
        AutoStageSpec("output", "cgra", 1)),
      dependencies = Seq(AutoDependencySpec(Some(0), 1, Some(AutoCopySpec(0, 0, 64))),
        AutoDependencySpec(Some(1), 2, Some(AutoCopySpec(0, 0, 64)))),
      endpoints = auto.endpoints.map(endpoint =>
        if (endpoint.name == "cgra") endpoint.copy(buffer = Some(AutoBuffer(0x60000, 256))) else endpoint))
    test(new GemminiPatch(params.copy(auto = linked))) { dut =>
      init(dut)
      capture(dut, Window(rows = 7, columns = 9))
      for ((tile, view, offset) <- Seq(
        (Tile(4, 2, 3, 2, 2), Tile(4, 0, 1, 6, 6), 21),
        (Tile(5, 0, 0, 2, 2), Tile(5, 0, 0, 3, 3), 0),
        (Tile(6, 6, 8, 1, 1), Tile(6, 5, 7, 2, 2), 0))) {
        request(dut, tile)
        copy(dut, tile, view, view.rows * view.columns * 3)
        start(dut)
        check(dut, 2, pack(8 -> 48, 3 -> 32, view.rows -> 16, 1 -> 0),
          pack(1 -> 56, 1 -> 48, tile.columns -> 32, tile.rows -> 16, tile.rows -> 0))
        check(dut, 7, 1, 0x60000 + offset)
        dut.io.index.poke(4.U)
        pokeCommand(dut.io.command, commands(4))
        assert((dut.io.patched.rs2.peek().litValue & 65535) == view.columns)
      }
    }
  }

  it should "replace old patches on recapture and preserve ordinary commands" in {
    test(new GemminiPatch(params)) { dut =>
      init(dut)
      val entry = Patch(0, true, 0, 64, GemminiValue.InputAddress)
      val address = (BigInt(1) << 40) + 0x40000
      capture(dut, Window(address = address), Seq(entry))
      request(dut, Tile(1, 0, 0, 1, 1))
      start(dut)
      check(dut, 0, commands.head.rs1, address)
      capture(dut, entries = Seq(Patch(0, true, 0, 2, GemminiValue.Rows)))
      request(dut, Tile(1, 0, 0, 3, 1))
      start(dut)
      check(dut, 0, commands.head.rs1, commands.head.rs2 | 3)
      capture(dut, entries = Nil)
      start(dut)
      check(dut, 0, commands.head.rs1, commands.head.rs2)
    }
  }

  it should "keep different jobs' window and patch entries independent" in {
    val jobs = auto.copy(
      stages = Seq(AutoStageSpec("first", "gemmini", 0), AutoStageSpec("second", "gemmini", 1),
        AutoStageSpec("output", "cgra", 0)),
      dependencies = Seq(AutoDependencySpec(Some(0), 2, Some(AutoCopySpec(0, 0, 64))),
        AutoDependencySpec(Some(1), 2, Some(AutoCopySpec(0, 0, 64)))))
    test(new GemminiPatch(params.copy(auto = jobs))) { dut =>
      init(dut)
      capture(dut)
      capture(dut, Window(address = 0x70000), Seq(Patch(0, true, 0, 64, GemminiValue.InputAddress)), job = 1)
      for (job <- Seq(0, 1, 0)) {
        request(dut, Tile(3, 0, 2, 2, 2), job = job)
        start(dut)
        dut.io.job.poke(job.U)
        if (job == 0) {
          check(dut, 0, commands.head.rs1, commands.head.rs2)
          check(dut, 7, commands(7).rs1, 0x40003)
        } else {
          check(dut, 0, commands.head.rs1, 0x70003)
          check(dut, 7, commands(7).rs1, commands(7).rs2)
        }
      }
    }
  }

  it should "capture patch metadata before commands and hold replay under native backpressure" in {
    test(new GemminiLinkAdapter(params)) { dut =>
      dut.io.configIn.valid.poke(false.B)
      dut.io.configAck.ready.poke(false.B)
      dut.io.patchIn.valid.poke(false.B)
      dut.io.cpuCommand.valid.poke(false.B)
      dut.io.command.ready.poke(false.B)
      dut.io.nativeBusy.poke(false.B)
      dut.io.publication.ready.poke(true.B)
      dut.io.publicationReply.valid.poke(false.B)
      dut.io.publicationReply.bits.result.poke(false.B)
      dut.io.publicationReply.bits.detail.poke(0.U)
      dut.io.autoLink.watchOutput.valid.poke(false.B)
      dut.io.autoLink.reportOutput.ready.poke(false.B)
      dut.io.autoLink.requestCopy.valid.poke(false.B)
      dut.io.autoLink.reportCopy.ready.poke(true.B)
      dut.io.autoLink.requestCompute.valid.poke(false.B)
      dut.io.autoLink.reportCompute.ready.poke(false.B)
      pokeConfig(dut.io.configIn.bits, Window(), patches)
      dut.io.configIn.valid.poke(true.B)
      dut.io.configIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.configIn.valid.poke(false.B)
      dut.io.configAck.valid.expect(true.B)
      dut.io.configAck.bits.done.expect(false.B)
      dut.io.configAck.ready.poke(true.B)
      dut.clock.step()
      dut.io.configAck.ready.poke(false.B)
      for (entry <- patches) {
        dut.io.cpuCommand.ready.expect(false.B)
        dut.io.patchIn.ready.expect(true.B)
        pokePatch(dut.io.patchIn.bits, entry)
        dut.io.patchIn.valid.poke(true.B)
        dut.clock.step()
      }
      dut.io.patchIn.valid.poke(false.B)
      dut.clock.step()
      for (command <- commands) {
        pokeCommand(dut.io.cpuCommand.bits, command)
        dut.io.cpuCommand.valid.poke(true.B)
        dut.io.cpuCommand.ready.expect(true.B)
        dut.io.command.valid.expect(false.B)
        dut.clock.step()
      }
      dut.io.cpuCommand.valid.poke(false.B)
      dut.clock.step()
      dut.io.configAck.valid.expect(true.B)
      dut.io.configAck.bits.done.expect(true.B)
      dut.io.configAck.bits.status.expect(AutoLinkStatus.Success)
      dut.io.configAck.ready.poke(true.B)
      dut.clock.step()
      dut.io.configAck.ready.poke(false.B)

      val tile = Tile(3, 0, 2, 2, 2)
      val watch = dut.io.autoLink.watchOutput
      watch.bits.job.poke(0.U)
      watch.bits.slot.poke(0.U)
      watch.bits.address.poke(0x1ffc0.U)
      watch.bits.bytes.poke(32.U)
      pokeTile(watch.bits.tile, tile)
      watch.valid.poke(true.B)
      watch.ready.expect(true.B)
      val compute = dut.io.autoLink.requestCompute
      compute.bits.job.poke(0.U)
      compute.bits.slot.poke(0.U)
      compute.bits.start.poke(true.B)
      pokeTile(compute.bits.tile, tile)
      compute.valid.poke(true.B)
      compute.ready.expect(false.B)
      dut.clock.step()
      watch.valid.poke(false.B)
      compute.ready.expect(false.B)
      dut.clock.step()
      dut.io.publicationReply.valid.poke(true.B)
      dut.clock.step()
      dut.io.publicationReply.valid.poke(false.B)
      compute.ready.expect(true.B)
      dut.clock.step()
      compute.valid.poke(false.B)
      dut.io.nativeBusy.poke(true.B)
      pokeTile(compute.bits.tile, Tile(99, 99, 99, 1, 1))
      dut.io.command.ready.poke(true.B)
      var cycles = 0
      while (!dut.io.command.valid.peek().litToBoolean && cycles < params.patchCapacity + 2) {
        dut.io.cpuCommand.ready.expect(false.B)
        dut.io.autoBusy.expect(true.B)
        dut.clock.step()
        cycles += 1
      }
      dut.io.command.ready.poke(false.B)
      for ((command, index) <- commands.zipWithIndex) {
        dut.io.command.valid.expect(true.B)
        dut.io.command.bits.inst.funct.expect(command.funct.U)
        val rs1 = dut.io.command.bits.rs1.peek().litValue
        val rs2 = dut.io.command.bits.rs2.peek().litValue
        if (index == 6) dut.io.command.bits.rs2.expect(0x1ffc0.U)
        if (index == 7) dut.io.command.bits.rs2.expect(0x40003.U)
        dut.clock.step(3)
        dut.io.command.valid.expect(true.B)
        dut.io.command.bits.rs1.expect(rs1.U)
        dut.io.command.bits.rs2.expect(rs2.U)
        dut.io.command.ready.poke(true.B)
        dut.clock.step()
        dut.io.command.ready.poke(false.B)
      }
      dut.io.command.valid.expect(false.B)
      dut.io.autoLink.reportCompute.valid.expect(false.B)
      dut.io.publicationReply.bits.result.poke(true.B)
      dut.io.publicationReply.valid.poke(true.B)
      dut.clock.step()
      dut.io.publicationReply.valid.poke(false.B)
      dut.io.autoLink.reportOutput.valid.expect(true.B)
      dut.io.autoLink.reportCompute.valid.expect(false.B)
      dut.io.nativeBusy.poke(false.B)
      dut.clock.step()
      dut.io.autoLink.reportCompute.valid.expect(true.B)
      dut.io.autoLink.reportCompute.bits.status.expect(AutoLinkStatus.Success)
    }
  }
}
