package chipyard.socgen.gemmini

import chisel3._
import chiseltest._
import chipyard.socgen.link._
import freechips.rocketchip.tile.{RoCCCommand, RocketTileParams, TileKey}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class GemminiConvTemplateSpec extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = Parameters.empty.alterPartial {
    case TileKey => RocketTileParams()
  }
  private val auto = AutoLinkParams(
    stages = Seq(AutoStageSpec("conv", "gemmini", 0), AutoStageSpec("relu", "cgra", 0)),
    dependencies = Seq(AutoDependencySpec(Some(0), 1, Some(AutoCopySpec(0, 0, 64)))),
    endpoints = Seq(
      AutoEndpointSpec("gemmini", Some(AutoBuffer(0x10000, 65536)), 65536),
      AutoEndpointSpec("cgra", None, 256)),
    beatBytes = 32,
    controlAddress = 0x30000,
    controlBytes = 4096)
  private val params = GemminiLinkParams(auto, beatBytes = 32, commandCapacity = 16)
  private val native = GemminiConvParams(16, 4096, 512, 1, 0x10000, 65536)
  private case class Command(funct: Int, rs1: BigInt, rs2: BigInt)
  private def pack(fields: (Int, Int)*): BigInt =
    fields.foldLeft(BigInt(0)) { case (value, (field, shift)) => value | (BigInt(field) << shift) }

  private def commands(height: Int = 3, width: Int = 5, stride: Int = 1,
      dilation: Int = 1, input: Int = 0x40000, padding: Int = 1): Seq[Command] = Seq(
    Command(0, pack(stride -> 16, 1 -> 2), pack(1 -> 48)),
    Command(0, 2, pack(0x3f800000 -> 32, 8 -> 0)),
    Command(16, pack(8 -> 48, 3 -> 32, height -> 16, 1 -> 0), pack(padding -> 56, stride -> 48, 1 -> 32, 1 -> 16, 1 -> 0)),
    Command(17, pack(3 -> 48, 1 -> 32, 1 -> 16, 1 -> 8), pack(1 -> 48, 1 -> 32, 1 -> 16, 8 -> 0)),
    Command(18, pack(3 -> 48, 3 -> 32, 3 -> 16, padding -> 0), pack(padding -> 32, width -> 0)),
    Command(19, pack(1 -> 48, dilation -> 0), pack(3 -> 48, 8 -> 32, 8 -> 16, 1 -> 0)),
    Command(20, 0x50000, 0x1ffe0),
    Command(21, 1, input),
    Command(15, pack(1 -> 18, 1 -> 16, 1 -> 8, 1 -> 0), 1))

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

  private def pokeTile(tile: AutoTile, row: Int, column: Int, rows: Int, columns: Int): Unit = {
    tile.id.poke((row * 16 + column).U)
    tile.row.poke(row.U)
    tile.column.poke(column.U)
    tile.rows.poke(rows.U)
    tile.columns.poke(columns.U)
    tile.last.poke(false.B)
  }

  private def begin(dut: GemminiConvTemplate, template: Seq[Command], enabled: Boolean = true): Unit = {
    dut.io.begin.bits.job.poke(0.U)
    dut.io.begin.bits.commandCount.poke(template.size.U)
    dut.io.begin.bits.convTemplate.poke(enabled.B)
    dut.io.begin.valid.poke(true.B)
    dut.clock.step()
    dut.io.begin.valid.poke(false.B)
    for (command <- template) {
      pokeCommand(dut.io.capture.bits, command)
      dut.io.capture.valid.poke(true.B)
      dut.clock.step()
    }
    dut.io.capture.valid.poke(false.B)
  }

  private def init(dut: GemminiConvTemplate): Unit = {
    dut.io.begin.valid.poke(false.B)
    dut.io.capture.valid.poke(false.B)
    dut.io.start.poke(false.B)
    dut.io.request.job.poke(0.U)
    dut.io.request.start.poke(true.B)
    dut.io.watch.job.poke(0.U)
    dut.io.watchValid.poke(true.B)
  }

  private def request(dut: GemminiConvTemplate, row: Int, column: Int, rows: Int,
      columns: Int, output: Int = 0x1ffe0, bytes: Int = -1): Unit = {
    pokeTile(dut.io.request.tile, row, column, rows, columns)
    pokeTile(dut.io.watch.tile, row, column, rows, columns)
    dut.io.watch.address.poke(output.U)
    dut.io.watch.bytes.poke((if (bytes < 0) rows * columns * 8 else bytes).U)
  }

  private def checkCommand(dut: GemminiConvTemplate, original: Command,
      rs1: BigInt, rs2: BigInt): Unit = {
    pokeCommand(dut.io.command, original)
    dut.io.patched.inst.funct.expect(original.funct.U)
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

  behavior of "Gemmini Conv template"

  it should "bind edge and tail tiles from one native capture and preserve every other field" in {
    test(new GemminiConvTemplate(params, native)) { dut =>
      init(dut)
      val template = commands()
      begin(dut, template)
      dut.io.captureValid.expect(true.B)
      for ((row, column, rows, columns, top, left, bottom, right, input, output) <- Seq(
        (0, 0, 2, 2, 1, 1, 0, 0, 0x40000, 0x1ffe0),
        (0, 2, 2, 2, 1, 0, 0, 0, 0x40003, 0x1ffc0),
        (2, 4, 1, 1, 0, 0, 1, 1, 0x40018, 0x1ffe0))) {
        request(dut, row, column, rows, columns, output)
        dut.io.requestValid.expect(true.B)
        dut.io.start.poke(true.B)
        dut.clock.step()
        dut.io.start.poke(false.B)
        // An unaccepted next request must not change the current replay binding.
        request(dut, 99, 99, 1, 1)
        for (original <- template) {
          val expected = original.funct match {
            case 16 => original.copy(rs2 = pack(1 -> 56, 1 -> 48, columns -> 32, rows -> 16, rows -> 0))
            case 17 => original.copy(rs1 = pack(3 -> 48, columns -> 32, 1 -> 16, 1 -> 8), rs2 = pack(1 -> 48, rows -> 32, columns -> 16, 8 -> 0))
            case 18 => original.copy(rs1 = pack(3 -> 48, 3 -> 32, 3 -> 16, left -> 0), rs2 = pack(right -> 48, top -> 32, bottom -> 24, 5 -> 0))
            case 19 => original.copy(rs1 = pack(rows -> 48, 1 -> 0), rs2 = pack(3 -> 48, 8 -> 32, 8 -> 16, columns -> 0))
            case 20 => original.copy(rs2 = output)
            case 21 => original.copy(rs2 = input)
            case _ => original
          }
          checkCommand(dut, original, expected.rs1, expected.rs2)
          dut.clock.step(2)
          checkCommand(dut, original, expected.rs1, expected.rs2)
        }
      }
    }
  }

  it should "use native stride and dilation extents and reject bad geometry or capture" in {
    test(new GemminiConvTemplate(params, native)) { dut =>
      init(dut)
      val template = commands(height = 7, width = 9, stride = 2, dilation = 2)
      begin(dut, template)
      dut.io.captureValid.expect(true.B)
      request(dut, 2, 3, 1, 1)
      dut.io.requestValid.expect(true.B)
      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)
      checkCommand(dut, template(4), pack(3 -> 48, 3 -> 32, 3 -> 16), pack(2 -> 48, 2 -> 24, 9 -> 0))
      checkCommand(dut, template(7), 1, 0x40000 + (3 * 9 + 5) * 3)
      for ((row, column, rows, columns, output, bytes) <- Seq(
        (3, 3, 1, 1, 0x1ffe0, 8),
        (0, 0, 0, 1, 0x1ffe0, 8),
        (0, 0, 1, 1, 0x1ffe0, 32),
        (0, 0, 1, 1, 0x10000, 8),
        (0, 0, 1, 1, 0x20000, 8))) {
        request(dut, row, column, rows, columns, output, bytes)
        dut.io.requestValid.expect(false.B)
      }
      begin(dut, template.dropRight(1))
      dut.io.captureValid.expect(false.B)
      begin(dut, template.updated(7, template(7).copy(rs1 = 0)))
      dut.io.captureValid.expect(false.B)
      begin(dut, template.drop(1))
      dut.io.captureValid.expect(false.B)
      begin(dut, template.patch(1, Nil, 1))
      dut.io.captureValid.expect(false.B)
      begin(dut, template.updated(1, template(1).copy(rs2 = 0x1234)))
      dut.io.captureValid.expect(false.B)
      begin(dut, commands(input = 0x10000))
      dut.io.captureValid.expect(true.B)
      request(dut, 0, 0, 1, 1)
      dut.io.requestValid.expect(false.B)
      begin(dut, commands(height = 7, width = 9, dilation = 3, padding = 3))
      dut.io.captureValid.expect(true.B)
      request(dut, 0, 0, 1, 1)
      dut.io.requestValid.expect(true.B)
      begin(dut, Seq(Command(0, 17, 19)), enabled = false)
      dut.io.captureValid.expect(true.B)
      dut.io.requestValid.expect(true.B)
      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)
      checkCommand(dut, template(6), template(6).rs1, template(6).rs2)
    }
  }

  it should "reject a mismatched watch before replay and hold native commands under backpressure" in {
    test(new GemminiLinkAdapter(params, Some(native))) { dut =>
      dut.io.configIn.valid.poke(false.B)
      dut.io.configAck.ready.poke(false.B)
      dut.io.cpuCommand.valid.poke(false.B)
      dut.io.command.ready.poke(false.B)
      dut.io.nativeBusy.poke(false.B)
      dut.io.event.valid.poke(false.B)
      dut.io.autoLink.watchOutput.valid.poke(false.B)
      dut.io.autoLink.reportOutput.ready.poke(false.B)
      dut.io.autoLink.requestCopy.valid.poke(false.B)
      dut.io.autoLink.reportCopy.ready.poke(true.B)
      dut.io.autoLink.requestCompute.valid.poke(false.B)
      dut.io.autoLink.reportCompute.ready.poke(false.B)
      val template = commands()
      dut.io.configIn.bits.job.poke(0.U)
      dut.io.configIn.bits.commandCount.poke(template.size.U)
      dut.io.configIn.bits.convTemplate.poke(true.B)
      dut.io.configIn.valid.poke(true.B)
      dut.io.configIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.configIn.valid.poke(false.B)
      dut.io.configAck.valid.expect(true.B)
      dut.io.configAck.bits.done.expect(false.B)
      dut.io.configAck.ready.poke(true.B)
      dut.clock.step()
      for (command <- template) {
        pokeCommand(dut.io.cpuCommand.bits, command)
        dut.io.cpuCommand.valid.poke(true.B)
        dut.io.cpuCommand.ready.expect(true.B)
        dut.clock.step()
      }
      dut.io.cpuCommand.valid.poke(false.B)
      dut.io.configAck.ready.poke(false.B)
      dut.clock.step()
      dut.io.configAck.valid.expect(true.B)
      dut.io.configAck.bits.done.expect(true.B)
      dut.io.configAck.bits.status.expect(AutoLinkStatus.Success)
      dut.io.configAck.ready.poke(true.B)
      dut.clock.step()
      for (bytes <- Seq(64, 32)) {
        dut.io.autoLink.watchOutput.bits.job.poke(0.U)
        dut.io.autoLink.watchOutput.bits.address.poke(0x1ffe0.U)
        dut.io.autoLink.watchOutput.bits.bytes.poke(bytes.U)
        pokeTile(dut.io.autoLink.watchOutput.bits.tile, 0, 0, 2, 2)
        dut.io.autoLink.watchOutput.valid.poke(true.B)
        dut.io.autoLink.watchOutput.ready.expect(true.B)
        dut.io.autoLink.requestCompute.bits.job.poke(0.U)
        dut.io.autoLink.requestCompute.bits.start.poke(true.B)
        pokeTile(dut.io.autoLink.requestCompute.bits.tile, 0, 0, 2, 2)
        dut.io.autoLink.requestCompute.valid.poke(true.B)
        dut.io.autoLink.requestCompute.ready.expect(false.B)
        dut.clock.step()
        dut.io.autoLink.watchOutput.valid.poke(false.B)
        dut.io.autoLink.requestCompute.ready.expect(true.B)
        dut.clock.step()
        dut.io.autoLink.requestCompute.valid.poke(false.B)
        if (bytes == 64) {
          dut.io.command.valid.expect(false.B)
          dut.io.autoLink.reportCompute.valid.expect(true.B)
          dut.io.autoLink.reportCompute.bits.detail.expect(GemminiLinkStatus.BadConfig.U)
          dut.io.autoLink.reportOutput.valid.expect(true.B)
          dut.io.autoLink.reportCompute.ready.poke(true.B)
          dut.io.autoLink.reportOutput.ready.poke(true.B)
          dut.clock.step()
          dut.io.autoLink.reportCompute.ready.poke(false.B)
          dut.io.autoLink.reportOutput.ready.poke(false.B)
        }
      }
      for (command <- template.take(3)) {
        dut.io.command.valid.expect(true.B)
        val operand = dut.io.command.bits.rs2.peek().litValue
        dut.clock.step(3)
        dut.io.command.bits.rs2.expect(operand.U)
        dut.io.command.bits.inst.funct.expect(command.funct.U)
        dut.io.command.ready.poke(true.B)
        dut.clock.step()
        dut.io.command.ready.poke(false.B)
      }
    }
  }
}
