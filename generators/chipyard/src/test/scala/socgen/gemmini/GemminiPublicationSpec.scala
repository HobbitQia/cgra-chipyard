package chipyard.socgen.gemmini

import chisel3._
import chisel3.util._
import chiseltest._
import chipyard.socgen.generated.CgraLinkControlGenerated
import chipyard.socgen.link._
import freechips.rocketchip.tile.{RoCCCommand, RocketTileParams, TileKey}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class GemminiErrorHarness(params: GemminiLinkParams) extends Module {
  val io = IO(new Bundle {
    val control = Flipped(Decoupled(new GemminiPublicationControl(params)))
    val reply = Decoupled(new GemminiPublicationReply(params))
    val acks = Input(Vec(2, Valid(new GemminiLinkAck)))
  })
  GemminiPublication(params, io.control, io.reply, io.acks.toSeq)
}

class GemminiPublicationHarness(params: GemminiLinkParams)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val watch = Flipped(Decoupled(new AutoWatch(params.auto)))
    val acks = Input(Vec(2, Valid(new GemminiLinkAck)))
    val config = Flipped(Decoupled(UInt(32.W)))
    val configured = Output(Bool())
    val cpu = Flipped(Decoupled(UInt(7.W)))
    val command = Decoupled(UInt(7.W))
    val compute = Flipped(Decoupled(Bool()))
    val nativeBusy = Input(Bool())
    val stallReply = Input(Bool())
    val output = Decoupled(new AutoEvent(params.auto))
    val complete = Decoupled(new AutoEvent(params.auto))
  })
  val adapter = Module(new GemminiLinkAdapter(params))
  adapter.io.configIn.valid := io.config.valid
  adapter.io.configIn.bits := 0.U.asTypeOf(new GemminiLinkConfig(params))
  adapter.io.configIn.bits.commandCount := io.config.bits
  io.config.ready := adapter.io.configIn.ready
  adapter.io.configAck.ready := true.B
  io.configured := adapter.io.configAck.valid && adapter.io.configAck.bits.done
  adapter.io.patchIn.valid := false.B
  adapter.io.patchIn.bits := 0.U.asTypeOf(new GemminiPatchEntry)
  adapter.io.cpuCommand.valid := io.cpu.valid
  adapter.io.cpuCommand.bits := 0.U.asTypeOf(new RoCCCommand)
  adapter.io.cpuCommand.bits.inst.funct := io.cpu.bits
  adapter.io.cpuCommand.bits.inst.opcode := "h7b".U
  io.cpu.ready := adapter.io.cpuCommand.ready
  io.command.valid := adapter.io.command.valid
  io.command.bits := adapter.io.command.bits.inst.funct
  adapter.io.command.ready := io.command.ready
  adapter.io.nativeBusy := io.nativeBusy
  val reply = Wire(Decoupled(new GemminiPublicationReply(params)))
  adapter.io.publicationReply.valid := reply.valid && !io.stallReply
  adapter.io.publicationReply.bits := reply.bits
  reply.ready := adapter.io.publicationReply.ready && !io.stallReply
  GemminiPublication(params, adapter.io.publication, reply, io.acks.toSeq)
  adapter.io.autoLink.watchOutput <> io.watch
  io.output <> adapter.io.autoLink.reportOutput
  adapter.io.autoLink.requestCopy.valid := false.B
  adapter.io.autoLink.requestCopy.bits := 0.U.asTypeOf(new AutoCopyRequest(params.auto))
  adapter.io.autoLink.requestCompute.valid := io.compute.valid
  adapter.io.autoLink.requestCompute.bits := 0.U.asTypeOf(new AutoComputeRequest(params.auto))
  adapter.io.autoLink.requestCompute.bits.start := io.compute.bits
  io.compute.ready := adapter.io.autoLink.requestCompute.ready
  adapter.io.autoLink.reportCopy.ready := true.B
  io.complete <> adapter.io.autoLink.reportCompute
}

class GemminiPublicationSpec extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = Parameters.empty.alterPartial {
    case TileKey => RocketTileParams()
  }
  private val auto = AutoLinkParams(
    stages = Seq(AutoStageSpec("producer", "gemmini", 0), AutoStageSpec("consumer", "cgra", 0)),
    dependencies = Seq(AutoDependencySpec(Some(0), 1, Some(AutoCopySpec(0, 0, 64)))),
    endpoints = Seq(
      AutoEndpointSpec("gemmini", Some(AutoBuffer(0x2000, 128)), 128),
      AutoEndpointSpec("cgra", None, 128)),
    beatBytes = 32,
    controlAddress = 0x3000,
    controlBytes = 4096)
  private val params = GemminiLinkParams(auto, commandCapacity = 2)

  private def clearAcks(acks: Vec[ValidIO[GemminiLinkAck]]): Unit = {
    acks.foreach { ack =>
      ack.valid.poke(false.B)
      ack.bits.denied.poke(false.B)
      ack.bits.corrupt.poke(false.B)
    }
  }

  private class Driver(dut: GemminiPublicationHarness) {
    dut.io.watch.valid.poke(false.B)
    dut.io.watch.bits.job.poke(0.U)
    dut.io.watch.bits.address.poke(0x2000.U)
    dut.io.watch.bits.bytes.poke(64.U)
    dut.io.config.valid.poke(false.B)
    dut.io.config.bits.poke(2.U)
    dut.io.cpu.valid.poke(false.B)
    dut.io.cpu.bits.poke(0.U)
    dut.io.command.ready.poke(true.B)
    dut.io.compute.valid.poke(false.B)
    dut.io.compute.bits.poke(true.B)
    dut.io.nativeBusy.poke(false.B)
    dut.io.stallReply.poke(false.B)
    dut.io.output.ready.poke(false.B)
    dut.io.complete.ready.poke(false.B)
    clearAcks(dut.io.acks)

    def waitFor(signal: Bool): Unit = {
      var cycles = 0
      while (!signal.peek().litToBoolean && cycles < 32) {
        dut.clock.step()
        cycles += 1
      }
      signal.expect(true.B)
    }

    def watch(): Unit = {
      dut.io.watch.valid.poke(true.B)
      waitFor(dut.io.watch.ready)
      dut.clock.step()
      dut.io.watch.valid.poke(false.B)
      dut.clock.step(3)
    }

    def command(funct: Int): Unit = {
      dut.io.cpu.bits.poke(funct.U)
      dut.io.cpu.valid.poke(true.B)
      waitFor(dut.io.cpu.ready)
      dut.clock.step()
      dut.io.cpu.valid.poke(false.B)
    }

    def capture(): Unit = {
      dut.io.config.valid.poke(true.B)
      waitFor(dut.io.config.ready)
      dut.clock.step()
      dut.io.config.valid.poke(false.B)
      command(2)
      command(3)
      waitFor(dut.io.configured)
      dut.clock.step()
    }

    def compute(start: Boolean = true): Unit = {
      dut.io.compute.bits.poke(start.B)
      dut.io.compute.valid.poke(true.B)
      waitFor(dut.io.compute.ready)
      dut.clock.step()
      dut.io.compute.valid.poke(false.B)
    }

    def replay(): Unit = {
      for (funct <- Seq(2, 3)) {
        waitFor(dut.io.command.valid)
        dut.io.command.bits.expect(funct.U)
        dut.clock.step()
      }
    }

    def result(port: DecoupledIO[AutoEvent], detail: Int = 0): Unit = {
      waitFor(port.valid)
      for (_ <- 0 until 3) {
        port.valid.expect(true.B)
        port.bits.status.expect(if (detail == 0) AutoLinkStatus.Success else AutoLinkStatus.SourceFailure)
        port.bits.detail.expect(detail.U)
        dut.clock.step()
      }
      port.ready.poke(true.B)
      dut.clock.step()
      port.ready.poke(false.B)
      port.valid.expect(false.B)
    }
  }

  behavior of "Gemmini publication"

  it should "hold error replies, include the final acknowledgement and clear each new job" in {
    test(new GemminiErrorHarness(params)) { dut =>
      clearAcks(dut.io.acks)
      dut.io.control.valid.poke(false.B)
      dut.io.reply.ready.poke(false.B)
      for ((port, detail) <- Seq(0 -> GemminiLinkStatus.Denied, 1 -> GemminiLinkStatus.Corrupt, 0 -> 0)) {
        dut.io.control.bits.enable.poke(true.B)
        dut.io.control.valid.poke(true.B)
        dut.io.control.ready.expect(true.B)
        dut.clock.step()
        dut.io.control.valid.poke(false.B)
        dut.io.reply.valid.expect(true.B)
        dut.io.reply.bits.detail.expect(0.U)
        dut.io.reply.ready.poke(true.B)
        dut.clock.step()
        dut.io.reply.ready.poke(false.B)

        dut.io.control.bits.enable.poke(false.B)
        dut.io.control.valid.poke(true.B)
        dut.io.acks(port).valid.poke(true.B)
        dut.io.acks(port).bits.denied.poke((detail == GemminiLinkStatus.Denied).B)
        dut.io.acks(port).bits.corrupt.poke((detail == GemminiLinkStatus.Corrupt).B)
        dut.clock.step()
        dut.io.control.valid.poke(false.B)
        clearAcks(dut.io.acks)
        for (_ <- 0 until 3) {
          dut.io.control.ready.expect(false.B)
          dut.io.reply.valid.expect(true.B)
          dut.io.reply.bits.detail.expect(detail.U)
          dut.clock.step()
        }
        dut.io.reply.ready.poke(true.B)
        dut.clock.step()
        dut.io.reply.ready.poke(false.B)
      }
    }
  }

  it should "require a CPU command boundary and native drain before reporting an output" in {
    test(new GemminiPublicationHarness(params)) { dut =>
      val driver = new Driver(dut)
      driver.watch()
      driver.command(2)
      dut.io.nativeBusy.poke(true.B)
      dut.clock.step(3)
      dut.io.nativeBusy.poke(false.B)
      dut.clock.step(4)
      dut.io.output.valid.expect(false.B)
      driver.command(3)
      dut.io.nativeBusy.poke(true.B)
      driver.command(CgraLinkControlGenerated.GEMMINI_COMMAND_END)
      dut.io.command.valid.expect(false.B)
      dut.clock.step(3)
      dut.io.output.valid.expect(false.B)

      dut.io.acks(0).valid.poke(true.B)
      dut.io.acks(0).bits.denied.poke(true.B)
      dut.clock.step()
      clearAcks(dut.io.acks)
      dut.io.stallReply.poke(true.B)
      dut.io.nativeBusy.poke(false.B)
      dut.clock.step(6)
      dut.io.output.valid.expect(false.B)
      dut.io.stallReply.poke(false.B)
      driver.result(dut.io.output, GemminiLinkStatus.Denied)
      dut.io.complete.valid.expect(false.B)
    }
  }

  it should "drain replayed work and preserve independent compute and output reports" in {
    test(new GemminiPublicationHarness(params)) { dut =>
      val driver = new Driver(dut)
      driver.capture()
      for (outputFirst <- Seq(false, true)) {
        driver.watch()
        driver.compute()
        dut.io.cpu.bits.poke(4.U)
        dut.io.cpu.valid.poke(true.B)
        dut.io.cpu.ready.expect(false.B)
        driver.replay()
        dut.io.nativeBusy.poke(true.B)
        dut.clock.step(3)
        dut.io.cpu.ready.expect(false.B)
        dut.io.command.valid.expect(false.B)
        dut.io.output.valid.expect(false.B)
        dut.io.complete.valid.expect(false.B)
        dut.io.nativeBusy.poke(false.B)
        if (outputFirst) {
          driver.result(dut.io.output)
          driver.result(dut.io.complete)
        } else {
          driver.result(dut.io.complete)
          driver.result(dut.io.output)
        }
        dut.io.cpu.valid.poke(false.B)
        dut.clock.step()
      }
    }
  }

  it should "complete an automatic job without a watched output and discard cancelled watches" in {
    test(new GemminiPublicationHarness(params)) { dut =>
      val driver = new Driver(dut)
      driver.capture()
      driver.compute()
      driver.replay()
      driver.result(dut.io.complete)
      dut.io.output.valid.expect(false.B)

      driver.watch()
      driver.compute(start = false)
      dut.clock.step(8)
      dut.io.output.valid.expect(false.B)
      dut.io.complete.valid.expect(false.B)
      driver.watch()
      driver.command(CgraLinkControlGenerated.GEMMINI_COMMAND_END)
      driver.result(dut.io.output)
    }
  }
}
