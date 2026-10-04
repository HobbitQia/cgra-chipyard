package chipyard.socgen.link

import chisel3._
import chisel3.util._
import chiseltest._
import chipyard.socgen.generated.CgraLinkControlGenerated._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSourceNode, ClockSourceParameters}
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}
import org.scalatest.flatspec.AnyFlatSpec

class AutoRunHarness(params: AutoLinkParams)(implicit p: Parameters) extends LazyModule {
  private val source = ClockSourceNode(Seq(ClockSourceParameters()))
  private val client = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(
    TLMasterParameters.v1("run-test", sourceId = IdRange(0, 1), requestFifo = true)))))
  private val root = LazyModule(new AutoLinkRoot(params))
  private val run = BundleBridgeSink[DecoupledIO[AutoRun]]()
  private val progress = BundleBridgeSource(() => new AutoProgress)
  root.clockNode := source
  root.controlNode := client
  run := root.runNode
  root.stateNode := progress

  override lazy val module = new HarnessImpl
  class HarnessImpl extends LazyModuleImp(AutoRunHarness.this) {
    val io = IO(new Bundle {
      val a = Flipped(Decoupled(new TLBundleA(client.out.head._2.bundle)))
      val d = Decoupled(new TLBundleD(client.out.head._2.bundle))
      val run = Decoupled(new AutoRun(params))
      val progress = Input(new AutoProgress)
    })
    source.out.head._1.clock := clock
    source.out.head._1.reset := reset
    val bus = client.out.head._1
    bus.a <> io.a
    io.d <> bus.d
    bus.b.ready := true.B
    bus.c.valid := false.B
    bus.c.bits := 0.U.asTypeOf(bus.c.bits)
    bus.e.valid := false.B
    bus.e.bits := 0.U.asTypeOf(bus.e.bits)
    io.run <> run.in.head._1
    progress.out.head._1 := io.progress
  }
}

class AutoRunSpec extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = Parameters.empty
  private val params = AutoLinkParams(
    stages = Seq(AutoStageSpec("compute", "ip", 0)),
    dependencies = Seq(AutoDependencySpec(None, 0, None)),
    endpoints = Seq(AutoEndpointSpec("ip", None, 256)),
    beatBytes = 8,
    controlAddress = 0x2000,
    controlBytes = 4096,
    runCapacity = 8)

  behavior of "AutoLinkRoot cached runs"

  it should "repeat sequential intervals after drain, snapshot bounds, and stop on failure" in {
    test(LazyModule(new AutoRunHarness(params)).module) { dut =>
      dut.io.a.valid.poke(false.B)
      dut.io.d.ready.poke(false.B)
      dut.io.run.ready.poke(false.B)
      dut.io.progress.running.poke(false.B)
      dut.io.progress.emitting.poke(false.B)
      dut.io.progress.done.poke(false.B)
      dut.io.progress.failed.poke(false.B)
      dut.io.progress.cycles.poke(0.U)
      dut.io.progress.overlap.poke(0.U)
      dut.io.progress.peakActive.poke(0.U)

      def waitFor(signal: Bool): Unit = {
        var cycles = 0
        while (!signal.peek().litToBoolean && cycles < 40) {
          dut.clock.step()
          cycles += 1
        }
        signal.expect(true.B)
      }

      def write(offset: Int, value: Int): Unit = {
        dut.io.a.bits.opcode.poke(TLMessages.PutFullData)
        dut.io.a.bits.param.poke(0.U)
        dut.io.a.bits.size.poke(3.U)
        dut.io.a.bits.source.poke(0.U)
        dut.io.a.bits.address.poke((params.controlAddress + offset).U)
        dut.io.a.bits.mask.poke(255.U)
        dut.io.a.bits.data.poke(value.U)
        dut.io.a.bits.corrupt.poke(false.B)
        dut.io.a.valid.poke(true.B)
        waitFor(dut.io.a.ready)
        dut.clock.step()
        dut.io.a.valid.poke(false.B)
        waitFor(dut.io.d.valid)
        dut.io.d.bits.denied.expect(false.B)
        dut.io.d.ready.poke(true.B)
        dut.clock.step()
        dut.io.d.ready.poke(false.B)
      }

      for (id <- 1 to 6) {
        write(AUTO_LINK_ROWS, id)
        write(AUTO_LINK_REPEAT_FIRST, if (id == 6) 4 else 1)
        write(AUTO_LINK_REPEAT_COUNT, if (id == 2) 2 else if (id == 6) 3 else 0)
        write(AUTO_LINK_RUN_CAPTURE, id)
      }

      def start(): Unit = {
        write(AUTO_LINK_RUN_FIRST, 1)
        write(AUTO_LINK_RUN_COUNT, 6)
        write(AUTO_LINK_RUN_START, 1)
      }

      def complete(id: Int, iteration: Int, failed: Boolean = false): Unit = {
        waitFor(dut.io.run.valid)
        for (_ <- 0 until 3) {
          dut.io.run.bits.plan.rows.expect(id.U)
          dut.io.run.bits.iteration.expect(iteration.U)
          dut.clock.step()
        }
        dut.io.run.ready.poke(true.B)
        dut.clock.step()
        dut.io.run.ready.poke(false.B)
        dut.io.progress.running.poke(true.B)
        dut.io.run.valid.expect(false.B)
        dut.clock.step(3)
        dut.io.run.valid.expect(false.B)
        dut.io.progress.done.poke(true.B)
        dut.io.progress.failed.poke(failed.B)
        dut.clock.step()
        dut.io.progress.running.poke(false.B)
        dut.io.progress.done.poke(false.B)
        dut.io.progress.failed.poke(false.B)
      }

      start()
      write(AUTO_LINK_RUN_FIRST, 7)
      write(AUTO_LINK_RUN_COUNT, 1)
      write(AUTO_LINK_ROWS, 99)
      val first = for (iteration <- 0 until 2; id <- 1 to 2) yield (id, iteration)
      val second = for (iteration <- 0 until 3; id <- 4 to 6) yield (id, iteration)
      (first ++ Seq((3, 0)) ++ second).foreach { case (id, iteration) => complete(id, iteration) }
      dut.clock.step(5)
      dut.io.run.valid.expect(false.B)

      start()
      first.dropRight(1).foreach { case (id, iteration) => complete(id, iteration) }
      complete(2, 1, failed = true)
      dut.clock.step(5)
      dut.io.run.valid.expect(false.B)

      write(AUTO_LINK_INPUT_READY, 1)
      complete(99, 0)
      dut.clock.step(5)
      dut.io.run.valid.expect(false.B)
    }
  }
}
