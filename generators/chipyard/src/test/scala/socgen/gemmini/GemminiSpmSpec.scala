package chipyard.socgen.gemmini

import chisel3._
import chisel3.util._
import chiseltest._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSourceNode, ClockSourceParameters}
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}
import org.scalatest.flatspec.AnyFlatSpec

class GemminiSpmPort(params: TLBundleParameters) extends Bundle {
  val a = Flipped(Decoupled(new TLBundleA(params)))
  val d = Decoupled(new TLBundleD(params))
}

class GemminiSpmHarness(implicit p: Parameters) extends LazyModule {
  private val source = ClockSourceNode(Seq(ClockSourceParameters()))
  private val clients = Seq.fill(3)(TLClientNode(Seq(TLMasterPortParameters.v1(Seq(
    TLMasterParameters.v1("spm-test", sourceId = IdRange(0, 8), requestFifo = true))))))
  private val spm = LazyModule(new GemminiExternalSpm(
    GemminiExternalSpmParams(0x2000, 1024), 1, 16, 64, 32))
  spm.clockNode := source
  spm.readNodes.head := clients(0)
  spm.writeNodes.head := clients(1)
  spm.systemNodes.head := clients(2)

  override lazy val module = new HarnessImpl
  class HarnessImpl extends LazyModuleImp(GemminiSpmHarness.this) {
    val io = IO(new Bundle {
      val read = new GemminiSpmPort(clients(0).out.head._2.bundle)
      val write = new GemminiSpmPort(clients(1).out.head._2.bundle)
      val system = new GemminiSpmPort(clients(2).out.head._2.bundle)
    })
    source.out.head._1.clock := clock
    source.out.head._1.reset := reset
    for ((client, port) <- clients.zip(Seq(io.read, io.write, io.system))) {
      val bus = client.out.head._1
      bus.a <> port.a
      port.d <> bus.d
      bus.b.ready := true.B
      bus.c.valid := false.B
      bus.c.bits := 0.U.asTypeOf(bus.c.bits)
      bus.e.valid := false.B
      bus.e.bits := 0.U.asTypeOf(bus.e.bits)
    }
  }
}

class GemminiSpmSpec extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = Parameters.empty

  private def request(port: GemminiSpmPort, address: Int, source: Int, bytes: Int,
      data: Option[BigInt] = None, mask: Option[BigInt] = None): Unit = {
    port.a.valid.poke(true.B)
    port.a.bits.opcode.poke(if (data.isEmpty) TLMessages.Get else if (mask.isDefined) TLMessages.PutPartialData else TLMessages.PutFullData)
    port.a.bits.param.poke(0.U)
    port.a.bits.size.poke(log2Ceil(bytes).U)
    port.a.bits.source.poke(source.U)
    port.a.bits.address.poke(address.U)
    port.a.bits.mask.poke(mask.getOrElse((BigInt(1) << bytes) - 1).U)
    port.a.bits.data.poke(data.getOrElse(BigInt(0)).U)
    port.a.bits.corrupt.poke(false.B)
  }

  private def response(port: GemminiSpmPort, source: Int, bytes: Int, data: Option[BigInt] = None): Unit = {
    port.d.valid.expect(true.B)
    port.d.bits.source.expect(source.U)
    port.d.bits.size.expect(log2Ceil(bytes).U)
    port.d.bits.opcode.expect(if (data.isDefined) TLMessages.AccessAckData else TLMessages.AccessAck)
    port.d.bits.denied.expect(false.B)
    port.d.bits.corrupt.expect(false.B)
    data.foreach(value => port.d.bits.data.expect(value.U))
  }

  private def pattern(start: Int, bytes: Int): BigInt =
    (0 until bytes).foldLeft(BigInt(0))((value, byte) => value | (BigInt((start + byte) & 255) << (8 * byte)))

  behavior of "Gemmini shared SPM"

  it should "pipeline requests and retain ordered SRAM returns under backpressure" in {
    test(LazyModule(new GemminiSpmHarness).module) { dut =>
      val ports = Seq(dut.io.read, dut.io.write, dut.io.system)
      ports.foreach { port =>
        port.a.valid.poke(false.B)
        port.d.ready.poke(true.B)
      }
      dut.clock.step()

      for (line <- 0 until 4) {
        request(dut.io.write, 0x2000 + line * 64, line, 64, Some(pattern(line * 64, 64)))
        dut.io.write.a.ready.expect(true.B)
        if (line > 0) response(dut.io.write, line - 1, 64)
        dut.clock.step()
      }
      dut.io.write.a.valid.poke(false.B)
      response(dut.io.write, 3, 64)
      dut.clock.step()

      for (row <- 0 until 8) {
        request(dut.io.read, 0x2000 + row * 16, row, 16)
        dut.io.read.a.ready.expect(true.B)
        if (row > 0) response(dut.io.read, row - 1, 16, Some(pattern((row - 1) * 16, 16)))
        dut.clock.step()
      }
      dut.io.read.a.valid.poke(false.B)
      response(dut.io.read, 7, 16, Some(pattern(112, 16)))
      dut.clock.step()

      dut.io.read.d.ready.poke(false.B)
      for (row <- 0 until 2) {
        request(dut.io.read, 0x2080 + row * 16, row, 16)
        dut.io.read.a.ready.expect(true.B)
        dut.clock.step()
      }
      request(dut.io.read, 0x20a0, 2, 16)
      request(dut.io.system, 0x2080, 0, 32, Some(pattern(19, 32)))
      dut.io.system.a.ready.expect(true.B)
      for (cycle <- 0 until 4) {
        dut.io.read.a.ready.expect(false.B)
        response(dut.io.read, 0, 16, Some(pattern(128, 16)))
        if (cycle == 1) response(dut.io.system, 0, 32)
        dut.clock.step()
        dut.io.system.a.valid.poke(false.B)
      }
      dut.io.read.d.ready.poke(true.B)
      dut.io.read.a.ready.expect(true.B)
      dut.clock.step()
      dut.io.read.a.valid.poke(false.B)
      response(dut.io.read, 1, 16, Some(pattern(144, 16)))
      dut.clock.step()
      response(dut.io.read, 2, 16, Some(pattern(160, 16)))
      dut.clock.step()

      request(dut.io.system, 0x2020, 0, 32)
      dut.io.system.a.ready.expect(true.B)
      dut.clock.step()
      request(dut.io.system, 0x2060, 1, 32, Some(pattern(7, 32)), Some(0xffff))
      dut.io.system.a.ready.expect(true.B)
      response(dut.io.system, 0, 32, Some(pattern(32, 32)))
      dut.clock.step()
      request(dut.io.system, 0x2060, 2, 32)
      dut.io.system.a.ready.expect(true.B)
      response(dut.io.system, 1, 32)
      dut.clock.step()
      dut.io.system.a.valid.poke(false.B)
      response(dut.io.system, 2, 32, Some(pattern(7, 16) | (pattern(112, 16) << 128)))
      dut.clock.step()

      // A same-cycle write wins over a read of its SRAM line.
      request(dut.io.read, 0x2000, 0, 16)
      request(dut.io.write, 0x2000, 0, 64, Some(pattern(9, 64)))
      dut.io.read.a.ready.expect(false.B)
      dut.io.write.a.ready.expect(true.B)
      dut.clock.step()
      dut.io.write.a.valid.poke(false.B)
      dut.io.read.a.ready.expect(true.B)
      dut.clock.step()
      dut.io.read.a.valid.poke(false.B)
      request(dut.io.system, 0x2000, 0, 32, Some(pattern(17, 32)))
      dut.io.system.a.ready.expect(true.B)
      response(dut.io.read, 0, 16, Some(pattern(9, 16)))
      dut.clock.step()
      dut.io.system.a.valid.poke(false.B)
      response(dut.io.system, 0, 32)
      dut.clock.step()

      dut.io.write.d.ready.poke(false.B)
      for (line <- 0 until 2) {
        request(dut.io.write, 0x2100 + line * 64, line, 64, Some(pattern(line, 64)))
        dut.io.write.a.ready.expect(true.B)
        dut.clock.step()
      }
      request(dut.io.write, 0x2180, 2, 64, Some(pattern(2, 64)))
      for (_ <- 0 until 3) {
        dut.io.write.a.ready.expect(false.B)
        response(dut.io.write, 0, 64)
        dut.clock.step()
      }
      dut.io.write.d.ready.poke(true.B)
      dut.io.write.a.ready.expect(true.B)
      dut.clock.step()
      dut.io.write.a.valid.poke(false.B)
      for (source <- 1 to 2) {
        response(dut.io.write, source, 64)
        dut.clock.step()
      }

      dut.io.system.d.ready.poke(false.B)
      request(dut.io.system, 0x20c0, 0, 32)
      dut.io.system.a.ready.expect(true.B)
      dut.clock.step()
      request(dut.io.system, 0x20e0, 1, 32, Some(pattern(41, 32)), Some(0xffff))
      dut.io.system.a.ready.expect(true.B)
      dut.clock.step()
      request(dut.io.system, 0x20e0, 2, 32)
      for (_ <- 0 until 3) {
        dut.io.system.a.ready.expect(false.B)
        response(dut.io.system, 0, 32, Some(pattern(192, 32)))
        dut.clock.step()
      }
      dut.io.system.d.ready.poke(true.B)
      dut.io.system.a.ready.expect(true.B)
      dut.clock.step()
      dut.io.system.a.valid.poke(false.B)
      response(dut.io.system, 1, 32)
      dut.clock.step()
      response(dut.io.system, 2, 32, Some(pattern(41, 16) | (pattern(240, 16) << 128)))
      dut.clock.step()
    }
  }
}
