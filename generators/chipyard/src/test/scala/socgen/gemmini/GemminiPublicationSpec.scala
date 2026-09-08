package chipyard.socgen.gemmini

import chisel3._
import chisel3.util._
import chiseltest._
import chipyard.socgen.link._
import freechips.rocketchip.tile.{RoCCCommand, RocketTileParams, TileKey}
import freechips.rocketchip.tilelink.TLMessages
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class GemminiPublicationHarness(params: GemminiLinkParams)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val watch = Flipped(Decoupled(new AutoWatch(params.auto)))
    val writes = Input(Vec(2, Valid(new GemminiLinkWrite(params))))
    val acks = Input(Vec(2, Valid(new GemminiLinkAck)))
    val cancel = Input(Bool())
    val cancelReady = Output(Bool())
    val stallReply = Input(Bool())
    val result = Decoupled(new AutoEvent(params.auto))
  })
  val adapter = Module(new GemminiLinkAdapter(params))
  adapter.io.configIn.valid := false.B
  adapter.io.configIn.bits := 0.U.asTypeOf(new GemminiLinkConfig(params))
  adapter.io.configAck.ready := true.B
  adapter.io.cpuCommand.valid := false.B
  adapter.io.cpuCommand.bits := 0.U.asTypeOf(new RoCCCommand)
  adapter.io.command.ready := true.B
  adapter.io.nativeBusy := false.B
  val reply = Wire(Decoupled(new GemminiPublicationReply(params)))
  adapter.io.publicationReply.valid := reply.valid && !io.stallReply
  adapter.io.publicationReply.bits := reply.bits
  reply.ready := adapter.io.publicationReply.ready && !io.stallReply
  GemminiPublication(params, adapter.io.publication, reply,
    io.writes.toSeq, io.acks.toSeq)
  adapter.io.autoLink.watchOutput <> io.watch
  io.result <> adapter.io.autoLink.reportOutput
  adapter.io.autoLink.requestCopy.valid := false.B
  adapter.io.autoLink.requestCopy.bits := 0.U.asTypeOf(new AutoCopyRequest(params.auto))
  adapter.io.autoLink.requestCompute.valid := io.cancel
  io.cancelReady := adapter.io.autoLink.requestCompute.ready
  adapter.io.autoLink.requestCompute.bits := 0.U.asTypeOf(new AutoComputeRequest(params.auto))
  adapter.io.autoLink.reportCopy.ready := true.B
  adapter.io.autoLink.reportCompute.ready := true.B
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
  private val params = GemminiLinkParams(auto, beatBytes = 32, commandCapacity = 1)

  private class Driver(dut: GemminiPublicationHarness) {
    dut.io.watch.valid.poke(false.B)
    dut.io.watch.bits.job.poke(0.U)
    dut.io.cancel.poke(false.B)
    dut.io.stallReply.poke(false.B)
    for (index <- 0 until 2) {
      dut.io.writes(index).valid.poke(false.B)
      dut.io.acks(index).valid.poke(false.B)
      dut.io.acks(index).bits.denied.poke(false.B)
      dut.io.acks(index).bits.corrupt.poke(false.B)
    }
    dut.io.result.ready.poke(false.B)

    def watch(address: Int, bytes: Int): Unit = {
      dut.io.watch.bits.address.poke(address.U)
      dut.io.watch.bits.bytes.poke(bytes.U)
      dut.io.watch.valid.poke(true.B)
      dut.io.watch.ready.expect(true.B)
      dut.clock.step()
      dut.io.watch.valid.poke(false.B)
      dut.clock.step(3)
    }

    def write(address: Int, size: Int, mask: BigInt, partial: Boolean = true,
        source: Int = 7, local: Boolean = false): Unit = {
      val request = dut.io.writes(if (local) 1 else 0)
      request.bits.source.poke(source.U)
      request.bits.address.poke(address.U)
      request.bits.size.poke(size.U)
      request.bits.mask.poke(mask.U)
      request.bits.opcode.poke(if (partial) TLMessages.PutPartialData else TLMessages.PutFullData)
      request.valid.poke(true.B)
      dut.clock.step()
      request.valid.poke(false.B)
      for (_ <- 0 until 3) {
        dut.io.result.valid.expect(false.B)
        dut.clock.step()
      }
    }

    def ack(size: Int, source: Int = 7, local: Boolean = false): Unit = {
      val response = dut.io.acks(if (local) 1 else 0)
      response.bits.source.poke(source.U)
      response.bits.size.poke(size.U)
      response.valid.poke(true.B)
      dut.clock.step()
      response.valid.poke(false.B)
      dut.clock.step()
    }

    def result(detail: Int = 0): Unit = {
      for (_ <- 0 until 3) {
        dut.io.result.valid.expect(true.B)
        dut.io.result.bits.status.expect(if (detail == 0) AutoLinkStatus.Success else AutoLinkStatus.SourceFailure)
        dut.io.result.bits.detail.expect(detail.U)
        dut.clock.step()
      }
      dut.io.result.ready.poke(true.B)
      dut.clock.step()
      dut.io.result.ready.poke(false.B)
      dut.io.result.valid.expect(false.B)
    }
  }

  behavior of "Gemmini publication"

  it should "count a shifted INT8 row and partial tail only after matching acknowledgements" in {
    test(new GemminiPublicationHarness(params)) { dut =>
      val driver = new Driver(dut)
      driver.watch(0x2010, 19)
      driver.write(0x2000, 5, BigInt("ffff0000", 16))
      driver.ack(5)
      dut.io.result.valid.expect(false.B)
      driver.write(0x2020, 2, 7)
      driver.ack(2)
      driver.result()
      driver.watch(0x2010, 16)
      driver.write(0x2010, 4, BigInt("ffff0000", 16), partial = false)
      driver.ack(4)
      driver.result()
    }
  }

  it should "preserve full-width publication and reject a wrong address or acknowledgement size" in {
    test(new GemminiPublicationHarness(params)) { dut =>
      val driver = new Driver(dut)
      val full = BigInt("ffffffff", 16)
      driver.watch(0x2000, 64)
      driver.write(0x2000, 5, full, partial = false)
      driver.ack(5)
      dut.io.result.valid.expect(false.B)
      driver.write(0x2020, 5, full, partial = false)
      driver.ack(5)
      driver.result()
      driver.watch(0x2000, 32)
      driver.write(0x2020, 5, full, partial = false)
      driver.ack(5)
      driver.result(GemminiLinkStatus.BadAddress)
      driver.watch(0x2000, 16)
      driver.write(0x2000, 4, 0xffff, partial = false)
      driver.ack(5)
      driver.result(GemminiLinkStatus.BadBeat)
    }
  }

  it should "reject holes, masks outside the request, overruns and duplicate outstanding writes" in {
    test(new GemminiPublicationHarness(params)) { dut =>
      val driver = new Driver(dut)
      for ((bytes, size, mask, detail) <- Seq(
        (3, 2, 5, GemminiLinkStatus.BadBeat),
        (3, 1, 7, GemminiLinkStatus.BadBeat),
        (2, 2, 7, GemminiLinkStatus.BadOrder))) {
        driver.watch(0x2000, bytes)
        driver.write(0x2000, size, mask)
        driver.ack(size)
        driver.result(detail)
      }
      driver.watch(0x2000, 32)
      driver.write(0x2000, 4, 0xffff, partial = false)
      driver.write(0x2010, 4, BigInt("ffff0000", 16), partial = false)
      driver.ack(4)
      driver.result(GemminiLinkStatus.BadOrder)
    }
  }

  it should "match reordered writes and acknowledgements across source namespaces and reuse slots" in {
    test(new GemminiPublicationHarness(params.copy(maxInflight = 3))) { dut =>
      val driver = new Driver(dut)
      val lower = BigInt("ffff", 16)
      val upper = BigInt("ffff0000", 16)
      driver.watch(0x2000, 64)
      driver.write(0x2030, 4, upper, partial = false, source = 7)
      driver.write(0x2000, 4, lower, partial = false, source = 7, local = true)
      driver.write(0x2020, 4, lower, partial = false, source = 8)
      driver.ack(4, source = 8)
      dut.io.result.valid.expect(false.B)
      driver.write(0x2010, 4, upper, partial = false, source = 8)
      driver.ack(4, source = 7, local = true)
      dut.io.result.valid.expect(false.B)
      driver.ack(4, source = 8)
      dut.io.result.valid.expect(false.B)
      driver.ack(4, source = 7)
      driver.result()
    }
  }

  it should "reject repeated byte coverage before and after acknowledgement" in {
    test(new GemminiPublicationHarness(params.copy(maxInflight = 2))) { dut =>
      val driver = new Driver(dut)
      val full = BigInt("ffffffff", 16)
      driver.watch(0x2000, 64)
      driver.write(0x2000, 5, full, partial = false, source = 1)
      driver.write(0x2000, 5, full, partial = false, source = 2, local = true)
      driver.ack(5, source = 1)
      dut.io.result.valid.expect(false.B)
      driver.ack(5, source = 2, local = true)
      driver.result(GemminiLinkStatus.BadOrder)

      driver.watch(0x2000, 64)
      driver.write(0x2000, 5, full, partial = false, source = 1)
      driver.ack(5, source = 1)
      dut.io.result.valid.expect(false.B)
      driver.write(0x2000, 5, full, partial = false, source = 1)
      driver.ack(5, source = 1)
      driver.result(GemminiLinkStatus.BadOrder)
    }
  }

  it should "drain other sources after a failed acknowledgement before arming a new watch" in {
    test(new GemminiPublicationHarness(params.copy(maxInflight = 2))) { dut =>
      val driver = new Driver(dut)
      val full = BigInt("ffffffff", 16)
      driver.watch(0x2000, 64)
      driver.write(0x2000, 5, full, partial = false, source = 1)
      driver.write(0x2020, 5, full, partial = false, source = 2, local = true)
      driver.ack(4, source = 1)
      driver.result(GemminiLinkStatus.BadBeat)
      driver.ack(5, source = 2, local = true)
      dut.io.result.valid.expect(false.B)
      driver.watch(0x2000, 32)
      driver.write(0x2000, 5, full, partial = false, source = 2, local = true)
      driver.ack(5, source = 2, local = true)
      driver.result()
    }
  }

  it should "accept both ports every cycle while retiring and reusing both source IDs" in {
    test(new GemminiPublicationHarness(params.copy(maxInflight = 2))) { dut =>
      val driver = new Driver(dut)
      driver.watch(0x2000, 64)
      for (cycle <- 0 until 2) {
        for (index <- 0 until 2) {
          val request = dut.io.writes(index)
          val response = dut.io.acks(index)
          val offset = cycle * 32 + index * 16
          request.valid.poke(true.B)
          request.bits.address.poke((0x2000 + offset).U)
          request.bits.source.poke(7.U)
          request.bits.size.poke(4.U)
          request.bits.opcode.poke(TLMessages.PutFullData)
          request.bits.mask.poke((BigInt(0xffff) << (index * 16)).U)
          response.valid.poke((cycle != 0).B)
          response.bits.source.poke(7.U)
          response.bits.size.poke(4.U)
        }
        dut.io.result.valid.expect(false.B)
        dut.clock.step()
      }
      dut.io.writes.foreach(_.valid.poke(false.B))
      dut.clock.step()
      dut.io.acks.foreach(_.valid.poke(false.B))
      dut.clock.step()
      driver.result()
    }
  }

  it should "drain a canceled watch before arming the next publication" in {
    test(new GemminiPublicationHarness(params)) { dut =>
      val driver = new Driver(dut)
      val full = BigInt("ffffffff", 16)
      driver.watch(0x2000, 64)
      driver.write(0x2000, 5, full, partial = false)
      dut.io.cancel.poke(true.B)
      dut.io.cancelReady.expect(true.B)
      dut.clock.step()
      dut.io.cancel.poke(false.B)
      dut.clock.step(3)
      dut.io.watch.ready.expect(false.B)
      driver.ack(5)
      dut.clock.step(3)
      dut.io.result.valid.expect(false.B)
      driver.watch(0x2000, 32)
      driver.write(0x2000, 5, full, partial = false)
      driver.ack(5)
      driver.result()
    }
  }

  it should "discard simultaneous cancellation and final acknowledgement under reply backpressure" in {
    test(new GemminiPublicationHarness(params)) { dut =>
      val driver = new Driver(dut)
      val full = BigInt("ffffffff", 16)
      driver.watch(0x2000, 32)
      driver.write(0x2000, 5, full, partial = false)
      dut.io.cancel.poke(true.B)
      dut.io.cancelReady.expect(true.B)
      dut.clock.step()
      dut.io.cancel.poke(false.B)
      dut.io.stallReply.poke(true.B)
      driver.ack(5)
      dut.clock.step(3)
      dut.io.watch.ready.expect(false.B)
      dut.io.result.valid.expect(false.B)
      dut.io.stallReply.poke(false.B)
      dut.clock.step(3)
      driver.watch(0x2000, 32)
      dut.io.result.valid.expect(false.B)
      driver.write(0x2000, 5, full, partial = false)
      driver.ack(5)
      driver.result()
    }
  }

  it should "give cancellation priority over an arriving publication result" in {
    test(new GemminiPublicationHarness(params)) { dut =>
      val driver = new Driver(dut)
      val full = BigInt("ffffffff", 16)
      driver.watch(0x2000, 32)
      driver.write(0x2000, 5, full, partial = false)
      val response = dut.io.acks(0)
      response.valid.poke(true.B)
      response.bits.source.poke(7.U)
      response.bits.size.poke(5.U)
      dut.clock.step()
      response.valid.poke(false.B)
      dut.io.cancel.poke(true.B)
      dut.io.cancelReady.expect(true.B)
      dut.clock.step()
      dut.io.cancel.poke(false.B)
      dut.clock.step(4)
      dut.io.result.valid.expect(false.B)
      driver.watch(0x2000, 32)
      driver.write(0x2000, 5, full, partial = false)
      driver.ack(5)
      driver.result()
    }
  }
}
