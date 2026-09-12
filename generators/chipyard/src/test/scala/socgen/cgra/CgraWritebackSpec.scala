package chipyard.socgen.cgra

import chisel3._
import chiseltest._
import chipyard.example.{CGRADMAParams, CGRAParams, CGRASpmReadParams, CGRASpmWindowParams}
import chipyard.socgen.link._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable

class CgraWritebackSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val spm = CGRASpmReadParams(enabled = true, addrWidth = 8, dataWidth = 32, words = 256)
  private val cgra = CGRAParams(
    dma = CGRADMAParams(enabled = true, dramAddrWidth = 64, dramDataWidth = 128, dramMaskWidth = 16),
    spmRead = spm)
  private val auto = AutoLinkParams(
    stages = Seq(AutoStageSpec("output", "cgra", 0)),
    dependencies = Seq(AutoDependencySpec(None, 0, None)),
    endpoints = Seq(AutoEndpointSpec("cgra", None, 1024, bufferSlots = 2)),
    beatBytes = 16, controlAddress = 0x60020000L, controlBytes = 4096)
  private val params = CgraLinkParams(auto, cgra, packetCapacity = 8)
  private val scale = CgraRequantParams(1, 0)
  private val packed = CGRASpmWindowParams(0x60010000L, 256,
    Some(CgraTensorBridgeParams(0, 256, 0, 0, scale, scale)))

  private def init(dut: CgraWriteback): Unit = {
    dut.io.request.valid.poke(false.B)
    dut.io.done.ready.poke(false.B)
    dut.io.spm.req.ready.poke(false.B)
    dut.io.spm.resp.valid.poke(false.B)
    dut.io.spm.resp.bits.poke(0.U)
    dut.io.writeReq.ready.poke(false.B)
    dut.io.writeResp.valid.poke(false.B)
    dut.io.writeResp.bits.poke(false.B)
  }

  private def request(dut: CgraWriteback, rows: Int, columns: Int, channels: Int,
      address: BigInt, stride: Int, word: Int = 16): Unit = {
    dut.io.request.bits.config.enabled.poke(true.B)
    dut.io.request.bits.config.address.poke(address.U)
    dut.io.request.bits.config.word.poke(word.U)
    dut.io.request.bits.config.slotStride.poke(48.U)
    dut.io.request.bits.config.channels.poke(channels.U)
    dut.io.request.bits.config.rowStride.poke(stride.U)
    dut.io.request.bits.tile.id.poke(4.U)
    dut.io.request.bits.slot.poke(1.U)
    dut.io.request.bits.tile.row.poke(1.U)
    dut.io.request.bits.tile.column.poke(1.U)
    dut.io.request.bits.tile.rows.poke(rows.U)
    dut.io.request.bits.tile.columns.poke(columns.U)
    dut.io.request.bits.tile.last.poke(true.B)
    dut.io.request.valid.poke(true.B)
    dut.io.request.ready.expect(true.B)
    dut.clock.step()
    dut.io.request.valid.poke(false.B)
  }

  private def drain(dut: CgraWriteback, values: Seq[Int], packedOutput: Boolean,
      firstAddress: BigInt, rowBytes: Int, rows: Int, stride: Int, fail: Boolean = false): Unit = {
    val bytes = mutable.Map.empty[BigInt, Int]
    val reads = mutable.ArrayBuffer.empty[Int]
    var response: Option[(Int, Int)] = None
    var acknowledgement: Option[Int] = None
    var heldWrite: Option[(BigInt, BigInt, BigInt)] = None
    var cycle = 0
    while (!dut.io.done.valid.peek().litToBoolean && cycle < 800) {
      dut.io.spm.req.ready.poke((cycle % 3 != 0).B)
      dut.io.spm.resp.valid.poke(response.exists(_._2 == 0).B)
      response.foreach { case (index, _) =>
        dut.io.spm.resp.bits.poke((BigInt(values(index)) & 0xffffffffL).U)
      }
      dut.io.writeReq.ready.poke((cycle % 5 == 0).B)
      dut.io.writeResp.valid.poke(acknowledgement.contains(0).B)
      dut.io.writeResp.bits.poke(fail.B)

      val readFire = dut.io.spm.req.valid.peek().litToBoolean && dut.io.spm.req.ready.peek().litToBoolean
      val responseFire = dut.io.spm.resp.valid.peek().litToBoolean && dut.io.spm.resp.ready.peek().litToBoolean
      val writeFire = dut.io.writeReq.valid.peek().litToBoolean && dut.io.writeReq.ready.peek().litToBoolean
      val ackFire = dut.io.writeResp.valid.peek().litToBoolean && dut.io.writeResp.ready.peek().litToBoolean
      if (dut.io.writeReq.valid.peek().litToBoolean) {
        val current = (dut.io.writeReq.bits.address.peek().litValue,
          dut.io.writeReq.bits.data.peek().litValue, dut.io.writeReq.bits.mask.peek().litValue)
        heldWrite.foreach(previous => assert(previous == current))
        heldWrite = if (writeFire) None else Some(current)
      }
      response = response.map { case (index, delay) => (index, math.max(0, delay - 1)) }
      acknowledgement = acknowledgement.map(delay => math.max(0, delay - 1))
      if (readFire) {
        assert(response.isEmpty)
        reads += dut.io.spm.req.bits.peek().litValue.toInt
        response = Some((reads.size - 1, 2))
      }
      if (responseFire) { response = None }
      if (writeFire) {
        assert(acknowledgement.isEmpty)
        val address = dut.io.writeReq.bits.address.peek().litValue
        val data = dut.io.writeReq.bits.data.peek().litValue
        val mask = dut.io.writeReq.bits.mask.peek().litValue
        assert(address % 16 == 0 && mask != 0)
        for (lane <- 0 until 16 if mask.testBit(lane)) {
          assert(!bytes.contains(address + lane))
          bytes(address + lane) = ((data >> (lane * 8)) & 255).toInt
        }
        acknowledgement = Some(4)
      }
      if (ackFire) { acknowledgement = None }
      dut.clock.step()
      cycle += 1
    }
    assert(cycle < 800 && acknowledgement.isEmpty && response.isEmpty)
    dut.io.done.bits.expect(if (fail) AutoLinkStatus.SinkFailure else AutoLinkStatus.Success)
    assert(reads == (64 until 64 + reads.size))
    if (!fail) {
      val expected = values.flatMap { value =>
        if (packedOutput) Seq(math.max(-128, math.min(127, value)) & 255)
        else (0 until 4).map(lane => (value >>> (lane * 8)) & 255)
      }
      assert(reads.size == values.size && bytes.size == expected.size)
      for (row <- 0 until rows; offset <- 0 until rowBytes) {
        assert(bytes(firstAddress + row * stride + offset) == expected(row * rowBytes + offset))
      }
    }
    dut.io.spm.resp.valid.poke(false.B)
    dut.io.writeResp.valid.poke(false.B)
    dut.io.request.ready.expect(false.B)
    dut.io.busy.expect(true.B)
    dut.clock.step(3)
    dut.io.done.valid.expect(true.B)
    dut.io.spm.req.valid.expect(false.B)
    dut.io.writeReq.valid.expect(false.B)
    dut.io.done.ready.poke(true.B)
    dut.clock.step()
    dut.io.done.ready.poke(false.B)
    dut.io.busy.expect(false.B)
  }

  behavior of "CgraWriteback"

  it should "pack strided INT8 tiles with partial beats and drain acknowledgements before reuse" in {
    test(new CgraWriteback(params, packed)) { dut =>
      init(dut)
      val values = (0 until 30).map(index => (index - 15) * 17)
      request(dut, 2, 3, 5, 0x1003, 40)
      drain(dut, values, packedOutput = true, firstAddress = 0x1030, rowBytes = 15, rows = 2, stride = 40)
      request(dut, 1, 2, 3, 0x2001, 16)
      drain(dut, values.take(6), packedOutput = true, firstAddress = 0x2014, rowBytes = 6, rows = 1, stride = 16)
      request(dut, 2, 3, 5, 0x1003, 40)
      drain(dut, values, packedOutput = true, firstAddress = 0x1030, rowBytes = 15, rows = 2, stride = 40, fail = true)
    }
  }

  it should "retain raw words in strided output rows" in {
    test(new CgraWriteback(params, CGRASpmWindowParams(0x60010000L, 1024))) { dut =>
      init(dut)
      request(dut, 2, 2, 2, 0x1004, 32)
      drain(dut, Seq(1, -2, 3, 4, 5, -6, 7, 8), packedOutput = false,
        firstAddress = 0x102c, rowBytes = 16, rows = 2, stride = 32)
    }
  }

  it should "lock SPM response ownership across response backpressure" in {
    test(new CgraSpmReadArbiter(spm)) { dut =>
      dut.io.spm.req.ready.poke(true.B)
      dut.io.spm.resp.valid.poke(false.B)
      dut.io.spm.resp.bits.poke(123.U)
      for (index <- 0 until 2) {
        dut.io.clients(index).req.valid.poke(true.B)
        dut.io.clients(index).req.bits.poke((10 + index).U)
        dut.io.clients(index).resp.ready.poke(false.B)
        dut.io.clients(index).busy.poke(true.B)
      }
      val first = dut.io.spm.req.bits.peek().litValue.toInt - 10
      dut.clock.step()
      dut.io.clients(first).req.valid.poke(false.B)
      dut.io.spm.resp.valid.poke(true.B)
      dut.io.spm.req.valid.expect(false.B)
      dut.io.clients(first).resp.valid.expect(true.B)
      dut.io.clients(1 - first).resp.valid.expect(false.B)
      dut.clock.step(3)
      dut.io.clients(first).resp.bits.expect(123.U)
      dut.io.clients(first).resp.ready.poke(true.B)
      dut.clock.step()
      dut.io.spm.resp.valid.poke(false.B)
      dut.io.spm.req.bits.expect((11 - first).U)
      dut.io.spm.req.valid.expect(true.B)
    }
  }
}
