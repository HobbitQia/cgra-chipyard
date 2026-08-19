package chipyard.example

import chisel3._
import chisel3.util._
import chiseltest._
import freechips.rocketchip.tilelink.TLMessages
import org.scalatest.flatspec.AnyFlatSpec

class GemminiSpadProducerEndpointHarness extends Module {
  private val endpointParams = SpmTransferEndpointParams.production
  private val producerParams = GemminiSpadProducerAdapterParams.production

  val io = IO(new Bundle {
    val requestIn = Flipped(Decoupled(new SpmTransferRequest))
    val writerA = Flipped(Valid(new GemminiSpadWriterAEvent(producerParams)))
    val writerD = Flipped(Valid(new GemminiSpadWriterDEvent))
    val readyOut = Decoupled(new SpmTransferReady)
    val producerActive = Output(Bool())
    val producerAcknowledgedBytes = Output(UInt(32.W))
  })

  val endpoint = Module(new SpmTransferEndpoint(endpointParams))
  val producer = Module(new GemminiSpadProducerAdapter(producerParams))

  endpoint.io.requestIn <> io.requestIn
  endpoint.io.requestOut <> producer.io.requestIn
  producer.io.readyOut <> endpoint.io.readyIn
  producer.io.writerA <> io.writerA
  producer.io.writerD <> io.writerD
  io.readyOut <> endpoint.io.readyOut

  endpoint.io.readStartIn.valid := false.B
  endpoint.io.readStartIn.bits := 0.U.asTypeOf(new SpmTransferIdentity)
  endpoint.io.releaseIn.valid := false.B
  endpoint.io.releaseIn.bits := 0.U.asTypeOf(new SpmTransferIdentity)
  endpoint.io.errorOut.ready := true.B

  io.producerActive := producer.io.active
  io.producerAcknowledgedBytes := producer.io.acknowledgedBytes
}

class GemminiSpadProducerAdapterSpec extends AnyFlatSpec
    with ChiselScalatestTester {
  private val params = GemminiSpadProducerAdapterParams.production
  private val fullMask = (BigInt(1) << params.fullWidthRowBytes) - 1

  private def initialize(dut: GemminiSpadProducerAdapter): Unit = {
    dut.io.requestIn.valid.poke(false.B)
    dut.io.writerA.valid.poke(false.B)
    dut.io.writerD.valid.poke(false.B)
    dut.io.readyOut.ready.poke(false.B)
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
    dut.clock.step()
  }

  private def request(
    dut: GemminiSpadProducerAdapter,
    jobId: BigInt,
    slot: BigInt,
    bytes: BigInt): Unit = {
    dut.io.requestIn.bits.jobId.poke(jobId.U)
    dut.io.requestIn.bits.slot.poke(slot.U)
    dut.io.requestIn.bits.maxBytes.poke(bytes.U)
    dut.io.requestIn.valid.poke(true.B)
    dut.io.requestIn.ready.expect(true.B)
    dut.clock.step()
    dut.io.requestIn.valid.poke(false.B)
  }

  private def writerA(
    dut: GemminiSpadProducerAdapter,
    address: BigInt,
    source: BigInt = 0,
    size: BigInt = 6,
    opcode: BigInt = TLMessages.PutFullData.litValue,
    mask: BigInt = fullMask): Unit = {
    dut.io.writerA.bits.address.poke(address.U)
    dut.io.writerA.bits.source.poke(source.U)
    dut.io.writerA.bits.size.poke(size.U)
    dut.io.writerA.bits.opcode.poke(opcode.U)
    dut.io.writerA.bits.mask.poke(mask.U)
    dut.io.writerA.valid.poke(true.B)
    dut.clock.step()
    dut.io.writerA.valid.poke(false.B)
  }

  private def writerD(
    dut: GemminiSpadProducerAdapter,
    source: BigInt = 0,
    size: BigInt = 6,
    denied: Boolean = false,
    corrupt: Boolean = false): Unit = {
    dut.io.writerD.bits.source.poke(source.U)
    dut.io.writerD.bits.size.poke(size.U)
    dut.io.writerD.bits.denied.poke(denied.B)
    dut.io.writerD.bits.corrupt.poke(corrupt.B)
    dut.io.writerD.valid.poke(true.B)
    dut.clock.step()
    dut.io.writerD.valid.poke(false.B)
  }

  private def consumeReady(
    dut: GemminiSpadProducerAdapter,
    jobId: BigInt,
    slot: BigInt,
    bytes: BigInt,
    status: BigInt): Unit = {
    dut.io.readyOut.valid.expect(true.B)
    dut.io.readyOut.bits.jobId.expect(jobId.U)
    dut.io.readyOut.bits.slot.expect(slot.U)
    dut.io.readyOut.bits.actualBytes.expect(bytes.U)
    dut.io.readyOut.bits.status.expect(status.U)
    dut.io.readyOut.ready.poke(true.B)
    dut.clock.step()
    dut.io.readyOut.ready.poke(false.B)
    dut.io.readyOut.valid.expect(false.B)
  }

  behavior of "GemminiSpadProducerAdapter"

  it should "emit one stable READY only after the one-row D handshake" in {
    test(new GemminiSpadProducerAdapter(params)) { dut =>
      initialize(dut)
      request(dut, 1, 0, 64)
      dut.io.active.expect(true.B)

      writerA(dut, params.slotBases(0), source = 3)
      dut.io.issuedBytes.expect(64.U)
      dut.io.acknowledgedBytes.expect(0.U)
      dut.io.readyOut.valid.expect(false.B)
      dut.clock.step(5)
      dut.io.readyOut.valid.expect(false.B)

      writerD(dut, source = 3)
      dut.io.acknowledgedBytes.expect(64.U)
      dut.io.readyOut.valid.expect(true.B)
      for (_ <- 0 until 4) {
        dut.io.readyOut.bits.jobId.expect(1.U)
        dut.io.readyOut.bits.slot.expect(0.U)
        dut.io.readyOut.bits.actualBytes.expect(64.U)
        dut.io.readyOut.bits.status.expect(0.U)
        dut.clock.step()
      }
      consumeReady(dut, 1, 0, 64, 0)
      dut.io.active.expect(false.B)
    }
  }

  it should "require all sixteen ordered row acknowledgements and serialize requests" in {
    test(new GemminiSpadProducerAdapter(params)) { dut =>
      initialize(dut)
      request(dut, 2, 1, 1024)

      for (row <- 0 until 16) {
        val address = params.slotBases(1) + row * params.fullWidthRowBytes
        writerA(dut, address, source = row & 3)
        dut.io.requestIn.valid.poke(true.B)
        dut.io.requestIn.bits.jobId.poke(3.U)
        dut.io.requestIn.bits.slot.poke(0.U)
        dut.io.requestIn.bits.maxBytes.poke(64.U)
        dut.io.requestIn.ready.expect(false.B)
        dut.io.readyOut.valid.expect(false.B)
        dut.clock.step(2)
        dut.io.requestIn.valid.poke(false.B)
        writerD(dut, source = row & 3)
        dut.io.acknowledgedBytes.expect(((row + 1) * 64).U)
        if (row < 15) {
          dut.io.readyOut.valid.expect(false.B)
        }
      }

      consumeReady(dut, 2, 1, 1024, 0)
      dut.io.requestIn.ready.expect(true.B)
    }
  }

  it should "return typed failures without false successful bytes" in {
    test(new GemminiSpadProducerAdapter(params)) { dut =>
      import GemminiSpadProducerStatus._

      initialize(dut)

      request(dut, 10, 2, 64)
      consumeReady(dut, 10, 2, 0, UnsupportedRequest)
      request(dut, 11, 0, 16)
      consumeReady(dut, 11, 0, 0, UnsupportedRequest)
      request(dut, 12, 0, 65)
      consumeReady(dut, 12, 0, 0, UnsupportedRequest)

      request(dut, 13, 0, 64)
      writerA(dut, params.slotBases(1), source = 1)
      dut.io.readyOut.valid.expect(false.B)
      writerD(dut, source = 1)
      consumeReady(dut, 13, 0, 0, UnexpectedAddress)

      request(dut, 14, 0, 64)
      writerA(dut, params.slotBases(0), source = 1, mask = fullMask - 1)
      writerD(dut, source = 1)
      consumeReady(dut, 14, 0, 0, UnexpectedSize)

      request(dut, 15, 0, 64)
      writerA(dut, params.slotBases(0), source = 1)
      writerD(dut, source = 1, denied = true)
      consumeReady(dut, 15, 0, 0, Denied)

      request(dut, 16, 0, 64)
      writerA(dut, params.slotBases(0), source = 1)
      writerD(dut, source = 1, corrupt = true)
      consumeReady(dut, 16, 0, 0, Corrupt)

      request(dut, 17, 0, 64)
      writerA(dut, params.slotBases(0), source = 1)
      writerD(dut, source = 2)
      consumeReady(dut, 17, 0, 0, SourceMismatch)

      request(dut, 18, 0, 64)
      writerD(dut, source = 0)
      consumeReady(dut, 18, 0, 0, UnexpectedOrder)
    }
  }

  it should "wait for a real D after an impossible duplicate A sequence" in {
    test(new GemminiSpadProducerAdapter(params)) { dut =>
      import GemminiSpadProducerStatus.UnexpectedOrder

      initialize(dut)
      request(dut, 20, 0, 128)
      writerA(dut, params.slotBases(0), source = 1)
      writerA(dut, params.slotBases(0) + 64, source = 2)
      dut.io.readyOut.valid.expect(false.B)
      writerD(dut, source = 1)
      consumeReady(dut, 20, 0, 0, UnexpectedOrder)
    }
  }

  it should "ignore cycles without dedicated writer events while armed" in {
    test(new GemminiSpadProducerAdapter(params)) { dut =>
      initialize(dut)
      request(dut, 30, 0, 64)
      // Ordinary external-SPAD traffic is on a different diplomacy branch and
      // therefore produces neither writerA nor writerD at this adapter.
      dut.clock.step(12)
      dut.io.issuedBytes.expect(0.U)
      dut.io.acknowledgedBytes.expect(0.U)
      dut.io.readyOut.valid.expect(false.B)
      writerA(dut, params.slotBases(0))
      writerD(dut)
      consumeReady(dut, 30, 0, 64, 0)
    }
  }

  behavior of "Gemmini producer and endpoint integration"

  it should "forward final-D READY through the endpoint exactly once" in {
    test(new GemminiSpadProducerEndpointHarness) { dut =>
      dut.io.requestIn.valid.poke(false.B)
      dut.io.writerA.valid.poke(false.B)
      dut.io.writerD.valid.poke(false.B)
      dut.io.readyOut.ready.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      dut.clock.step()

      dut.io.requestIn.bits.jobId.poke(40.U)
      dut.io.requestIn.bits.slot.poke(0.U)
      dut.io.requestIn.bits.maxBytes.poke(64.U)
      dut.io.requestIn.valid.poke(true.B)
      while (dut.io.requestIn.ready.peek().litValue == 0) {
        dut.clock.step()
      }
      dut.clock.step()
      dut.io.requestIn.valid.poke(false.B)
      while (dut.io.producerActive.peek().litValue == 0) {
        dut.clock.step()
      }

      dut.io.writerA.bits.address.poke(params.slotBases(0).U)
      dut.io.writerA.bits.source.poke(0.U)
      dut.io.writerA.bits.size.poke(6.U)
      dut.io.writerA.bits.opcode.poke(TLMessages.PutFullData)
      dut.io.writerA.bits.mask.poke(fullMask.U)
      dut.io.writerA.valid.poke(true.B)
      dut.clock.step()
      dut.io.writerA.valid.poke(false.B)
      dut.clock.step(3)
      dut.io.readyOut.valid.expect(false.B)

      dut.io.writerD.bits.source.poke(0.U)
      dut.io.writerD.bits.size.poke(6.U)
      dut.io.writerD.bits.denied.poke(false.B)
      dut.io.writerD.bits.corrupt.poke(false.B)
      dut.io.writerD.valid.poke(true.B)
      dut.clock.step()
      dut.io.writerD.valid.poke(false.B)
      while (dut.io.readyOut.valid.peek().litValue == 0) {
        dut.clock.step()
      }
      for (_ <- 0 until 3) {
        dut.io.readyOut.bits.jobId.expect(40.U)
        dut.io.readyOut.bits.slot.expect(0.U)
        dut.io.readyOut.bits.actualBytes.expect(64.U)
        dut.io.readyOut.bits.status.expect(0.U)
        dut.clock.step()
      }
      dut.io.readyOut.ready.poke(true.B)
      dut.clock.step()
      dut.io.readyOut.ready.poke(false.B)
      dut.io.readyOut.valid.expect(false.B)
      dut.clock.step(3)
      dut.io.readyOut.valid.expect(false.B)
    }
  }
}
