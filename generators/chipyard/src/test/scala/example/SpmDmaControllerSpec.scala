package chipyard.example

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SpmDmaControllerSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val params = SpmDmaParams(slotCount = 2, slotSizeBytes = 1024, beatBytes = 16)

  private def init(dut: SpmDmaController): Unit = {
    dut.io.jobIn.valid.poke(false.B)
    dut.io.stepIn.valid.poke(false.B)
    dut.io.producerStart.ready.poke(true.B)
    dut.io.producerDone.valid.poke(false.B)
    dut.io.transferStart.ready.poke(true.B)
    dut.io.transferDone.valid.poke(false.B)
    dut.io.consumerStart.ready.poke(true.B)
    dut.io.consumerDone.valid.poke(false.B)
    dut.io.resultOut.ready.poke(false.B)
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
  }

  private def pokeJob(dut: SpmDmaController, mode: UInt): Unit = {
    dut.io.jobIn.bits.jobId.poke(7.U)
    dut.io.jobIn.bits.slot.poke(1.U)
    dut.io.jobIn.bits.bytes.poke(128.U)
    dut.io.jobIn.bits.mode.poke(mode)
  }

  private def pokeCommand(command: SpmDmaCommand): Unit = {
    command.jobId.poke(7.U)
    command.slot.poke(1.U)
    command.bytes.poke(128.U)
  }

  private def expectCommand(command: SpmDmaCommand): Unit = {
    command.jobId.expect(7.U)
    command.slot.expect(1.U)
    command.bytes.expect(128.U)
  }

  private def pokeSuccess(result: SpmDmaResult, stage: UInt, data: Int = 0): Unit = {
    result.jobId.poke(7.U)
    result.slot.poke(1.U)
    result.bytes.poke(128.U)
    result.stage.poke(stage)
    result.status.poke(SpmDmaStatus.Success)
    result.detail.poke(0.U)
    result.data.poke(data.U)
  }

  private def submit(dut: SpmDmaController, mode: UInt): Unit = {
    pokeJob(dut, mode)
    dut.io.jobIn.valid.poke(true.B)
    dut.io.producerStart.valid.expect(true.B)
    dut.clock.step()
    dut.io.jobIn.valid.poke(false.B)
  }

  behavior of "SpmDmaController"

  it should "run all stages automatically in order" in {
    test(new SpmDmaController(params)) { dut =>
      init(dut)
      submit(dut, SpmDmaMode.Auto)

      pokeSuccess(dut.io.producerDone.bits, SpmDmaStage.Producer)
      dut.io.producerDone.valid.poke(true.B)
      dut.clock.step()
      dut.io.producerDone.valid.poke(false.B)
      dut.io.transferStart.valid.expect(true.B)
      expectCommand(dut.io.transferStart.bits)
      dut.clock.step()

      pokeSuccess(dut.io.transferDone.bits, SpmDmaStage.Transfer)
      dut.io.transferDone.valid.poke(true.B)
      dut.clock.step()
      dut.io.transferDone.valid.poke(false.B)
      dut.io.consumerStart.valid.expect(true.B)
      expectCommand(dut.io.consumerStart.bits)
      dut.clock.step()

      pokeSuccess(dut.io.consumerDone.bits, SpmDmaStage.Consumer, data = 23)
      dut.io.consumerDone.valid.poke(true.B)
      dut.clock.step()
      dut.io.consumerDone.valid.poke(false.B)
      dut.io.resultOut.valid.expect(true.B)
      dut.io.resultOut.bits.status.expect(SpmDmaStatus.Success)
      dut.io.resultOut.bits.data.expect(23.U)
    }
  }

  it should "advance manual jobs only on protocol steps" in {
    test(new SpmDmaController(params)) { dut =>
      init(dut)
      submit(dut, SpmDmaMode.Manual)

      pokeSuccess(dut.io.producerDone.bits, SpmDmaStage.Producer)
      dut.io.producerDone.valid.poke(true.B)
      dut.clock.step()
      dut.io.producerDone.valid.poke(false.B)
      dut.io.transferStart.valid.expect(false.B)
      pokeCommand(dut.io.stepIn.bits)
      dut.io.stepIn.valid.poke(true.B)
      dut.clock.step()
      dut.io.stepIn.valid.poke(false.B)
      dut.io.transferStart.valid.expect(true.B)
      dut.clock.step()

      pokeSuccess(dut.io.transferDone.bits, SpmDmaStage.Transfer)
      dut.io.transferDone.valid.poke(true.B)
      dut.clock.step()
      dut.io.transferDone.valid.poke(false.B)
      dut.io.consumerStart.valid.expect(false.B)
      pokeCommand(dut.io.stepIn.bits)
      dut.io.stepIn.valid.poke(true.B)
      dut.clock.step()
      dut.io.stepIn.valid.poke(false.B)
      dut.io.consumerStart.valid.expect(true.B)
    }
  }
}
