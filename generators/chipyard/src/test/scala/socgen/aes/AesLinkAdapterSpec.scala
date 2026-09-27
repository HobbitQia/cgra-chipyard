package chipyard.socgen.aes

import chisel3._
import chiseltest._
import chipyard.socgen.link._
import org.scalatest.flatspec.AnyFlatSpec

class AesLinkAdapterSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val auto = AutoLinkParams(
    stages = Seq(AutoStageSpec("decrypt", "aes", 0), AutoStageSpec("consumer", "cgra", 0)),
    dependencies = Seq(
      AutoDependencySpec(None, 0, Some(AutoCopySpec(0, 0, 64, sourceAddress = Some(0x1000)))),
      AutoDependencySpec(Some(0), 1, Some(AutoCopySpec(0, 0, 64)))),
    endpoints = Seq(
      AutoEndpointSpec("aes", Some(AutoBuffer(0x2000, 256)), 256),
      AutoEndpointSpec("cgra", None, 256)),
    beatBytes = 16,
    controlAddress = 0x3000,
    controlBytes = 4096)

  behavior of "AES publication"

  it should "finish downstream compute and publication independently while preserving external roots" in {
    test(new AesLinkAdapter(AesLinkParams(auto))) { dut =>
      val port = dut.io.autoLink
      dut.io.configIn.valid.poke(false.B)
      dut.io.configIn.bits.job.poke(0.U)
      dut.io.configIn.bits.start.poke(false.B)
      dut.io.configIn.bits.descriptor.source.ip.poke(0x1000.U)
      dut.io.configIn.bits.descriptor.source.isize.poke(64.U)
      dut.io.configIn.bits.descriptor.destination.op.poke(0x2000.U)
      dut.io.configIn.bits.descriptor.destination.cmpflag.poke(0x4000.U)
      dut.io.configIn.bits.descriptor.key.poke(0.U)
      dut.io.configIn.bits.descriptor.encrypt.poke(false.B)
      dut.io.job.ready.poke(true.B)
      dut.io.inputReadDone.valid.poke(false.B)
      dut.io.inputReadDone.bits.poke(false.B)
      dut.io.jobDone.valid.poke(false.B)
      dut.io.jobDone.bits.poke(false.B)
      port.watchOutput.valid.poke(false.B)
      port.watchOutput.bits.job.poke(0.U)
      port.reportOutput.ready.poke(false.B)
      port.requestCopy.valid.poke(false.B)
      port.requestCopy.bits.job.poke(0.U)
      port.requestCopy.bits.task.poke(0.U)
      port.requestCopy.bits.sourceAddress.poke(0x1000.U)
      port.requestCopy.bits.bytes.poke(64.U)
      port.reportCopy.ready.poke(true.B)
      port.requestCompute.valid.poke(false.B)
      port.requestCompute.bits.start.poke(true.B)
      port.reportCompute.ready.poke(false.B)

      def arm(writeback: Boolean): Unit = {
        port.watchOutput.bits.writeback.poke(writeback.B)
        port.watchOutput.bits.address.poke(0x2800.U)
        port.watchOutput.valid.poke(true.B)
        port.watchOutput.ready.expect(true.B)
        dut.clock.step()
        port.watchOutput.valid.poke(false.B)
      }

      def complete(): Unit = {
        dut.io.inputReadDone.valid.poke(true.B)
        dut.io.inputReadDone.ready.expect(true.B)
        dut.clock.step()
        dut.io.inputReadDone.valid.poke(false.B)
        dut.clock.step()
        dut.io.jobDone.valid.poke(true.B)
        dut.io.jobDone.ready.expect(true.B)
        dut.clock.step()
        dut.io.jobDone.valid.poke(false.B)
        port.reportOutput.valid.expect(true.B)
      }

      dut.io.configIn.valid.poke(true.B)
      dut.io.configIn.ready.expect(true.B)
      dut.clock.step()
      dut.io.configIn.valid.poke(false.B)

      for (outputFirst <- Seq(true, false)) {
        arm(outputFirst)
        port.requestCopy.valid.poke(true.B)
        port.requestCopy.ready.expect(true.B)
        dut.io.job.bits.destination.op.expect((if (outputFirst) 0x2800 else 0x2000).U)
        dut.io.job.bits.destination.cmpflag.expect(0x4000.U)
        dut.clock.step()
        port.requestCopy.valid.poke(false.B)
        complete()
        if (outputFirst) {
          port.reportOutput.ready.poke(true.B)
          dut.clock.step()
          port.reportOutput.ready.poke(false.B)
        }
        dut.io.configIn.ready.expect(false.B)
        port.requestCompute.ready.expect(true.B)
        port.requestCompute.valid.poke(true.B)
        dut.clock.step()
        port.requestCompute.valid.poke(false.B)
        port.reportCompute.valid.expect(true.B)
        dut.clock.step(2)
        port.reportCompute.valid.expect(true.B)
        port.reportCompute.ready.poke(true.B)
        dut.clock.step()
        port.reportCompute.ready.poke(false.B)
        if (!outputFirst) {
          dut.io.configIn.ready.expect(false.B)
          port.reportOutput.valid.expect(true.B)
          port.reportOutput.ready.poke(true.B)
          dut.clock.step()
          port.reportOutput.ready.poke(false.B)
        }
        dut.io.configIn.ready.expect(true.B)
      }

      for (writeback <- Seq(true, false)) {
        arm(writeback)
        dut.io.configIn.bits.start.poke(true.B)
        dut.io.configIn.valid.poke(true.B)
        dut.io.configIn.ready.expect(true.B)
        dut.clock.step()
        dut.io.configIn.valid.poke(false.B)
        dut.io.job.valid.expect(true.B)
        dut.io.job.bits.destination.op.expect((if (writeback) 0x2800 else 0x2000).U)
        dut.io.job.bits.destination.cmpflag.expect(0x4000.U)
        dut.clock.step()
        complete()
        port.reportCompute.valid.expect(false.B)
        port.reportOutput.ready.poke(true.B)
        dut.clock.step()
        port.reportOutput.ready.poke(false.B)
        dut.io.configIn.bits.start.poke(false.B)
        dut.io.configIn.ready.expect(true.B)
      }
    }
  }
}
