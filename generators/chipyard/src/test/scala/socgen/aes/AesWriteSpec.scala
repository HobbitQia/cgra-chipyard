package chipyard.socgen.aes

import chisel3._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import roccaccutils.{L2MemHelperParams, MemWriter32}
import scala.collection.mutable

class AesWriteSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "AES byte writer"

  it should "keep low-byte-first chunks through partial writes, lane rotation and stalls" in {
    implicit val p: Parameters = Parameters.empty
    implicit val hp: L2MemHelperParams = L2MemHelperParams()
    test(new MemWriter32(cmd_que_depth = 2)) { dut =>
      dut.io.memwrites_in.valid.poke(false.B)
      dut.io.decompress_dest_info.valid.poke(false.B)
      dut.io.l2io.req.ready.poke(false.B)
      dut.io.l2io.resp.valid.poke(false.B)
      dut.io.l2io.resp.bits.data.poke(0.U)
      dut.io.l2io.no_memops_inflight.poke(false.B)

      for ((length, job) <- Seq(48, 16).zipWithIndex) {
        val destination = 0x1003 + job * 128
        val completion = 0x2000 + job
        val bytes = (0 until length).map(index => (index * 7 + job * 43 + 1) & 255)
        dut.io.decompress_dest_info.bits.op.poke(destination.U)
        dut.io.decompress_dest_info.bits.cmpflag.poke(completion.U)
        dut.io.decompress_dest_info.valid.poke(true.B)
        dut.io.decompress_dest_info.ready.expect(true.B)
        dut.clock.step()
        dut.io.decompress_dest_info.valid.poke(false.B)
        for ((chunk, index) <- bytes.grouped(32).toSeq.zipWithIndex) {
          val data = chunk.zipWithIndex.foldLeft(BigInt(0)) { case (value, (byte, lane)) =>
            value | (BigInt(byte) << (lane * 8))
          }
          dut.io.memwrites_in.bits.data.poke(data.U)
          dut.io.memwrites_in.bits.validbytes.poke(chunk.size.U)
          dut.io.memwrites_in.bits.end_of_message.poke((index == (length - 1) / 32).B)
          dut.io.memwrites_in.valid.poke(true.B)
          dut.io.memwrites_in.ready.expect(true.B)
          dut.clock.step()
        }
        dut.io.memwrites_in.valid.poke(false.B)

        val written = mutable.Map.empty[BigInt, Int]
        var held: Option[(BigInt, BigInt, BigInt)] = None
        var finished = false
        var cycles = 0
        while (!finished && cycles < 200) {
          val ready = cycles % 3 == 2
          dut.io.l2io.req.ready.poke(ready.B)
          if (dut.io.l2io.req.valid.peek().litToBoolean) {
            val address = dut.io.l2io.req.bits.addr.peek().litValue
            val size = dut.io.l2io.req.bits.size.peek().litValue
            val data = dut.io.l2io.req.bits.data.peek().litValue
            val request = (address, size, data)
            held.foreach(previous => assert(previous == request))
            held = if (ready) None else Some(request)
            if (ready) {
              if (address == completion) {
                assert(size == 0 && data == 1)
                finished = true
              } else {
                for (lane <- 0 until (1 << size.toInt)) {
                  assert(!written.contains(address + lane))
                  written(address + lane) = ((data >> (lane * 8)) & 255).toInt
                }
              }
            }
          }
          dut.clock.step()
          cycles += 1
        }
        assert(finished)
        assert(written.toMap == bytes.zipWithIndex.map { case (byte, index) =>
          BigInt(destination + index) -> byte
        }.toMap)
        dut.io.bufs_completed.expect((job + 1).U)
        dut.io.no_writes_inflight.expect(false.B)
        dut.io.l2io.no_memops_inflight.poke(true.B)
        dut.io.no_writes_inflight.expect(true.B)
        dut.io.l2io.req.ready.poke(false.B)
        dut.clock.step(2)
        dut.io.l2io.no_memops_inflight.poke(false.B)
      }
    }
  }
}
