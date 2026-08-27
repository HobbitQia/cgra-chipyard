// See LICENSE for license details

package aes

import chisel3._
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams}
import roccaccutils.{DstInfo, StreamInfo}

class AesJob extends Bundle {
  val source = new StreamInfo
  val key = UInt(AES256Consts.KEY_SZ_BITS.W)
  val encrypt = Bool()
  val destination = new DstInfo
}

class AesJobAsyncLink extends Bundle {
  private val crossing = AsyncQueueParams.singleton()
  val job = new AsyncBundle(new AesJob, crossing)
  val inputReadDone = Flipped(new AsyncBundle(Bool(), crossing))
  val jobDone = Flipped(new AsyncBundle(Bool(), crossing))
}
