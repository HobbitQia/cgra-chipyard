package chipyard.socgen.aes

import chisel3._
import chisel3.util._
import chipyard.socgen.link._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.subsystem.{BaseSubsystem, InstantiatesHierarchicalElements, PBUS}
import freechips.rocketchip.tile.RocketTile
import freechips.rocketchip.util.{AsyncQueueParams, FromAsyncBundle, ToAsyncBundle}
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import org.chipsalliance.diplomacy.lazymodule.LazyModule

case class AesLinkParams(
  auto: AutoLinkParams,
  key: BigInt,
  encrypt: Boolean,
  ciphertextAddress: BigInt,
  completionAddress: BigInt) {
  require(key > 0 && key.bitLength <= 256)
  require(ciphertextAddress >= 0 && ciphertextAddress.bitLength <= auto.addressWidth)
  require(completionAddress >= 0 && completionAddress.bitLength <= auto.addressWidth)
}

case class AesLinkAttachParams(adapter: AesLinkParams, portName: String) {
  require(adapter.auto.endpoints.exists(_.name == portName))
}

case object AesLinkKey extends Field[Option[AesLinkAttachParams]](None)

class WithAesLink(params: AesLinkAttachParams) extends Config((_, _, _) => { case AesLinkKey => Some(params) })

class AesLinkAdapter(params: AesLinkParams) extends Module {
  val io = IO(new Bundle {
    val autoLink = new AutoEndpointIO(params.auto)
    val job = Decoupled(new _root_.aes.AesJob)
    val inputReadDone = Flipped(Decoupled(Bool()))
    val jobDone = Flipped(Decoupled(Bool()))
  })

  val active = RegInit(false.B)
  val copy = Reg(new AutoCopyRequest(params.auto))
  val readDone = RegInit(false.B)
  val copyReported = RegInit(false.B)
  val done = RegInit(false.B)
  val computeAccepted = RegInit(false.B)

  io.autoLink.watchOutput.ready := false.B
  io.autoLink.reportOutput.valid := false.B
  io.autoLink.reportOutput.bits := 0.U.asTypeOf(new AutoEvent(params.auto))

  io.job.valid := io.autoLink.requestCopy.valid && !active
  io.job.bits.source.ip := io.autoLink.requestCopy.bits.sourceAddress
  io.job.bits.source.isize := io.autoLink.requestCopy.bits.bytes
  // Caliptra consumes the first key byte from the low UInt byte.
  val keyBytes = params.key.U(_root_.aes.AES256Consts.KEY_SZ_BITS.W)
    .asTypeOf(Vec(_root_.aes.AES256Consts.KEY_SZ_BYTES, UInt(8.W)))
  io.job.bits.key := Cat(keyBytes)
  io.job.bits.encrypt := params.encrypt.B
  io.job.bits.destination.op := params.ciphertextAddress.U
  io.job.bits.destination.cmpflag := params.completionAddress.U
  io.autoLink.requestCopy.ready := io.job.ready && !active

  io.inputReadDone.ready := active && !readDone
  io.jobDone.ready := active && !done

  io.autoLink.reportCopy.valid := active && readDone && !copyReported
  io.autoLink.reportCopy.bits.task := copy.task
  io.autoLink.reportCopy.bits.status := AutoLinkStatus.Success
  io.autoLink.reportCopy.bits.detail := 0.U

  io.autoLink.requestCompute.ready := Mux(
    io.autoLink.requestCompute.bits.start,
    active && copyReported && !computeAccepted,
    !active)
  io.autoLink.reportCompute.valid := active && computeAccepted && done
  io.autoLink.reportCompute.bits.status := AutoLinkStatus.Success
  io.autoLink.reportCompute.bits.detail := 0.U
  io.autoLink.reportCompute.bits.data := 0.U

  when(io.autoLink.requestCopy.fire) {
    active := true.B
    copy := io.autoLink.requestCopy.bits
    readDone := false.B
    copyReported := false.B
    done := false.B
    computeAccepted := false.B
  }
  when(io.inputReadDone.fire) {
    readDone := true.B
  }
  when(io.jobDone.fire) {
    done := true.B
  }
  when(io.autoLink.reportCopy.fire) {
    copyReported := true.B
  }
  when(io.autoLink.requestCompute.fire) {
    when(io.autoLink.requestCompute.bits.start) {
      computeAccepted := true.B
    }
  }
  when(io.autoLink.reportCompute.fire) {
    active := false.B
    readDone := false.B
    copyReported := false.B
    done := false.B
    computeAccepted := false.B
  }
}

class AesLinkEndpoint(params: AesLinkParams)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  val autoNode = BundleBridgeSink[AutoEndpointAsyncLink]()
  val jobNode = BundleBridgeSource(() => new _root_.aes.AesJobAsyncLink)

  override lazy val module = new EndpointImpl
  class EndpointImpl extends Impl {
    withClockAndReset(clock, reset) {
      val adapter = Module(new AesLinkAdapter(params))
      val auto = autoNode.in.head._1
      val job = jobNode.out.head._1

      adapter.io.autoLink.watchOutput <> FromAsyncBundle(auto.watchOutput)
      auto.reportOutput <> ToAsyncBundle(adapter.io.autoLink.reportOutput, AsyncQueueParams.singleton())
      adapter.io.autoLink.requestCopy <> FromAsyncBundle(auto.requestCopy)
      auto.reportCopy <> ToAsyncBundle(adapter.io.autoLink.reportCopy, AsyncQueueParams.singleton())
      adapter.io.autoLink.requestCompute <> FromAsyncBundle(auto.requestCompute)
      auto.reportCompute <> ToAsyncBundle(adapter.io.autoLink.reportCompute, AsyncQueueParams.singleton())

      job.job <> ToAsyncBundle(adapter.io.job, AsyncQueueParams.singleton())
      adapter.io.inputReadDone <> FromAsyncBundle(job.inputReadDone)
      adapter.io.jobDone <> FromAsyncBundle(job.jobDone)
    }
  }
}

trait CanHaveAesLink {
  this: BaseSubsystem with InstantiatesHierarchicalElements with CanHaveAutoLink =>
  private val pbus = locateTLBusWrapper(PBUS)

  val aesLink = p(AesLinkKey).map { attach =>
    val accelerators = totalTiles.values.toSeq.flatMap {
      case tile: RocketTile =>
        tile.roccs.collect { case accelerator: _root_.aes.AES256ECBAccel => accelerator }
      case _ => Nil
    }
    require(accelerators.size == 1)
    val accelerator = accelerators.head
    require(accelerator.jobNode.nonEmpty)
    val endpoint = LazyModule(new AesLinkEndpoint(attach.adapter))

    endpoint.autoNode := autoLink.get.endpoint(attach.portName)
    accelerator.jobNode.get := endpoint.jobNode
    endpoint.clockNode := pbus.fixedClockNode
    endpoint
  }
}
