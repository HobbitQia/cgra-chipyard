package chipyard.socgen.link

import chisel3._
import chisel3.util.Decoupled
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.tilelink.TLRegisterNode
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams, ToAsyncBundle}
import org.chipsalliance.cde.config.Parameters

/** Converts one CPU input-ready write into an AutoLink root event. */
class AutoLinkRoot(params: AutoLinkParams)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  private val device = new SimpleDevice("auto-link-root", Seq("coredac,auto-link-root"))
  val controlNode = TLRegisterNode(
    address = Seq(AddressSet(params.controlAddress, params.controlBytes - 1)),
    device = device,
    beatBytes = 8,
    concurrency = 1)
  val eventNode = BundleBridgeSource(() =>
    new AsyncBundle(new AutoEvent(params), AsyncQueueParams.singleton()))

  override lazy val module = new RootImpl
  class RootImpl extends Impl {
    withClockAndReset(clock, reset) {
      val event = Wire(Decoupled(new AutoEvent(params)))
      val inputReady = Wire(Decoupled(UInt(1.W)))
      event.valid := inputReady.valid && inputReady.bits.asBool
      event.bits := 0.U.asTypeOf(new AutoEvent(params))
      event.bits.stage := 0.U
      event.bits.status := AutoLinkStatus.Success
      inputReady.ready := Mux(inputReady.bits.asBool, event.ready, true.B)
      eventNode.out.head._1 <> ToAsyncBundle(event, AsyncQueueParams.singleton())

      controlNode.regmap(0 -> Seq(RegField.w(1, inputReady)))
    }
  }
}
