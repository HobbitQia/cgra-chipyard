package chipyard.socgen.link

import chisel3._
import chisel3.util.Decoupled
import chipyard.socgen.generated.CgraLinkControlGenerated
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.tilelink.TLRegisterNode
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams, ToAsyncBundle}
import org.chipsalliance.cde.config.Parameters

/** Captures run geometry once and emits tile-ready events under backpressure. */
class AutoLinkRoot(params: AutoLinkParams)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  private val device = new SimpleDevice("auto-link-root", Seq("coredac,auto-link-root"))
  val controlNode = TLRegisterNode(
    address = Seq(AddressSet(params.controlAddress, params.controlBytes - 1)),
    device = device,
    beatBytes = 8,
    concurrency = 1)
  val eventNode = BundleBridgeSource(() =>
    new AsyncBundle(new AutoTileEvent(params), AsyncQueueParams.singleton()))

  override lazy val module = new RootImpl
  class RootImpl extends Impl {
    withClockAndReset(clock, reset) {
      val event = Wire(Decoupled(new AutoTileEvent(params)))
      val inputReady = Wire(Decoupled(UInt(1.W)))
      val rows = RegInit(1.U(params.lengthWidth.W))
      val columns = RegInit(1.U(params.lengthWidth.W))
      val tileRows = RegInit(1.U(params.lengthWidth.W))
      val tileColumns = RegInit(1.U(params.lengthWidth.W))
      val error = RegInit(false.B)
      val cursor = Module(new AutoTileCursor(params.lengthWidth))
      cursor.io.start.valid := inputReady.valid && inputReady.bits.asBool
      cursor.io.start.bits.rows := rows
      cursor.io.start.bits.columns := columns
      cursor.io.start.bits.tileRows := tileRows
      cursor.io.start.bits.tileColumns := tileColumns
      inputReady.ready := Mux(inputReady.bits.asBool, cursor.io.start.ready, true.B)
      when(cursor.io.start.fire) {
        error := cursor.io.error
      }
      event.valid := cursor.io.out.valid
      event.bits.event := 0.U.asTypeOf(new AutoEvent(params))
      event.bits.tile := cursor.io.out.bits
      cursor.io.out.ready := event.ready
      eventNode.out.head._1 <> ToAsyncBundle(event, AsyncQueueParams.singleton())

      import CgraLinkControlGenerated._
      controlNode.regmap(
        AUTO_LINK_INPUT_READY -> Seq(RegField.w(1, inputReady)),
        AUTO_LINK_ROWS -> Seq(RegField(params.lengthWidth, rows)),
        AUTO_LINK_COLUMNS -> Seq(RegField(params.lengthWidth, columns)),
        AUTO_LINK_TILE_ROWS -> Seq(RegField(params.lengthWidth, tileRows)),
        AUTO_LINK_TILE_COLUMNS -> Seq(RegField(params.lengthWidth, tileColumns)),
        AUTO_LINK_EMITTING -> Seq(RegField.r(1, cursor.io.busy)),
        AUTO_LINK_CONFIG_ERROR -> Seq(RegField.r(1, error)))
    }
  }
}
