package chipyard.socgen.link

import chisel3._
import chisel3.util._
import chipyard.socgen.generated.CgraLinkControlGenerated
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.tilelink.TLRegisterNode
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams, ToAsyncBundle}
import org.chipsalliance.cde.config.Parameters

/** Snapshots geometry and transfer bindings once, before releasing a run. */
class AutoLinkRoot(params: AutoLinkParams)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  private val device = new SimpleDevice("auto-link-root", Seq("coredac,auto-link-root"))
  val controlNode = TLRegisterNode(
    address = Seq(AddressSet(params.controlAddress, params.controlBytes - 1)),
    device = device,
    beatBytes = 8,
    concurrency = 1)
  val runNode = BundleBridgeSource(() =>
    new AsyncBundle(new AutoRun(params), AsyncQueueParams.singleton()))
  // Root and fabric are both on the fixed peripheral-bus clock.
  val stateNode = BundleBridgeSink[AutoProgress]()

  override lazy val module = new RootImpl
  class RootImpl extends Impl {
    withClockAndReset(clock, reset) {
      import CgraLinkControlGenerated._
      require(AUTO_LINK_TRANSFER_BASE + params.dependencies.size * AUTO_LINK_TRANSFER_STRIDE <= AUTO_LINK_REGION_BASE)
      require(AUTO_LINK_REGION_BASE + params.stages.size * AUTO_LINK_REGION_STRIDE <= params.controlBytes)
      val run = Wire(Decoupled(new AutoRun(params)))
      val inputReady = Wire(Decoupled(UInt(1.W)))
      val rows = RegInit(1.U(params.lengthWidth.W))
      val columns = RegInit(1.U(params.lengthWidth.W))
      val tileRows = RegInit(1.U(params.lengthWidth.W))
      val tileColumns = RegInit(1.U(params.lengthWidth.W))
      val transfers = RegInit(AutoTileBinding.defaults(params))
      val regions = RegInit(0.U.asTypeOf(Vec(params.stages.size, new AutoRegion(params.lengthWidth))))
      val error = RegInit(false.B)
      val shapeValid = rows =/= 0.U && columns =/= 0.U && tileRows =/= 0.U && tileColumns =/= 0.U
      val multipleSlots = (params.bufferSlots > 1).B && (rows > tileRows || columns > tileColumns)
      val regionValid = regions.map(region => region.rows === 0.U ||
        (region.columns =/= 0.U && region.rowStep =/= 0.U && region.columnStep =/= 0.U)).reduce(_ && _)
      val rangeValid = params.dependencies.zipWithIndex.flatMap { case (dependency, index) =>
        dependency.copy.map(_ => AutoTileBinding.transferValid(params, index, transfers(index), multipleSlots))
      }.reduceOption(_ && _).getOrElse(true.B)
      val valid = shapeValid && regionValid && rangeValid
      run.valid := inputReady.valid && inputReady.bits.asBool && valid
      run.bits.plan.rows := rows
      run.bits.plan.columns := columns
      run.bits.plan.tileRows := tileRows
      run.bits.plan.tileColumns := tileColumns
      run.bits.transfers := transfers
      run.bits.regions := regions
      inputReady.ready := !inputReady.bits.asBool || !valid || run.ready
      when(inputReady.fire && inputReady.bits.asBool) {
        error := !valid
      }
      runNode.out.head._1 <> ToAsyncBundle(run, AsyncQueueParams.singleton())
      val state = stateNode.in.head._1
      val transferFields = transfers.zipWithIndex.flatMap { case (transfer, index) =>
        Seq(transfer.sourceOffset, transfer.destinationOffset, transfer.sourceStride,
          transfer.destinationStride, transfer.bytesPerPixel).zipWithIndex.map { case (field, offset) =>
          (AUTO_LINK_TRANSFER_BASE + index * AUTO_LINK_TRANSFER_STRIDE + offset * 8) ->
            Seq(RegField(field.getWidth, field))
        }
      }
      val regionFields = regions.zipWithIndex.flatMap { case (region, index) =>
        Seq(region.rows, region.columns, region.rowStep, region.columnStep,
          region.top, region.bottom, region.left, region.right).zipWithIndex.map { case (field, offset) =>
          (AUTO_LINK_REGION_BASE + index * AUTO_LINK_REGION_STRIDE + offset * 8) ->
            Seq(RegField(field.getWidth, field))
        }
      }
      controlNode.regmap((Seq(
        AUTO_LINK_INPUT_READY -> Seq(RegField.w(1, inputReady)),
        AUTO_LINK_ROWS -> Seq(RegField(params.lengthWidth, rows)),
        AUTO_LINK_COLUMNS -> Seq(RegField(params.lengthWidth, columns)),
        AUTO_LINK_TILE_ROWS -> Seq(RegField(params.lengthWidth, tileRows)),
        AUTO_LINK_TILE_COLUMNS -> Seq(RegField(params.lengthWidth, tileColumns)),
        AUTO_LINK_EMITTING -> Seq(RegField.r(1, state.emitting)),
        AUTO_LINK_RUNNING -> Seq(RegField.r(1, state.running)),
        AUTO_LINK_CYCLES -> Seq(RegField.r(64, state.cycles)),
        AUTO_LINK_OVERLAP -> Seq(RegField.r(64, state.overlap)),
        AUTO_LINK_CONFIG_ERROR -> Seq(RegField.r(1, error))) ++ transferFields ++ regionFields): _*)
    }
  }
}
