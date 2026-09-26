package chipyard.socgen.link

import chisel3._
import chisel3.util._
import chipyard.socgen.generated.CgraLinkControlGenerated
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.tilelink.TLRegisterNode
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
  val runNode = BundleBridgeSource(() => Decoupled(new AutoRun(params)))
  // Root and fabric are both on the fixed peripheral-bus clock.
  val stateNode = BundleBridgeSink[AutoProgress]()

  override lazy val module = new RootImpl
  class RootImpl extends Impl {
    withClockAndReset(clock, reset) {
      import CgraLinkControlGenerated._
      require(AUTO_LINK_TRANSFER_BASE + params.dependencyCount * AUTO_LINK_TRANSFER_STRIDE <= AUTO_LINK_REGION_BASE)
      require(AUTO_LINK_REGION_BASE + params.stageCount * AUTO_LINK_REGION_STRIDE <= params.controlBytes)
      val run = Wire(Decoupled(new AutoRun(params)))
      val inputReady = Wire(Decoupled(UInt(1.W)))
      val rows = RegInit(1.U(params.lengthWidth.W))
      val columns = RegInit(1.U(params.lengthWidth.W))
      val tileRows = RegInit(1.U(params.lengthWidth.W))
      val tileColumns = RegInit(1.U(params.lengthWidth.W))
      val transfers = RegInit(AutoTileBinding.defaults(params))
      val regions = RegInit(0.U.asTypeOf(Vec(params.stageCount, new AutoRegion(params.lengthWidth))))
      val jobs = RegInit(AutoTileBinding.jobs(params))
      val stages = RegInit(AutoTileBinding.stages(params))
      val edges = RegInit(AutoTileBinding.edges(params))
      val stage = RegInit(0.U(params.stageWidth.W))
      val edge = RegInit(0.U(params.dependencyWidth.W))
      val jobWrite = Wire(Decoupled(UInt(params.jobWidth.W)))
      jobWrite.ready := true.B
      when(jobWrite.fire) {
        jobs(stage) := jobWrite.bits
      }
      val staging = Wire(new AutoRun(params))
      staging.plan.rows := rows
      staging.plan.columns := columns
      staging.plan.tileRows := tileRows
      staging.plan.tileColumns := tileColumns
      staging.transfers := transfers
      staging.regions := regions
      staging.jobs := jobs
      staging.stages := stages
      staging.edges := edges
      run.valid := inputReady.valid && inputReady.bits.asBool
      run.bits := staging
      inputReady.ready := !inputReady.bits.asBool || run.ready
      val pending = Module(new Queue(new AutoRun(params), 1))
      pending.io.enq <> run
      runNode.out.head._1 <> pending.io.deq
      val state = stateNode.in.head._1
      val sequenceBusy = WireDefault(false.B)
      val running = state.running || sequenceBusy || pending.io.deq.valid
      val runFields = if (params.runCapacity > 0) {
        val indexWidth = math.max(1, log2Ceil(params.runCapacity))
        val countWidth = math.max(1, log2Ceil(params.runCapacity + 1))
        val capture = Wire(Decoupled(UInt(indexWidth.W)))
        val start = Wire(Decoupled(UInt(1.W)))
        val first = RegInit(0.U(indexWidth.W))
        val count = RegInit(1.U(countWidth.W))
        val index = Reg(UInt(indexWidth.W))
        val remaining = Reg(UInt(countWidth.W))
        val idle :: fetch :: issue :: waitRun :: Nil = Enum(4)
        val phase = RegInit(idle)
        val descriptions = SyncReadMem(params.runCapacity, new AutoRun(params))
        val description = Reg(new AutoRun(params))
        sequenceBusy := phase =/= idle
        capture.ready := !running
        start.ready := !running && !capture.valid && !inputReady.valid
        val begin = start.fire && start.bits.asBool
        val advance = phase === waitRun && state.done && !state.failed && remaining > 1.U
        val next = index + 1.U
        val selected = Mux(begin, first, next)
        val read = descriptions.read(selected, begin || advance)
        when(capture.fire) {
          descriptions.write(capture.bits, staging)
        }
        when(begin) {
          index := first
          remaining := count
          phase := fetch
        }
        when(phase === fetch) {
          description := read
          phase := issue
        }
        when(sequenceBusy) {
          inputReady.ready := false.B
          run.valid := phase === issue
          run.bits := description
          when(run.fire) {
            phase := waitRun
          }
        }
        when(phase === waitRun && state.done) {
          phase := idle
          when(advance) {
            index := next
            remaining := remaining - 1.U
            phase := fetch
          }
        }
        Seq(
          AUTO_LINK_RUN_CAPTURE -> Seq(RegField.w(indexWidth, capture)),
          AUTO_LINK_RUN_FIRST -> Seq(RegField(indexWidth, first)),
          AUTO_LINK_RUN_COUNT -> Seq(RegField(countWidth, count)),
          AUTO_LINK_RUN_START -> Seq(RegField.w(1, start)))
      } else Seq.empty
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
      def stageField(offset: Int, width: Int)(update: UInt => Unit): (Int, Seq[RegField]) = {
        val value = Wire(Decoupled(UInt(width.W)))
        value.ready := true.B
        when(value.fire) { update(value.bits) }
        offset -> Seq(RegField.w(width, value))
      }
      val graphFields = Seq(
        AUTO_LINK_EDGE -> Seq(RegField(params.dependencyWidth, edge)),
        stageField(AUTO_LINK_ENDPOINT, params.endpointWidth)(value => stages(stage).endpoint := value),
        stageField(AUTO_LINK_ENABLE, 1)(value => stages(stage).enabled := value.asBool),
        stageField(AUTO_LINK_EDGE_SOURCE, params.stageWidth)(value => edges(edge).source := value),
        stageField(AUTO_LINK_EDGE_DESTINATION, params.stageWidth)(value => edges(edge).destination := value),
        stageField(AUTO_LINK_EDGE_FLAGS, 3) { value =>
          edges(edge).enabled := value(0)
          edges(edge).root := value(1)
          edges(edge).copy := value(2)
        },
        stageField(AUTO_LINK_EDGE_BYTES, params.lengthWidth)(value => edges(edge).bytes := value),
        stageField(AUTO_LINK_EDGE_EXPANSION, 3)(value => edges(edge).expansion := value),
        stageField(AUTO_LINK_EDGE_SOURCE_BASE, params.addressWidth)(value => transfers(edge).sourceBase := value),
        stageField(AUTO_LINK_OUTPUT_FLAGS, 2) { value =>
          stages(stage).output.writeback := value(0)
          stages(stage).output.packed := value(1)
        },
        stageField(AUTO_LINK_OUTPUT_PIXEL_BYTES, params.lengthWidth)(value => stages(stage).output.bytesPerPixel := value),
        stageField(AUTO_LINK_OUTPUT_SOURCE_OFFSET, params.addressWidth)(value => stages(stage).output.sourceOffset := value),
        stageField(AUTO_LINK_OUTPUT_SOURCE_STRIDE, params.addressWidth)(value => stages(stage).output.sourceStride := value),
        stageField(AUTO_LINK_OUTPUT_ADDRESS, params.addressWidth)(value => stages(stage).output.address := value),
        stageField(AUTO_LINK_OUTPUT_STRIDE, params.addressWidth)(value => stages(stage).output.stride := value),
        stageField(AUTO_LINK_OUTPUT_BYTES, params.lengthWidth)(value => stages(stage).output.bytes := value))
      controlNode.regmap((Seq(
        AUTO_LINK_INPUT_READY -> Seq(RegField.w(1, inputReady)),
        AUTO_LINK_ROWS -> Seq(RegField(params.lengthWidth, rows)),
        AUTO_LINK_COLUMNS -> Seq(RegField(params.lengthWidth, columns)),
        AUTO_LINK_TILE_ROWS -> Seq(RegField(params.lengthWidth, tileRows)),
        AUTO_LINK_TILE_COLUMNS -> Seq(RegField(params.lengthWidth, tileColumns)),
        AUTO_LINK_EMITTING -> Seq(RegField.r(1, state.emitting)),
        AUTO_LINK_RUNNING -> Seq(RegField.r(1, running)),
        AUTO_LINK_CYCLES -> Seq(RegField.r(64, state.cycles)),
        AUTO_LINK_OVERLAP -> Seq(RegField.r(64, state.overlap)),
        AUTO_LINK_PEAK_ACTIVE -> Seq(RegField.r(64, state.peakActive)),
        AUTO_LINK_STAGE -> Seq(RegField(params.stageWidth, stage)),
        AUTO_LINK_JOB -> Seq(RegField.w(params.jobWidth, jobWrite))) ++ runFields ++ transferFields ++ regionFields ++ graphFields): _*)
    }
  }
}
