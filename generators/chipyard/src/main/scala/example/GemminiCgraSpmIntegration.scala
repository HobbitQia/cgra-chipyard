package chipyard.example

import chisel3._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.subsystem.{BaseSubsystem, InstantiatesHierarchicalElements, PBUS}
import freechips.rocketchip.tile.RocketTile
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{AsyncQueueParams, FromAsyncBundle, ToAsyncBundle}
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import org.chipsalliance.diplomacy.lazymodule.LazyModule

case class GemminiCgraSpmParams(
  spm: SharedSpmParams,
  link: SpmLinkParams,
  gemminiTable: SpmCommunicationTable,
  cgraTable: SpmCommunicationTable,
  controlAddress: BigInt,
  controlBytes: Int,
  packetCapacity: Int) {
  val linkId: Int = gemminiTable.publishTo.head.link
  val cgra: CgraSpmParams = CgraSpmParams(
    link,
    cgraTable,
    CGRAGenerated.params,
    spm.slotBases,
    packetCapacity)
}

case object GemminiCgraSpmKey extends Field[Option[GemminiCgraSpmParams]](None)
case object CgraSpmKey extends Field[Option[CgraSpmParams]](None)

class WithGemminiCgraSpm(params: GemminiCgraSpmParams)
    extends Config((_, _, _) => {
      case GemminiCgraSpmKey => Some(params)
      case CgraSpmKey => Some(params.cgra)
    })

/** Concrete Gemmini-to-CGRA AutoLink instance. */
class GemminiCgraSpmIntegration(
  gemminiAccelerator: gemmini.Gemmini[chisel3.SInt, gemmini.Float, gemmini.Float],
  params: GemminiCgraSpmParams)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  private val gemminiConfig = gemminiAccelerator.config
  private val readBeatBytes = gemminiConfig.sp_width / 8
  private val writeBeatBytes =
    gemminiConfig.meshColumns * gemminiConfig.tileColumns * gemminiConfig.accType.getWidth / 8
  private val gemminiParams = GemminiSpmParams(
    params.link,
    params.gemminiTable,
    params.spm.slotBases,
    writeBeatBytes)

  require(readBeatBytes == params.link.beatBytes)

  val spm = LazyModule(new SharedSpm(params.spm, readBeatBytes, writeBeatBytes))
  val monitor = LazyModule(new GemminiSpmMonitor(gemminiParams))
  val control = LazyModule(new CgraSpmControl(
    params.cgra,
    params.controlAddress,
    params.controlBytes))
  val cgraNode = BundleBridgeSource(() => new CgraSpmAsyncLink(params.cgra))

  spm.readers :=* gemminiAccelerator.spad_read_nodes
  spm.writers :=* TLWidthWidget(readBeatBytes) :=* TLBuffer() :=*
    gemminiAccelerator.spad_write_nodes
  spm.writers := monitor.node := TLWidthWidget(readBeatBytes) := TLBuffer() :=
    gemminiAccelerator.spad.spad_writer.get.node

  override lazy val module = new IntegrationImpl
  class IntegrationImpl extends Impl {
    withClockAndReset(clock, reset) {
      val producer = Module(new GemminiSpmAdapter(gemminiParams))
      val autoLink = Module(new SpmAutoLink(params.link, params.linkId))
      val cgra = cgraNode.out.head._1

      producer.io.write <> monitor.module.io.write
      producer.io.ack <> monitor.module.io.ack
      producer.io.endpoint.deliver.valid := false.B
      producer.io.endpoint.deliver.bits := 0.U.asTypeOf(new SpmLinkEvent(params.link))
      producer.io.endpoint.done.ready := true.B
      autoLink.io.produced <> producer.io.endpoint.produced
      cgra.deliver <> ToAsyncBundle(autoLink.io.deliver, AsyncQueueParams.singleton())
      autoLink.io.done <> FromAsyncBundle(cgra.done)
      control.module.io.resultIn <> autoLink.io.result

      cgra.config <> ToAsyncBundle(control.module.io.configOut, AsyncQueueParams.singleton())
      control.module.io.configAck <> FromAsyncBundle(cgra.configAck)
    }
  }
}

trait CanHaveGemminiCgraSpm {
  this: BaseSubsystem with InstantiatesHierarchicalElements =>
  private val pbus = locateTLBusWrapper(PBUS)

  val gemminiCgraSpm = p(GemminiCgraSpmKey).map { params =>
    val gemminis = totalTiles.values.toSeq.flatMap {
      case tile: RocketTile =>
        tile.roccs.collect { case accelerator: gemmini.Gemmini[_, _, _] => accelerator }
      case _ => Nil
    }
    val cgras = totalTiles.values.toSeq.flatMap {
      case tile: RocketTile =>
        tile.roccs.collect { case accelerator: CGRAAccelerator => accelerator }
      case _ => Nil
    }
    require(gemminis.size == 1 && cgras.size == 1)

    val gemminiAccelerator = gemminis.head.asInstanceOf[
      gemmini.Gemmini[chisel3.SInt, gemmini.Float, gemmini.Float]]
    val integration = LazyModule(new GemminiCgraSpmIntegration(gemminiAccelerator, params))
    cgras.head.spmNode.get := integration.cgraNode
    integration.clockNode := pbus.fixedClockNode
    integration.spm.clockNode := pbus.fixedClockNode
    integration.monitor.clockNode := pbus.fixedClockNode
    integration.control.clockNode := pbus.fixedClockNode

    pbus.coupleTo("shared-spm") {
      integration.spm.readers := TLFragmenter(params.link.beatBytes, pbus.blockBytes) :=
        TLWidthWidget(pbus) := _
    }
    pbus.coupleTo("cgra-spm-control") {
      integration.control.node := TLBuffer() :=
        TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _
    }
    integration
  }
}
