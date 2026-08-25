package chipyard.socgen.cgra

import chisel3._
import chisel3.util._
import chipyard.example.CGRAAccelerator
import chipyard.socgen.generated.CgraLinkControlGenerated
import chipyard.socgen.link.{AutoEvent, AutoLinkStatus, CanHaveAutoLink}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.subsystem.{BaseSubsystem, InstantiatesHierarchicalElements, PBUS}
import freechips.rocketchip.tile.RocketTile
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams, FromAsyncBundle, ToAsyncBundle}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.LazyModule

/** CPU configuration and result registers for the CGRA AutoLink adapter. */
class CgraLinkControl(params: CgraLinkParams, address: BigInt, pageSizeBytes: Int)(implicit p: Parameters) extends ClockSinkDomain(ClockSinkParameters())(p) {
  private val device = new SimpleDevice("cgra-link-control", Seq("coredac,cgra-link-control"))
  val node = TLRegisterNode(
    address = Seq(AddressSet(address, pageSizeBytes - 1)),
    device = device,
    beatBytes = 8,
    concurrency = 1)
  val configNode = BundleBridgeSource(() => new CgraLinkConfigAsync(params))
  val resultNode = BundleBridgeSink[AsyncBundle[AutoEvent]]()

  override lazy val module = new ControlImpl
  class ControlImpl extends Impl {
    withClockAndReset(clock, reset) {
      val configLink = configNode.out.head._1
      val configOut = Wire(Decoupled(new CgraLinkConfig(params)))
      val configAck = FromAsyncBundle(configLink.ack)
      val resultIn = FromAsyncBundle(resultNode.in.head._1)
      configLink.config <> ToAsyncBundle(configOut, AsyncQueueParams.singleton())

      val packetCount = RegInit(0.U(32.W))
      val configSubmit = Wire(Decoupled(UInt(1.W)))
      configOut.valid := configSubmit.valid && configSubmit.bits.asBool
      configOut.bits.packetCount := packetCount(params.packetCountWidth - 1, 0)
      configSubmit.ready := configOut.ready

      val results = Module(new Queue(new AutoEvent(params.auto), 2))
      val resultArbiter = Module(new Arbiter(new AutoEvent(params.auto), 2))
      resultArbiter.io.in(0) <> resultIn
      resultArbiter.io.in(1).valid := configAck.valid &&
        configAck.bits.status =/= AutoLinkStatus.Success
      resultArbiter.io.in(1).bits.status := configAck.bits.status
      resultArbiter.io.in(1).bits.detail := configAck.bits.detail
      resultArbiter.io.in(1).bits.data := 0.U
      results.io.enq <> resultArbiter.io.out
      configAck.ready := Mux(
        configAck.bits.status === AutoLinkStatus.Success,
        true.B,
        resultArbiter.io.in(1).ready)

      val resultPop = Wire(Decoupled(UInt(1.W)))
      val result = RegInit(0.U.asTypeOf(new AutoEvent(params.auto)))
      resultPop.ready := Mux(resultPop.bits.asBool, results.io.deq.valid, true.B)
      results.io.deq.ready := resultPop.valid && resultPop.bits.asBool
      when(results.io.deq.fire) {
        result := results.io.deq.bits
      }

      import CgraLinkControlGenerated._
      node.regmap(
        PACKET_COUNT -> Seq(RegField(32, packetCount)),
        CONFIG_SUBMIT -> Seq(RegField.w(1, configSubmit)),
        RESULT_VALID -> Seq(RegField.r(1, results.io.deq.valid)),
        RESULT_POP -> Seq(RegField.w(1, resultPop)),
        RESULT_STATUS -> Seq(RegField.r(32, result.status)),
        RESULT_DETAIL -> Seq(RegField.r(32, result.detail)),
        RESULT_DATA -> Seq(RegField.r(32, result.data)))
    }
  }
}

trait CanHaveCgraLink {
  this: BaseSubsystem with InstantiatesHierarchicalElements with CanHaveAutoLink =>
  private val pbus = locateTLBusWrapper(PBUS)

  val cgraLink = p(CgraLinkKey).map { attach =>
    val cgras = totalTiles.values.toSeq.flatMap {
      case tile: RocketTile =>
        tile.roccs.collect { case accelerator: CGRAAccelerator => accelerator }
      case _ => Nil
    }
    require(cgras.size == 1)
    val params = attach.adapter
    val cgra = cgras.head
    val control = LazyModule(new CgraLinkControl(params, attach.controlAddress, attach.controlBytes))

    cgra.autoNode.get := autoLink.get.endpoint(attach.portName)
    cgra.linkConfigNode.get := control.configNode
    control.resultNode := autoLink.get.result(attach.portName)
    control.clockNode := pbus.fixedClockNode
    pbus.coupleTo("cgra-link-control") {
      control.node := TLBuffer() := TLFragmenter(
        pbus.beatBytes,
        pbus.blockBytes) := _
    }
    control
  }
}
