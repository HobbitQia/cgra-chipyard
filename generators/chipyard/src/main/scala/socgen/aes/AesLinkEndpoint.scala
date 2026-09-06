package chipyard.socgen.aes

import chisel3._
import chisel3.util._
import chipyard.socgen.generated.CgraLinkControlGenerated
import chipyard.socgen.link._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.subsystem.{BaseSubsystem, InstantiatesHierarchicalElements, PBUS}
import freechips.rocketchip.tile.RocketTile
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{AsyncQueueParams, FromAsyncBundle, ToAsyncBundle}
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import org.chipsalliance.diplomacy.lazymodule.LazyModule

case class AesLinkAttachParams(adapter: AesLinkParams, portName: String) {
  require(adapter.auto.endpoints.exists(_.name == portName))
}

case object AesLinkKey extends Field[Option[AesLinkAttachParams]](None)

class WithAesLink(params: AesLinkAttachParams) extends Config((_, _, _) => { case AesLinkKey => Some(params) })

class AesLinkEndpoint(params: AesLinkParams)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  val autoNode = BundleBridgeSink[AutoEndpointAsyncLink]()
  val jobNode = BundleBridgeSource(() => new _root_.aes.AesJobAsyncLink)
  private val device = new SimpleDevice("aes-job", Seq("coredac,aes-job"))
  val controlNode = TLRegisterNode(
    address = Seq(AddressSet(
      CgraLinkControlGenerated.aesJobAddress,
      CgraLinkControlGenerated.pageSizeBytes - 1)),
    device = device,
    beatBytes = 8,
    concurrency = 1)

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

      val source = RegInit(0.U(64.W))
      val bytes = RegInit(0.U(64.W))
      val destination = RegInit(0.U(64.W))
      val completion = RegInit(0.U(64.W))
      val key = RegInit(VecInit(Seq.fill(4)(0.U(64.W))))
      val encrypt = RegInit(false.B)
      val selectedJob = RegInit(0.U(32.W))
      val submit = Wire(Decoupled(UInt(2.W)))
      val configReady = RegInit(false.B)
      val configDone = RegInit(false.B)
      val configStatus = RegInit(AutoLinkStatus.Success)
      val configDetail = RegInit(0.U(params.auto.detailWidth.W))
      val keyBytes = Cat(key.reverse).asTypeOf(
        Vec(_root_.aes.AES256Consts.KEY_SZ_BYTES, UInt(8.W)))

      adapter.io.configIn.valid := submit.valid && submit.bits(0)
      adapter.io.configIn.bits.job := selectedJob
      adapter.io.configIn.bits.start := submit.bits(1)
      adapter.io.configIn.bits.descriptor.source.ip := source
      adapter.io.configIn.bits.descriptor.source.isize := bytes
      adapter.io.configIn.bits.descriptor.destination.op := destination
      adapter.io.configIn.bits.descriptor.destination.cmpflag := completion
      // Caliptra consumes the first key byte from the low UInt byte.
      adapter.io.configIn.bits.descriptor.key := Cat(keyBytes)
      adapter.io.configIn.bits.descriptor.encrypt := encrypt
      submit.ready := Mux(
        submit.bits(0),
        adapter.io.configIn.ready,
        true.B)

      when(adapter.io.configIn.valid) {
        configReady := false.B
        configDone := false.B
        configStatus := AutoLinkStatus.Success
        configDetail := 0.U
      }
      when(adapter.io.configIn.fire) {
        configReady := true.B
        configDone := true.B
        configStatus := adapter.io.configStatus
        configDetail := adapter.io.configDetail
      }

      import CgraLinkControlGenerated._
      controlNode.regmap(
        AES_SOURCE -> Seq(RegField(64, source)),
        AES_BYTES -> Seq(RegField(64, bytes)),
        AES_DESTINATION -> Seq(RegField(64, destination)),
        AES_COMPLETION -> Seq(RegField(64, completion)),
        AES_KEY0 -> Seq(RegField(64, key(0))),
        AES_KEY1 -> Seq(RegField(64, key(1))),
        AES_KEY2 -> Seq(RegField(64, key(2))),
        AES_KEY3 -> Seq(RegField(64, key(3))),
        AES_ENCRYPT -> Seq(RegField(1, encrypt)),
        AES_SUBMIT -> Seq(RegField.w(2, submit)),
        AES_CONFIG_READY -> Seq(RegField.r(1, configReady)),
        AES_CONFIG_DONE -> Seq(RegField.r(1, configDone)),
        AES_CONFIG_STATUS -> Seq(RegField.r(32, configStatus)),
        AES_CONFIG_DETAIL -> Seq(RegField.r(32, configDetail)),
        AES_SELECT -> Seq(RegField(32, selectedJob)))
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
    pbus.coupleTo("aes-job") {
      endpoint.controlNode := TLBuffer() := TLFragmenter(
        endpoint.controlNode.beatBytes,
        pbus.blockBytes) := _
    }
    endpoint
  }
}
