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

object AesLinkStatus {
  val BadAddress = 1
  val BadLength = 2
}

object AesJobControl {
  val Source = 0x000
  val Bytes = 0x008
  val Destination = 0x010
  val Completion = 0x018
  val Key0 = 0x020
  val Key1 = 0x028
  val Key2 = 0x030
  val Key3 = 0x038
  val Encrypt = 0x040
  val Submit = 0x048
}

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
    val rootJob = Flipped(Decoupled(new _root_.aes.AesJob))
    val job = Decoupled(new _root_.aes.AesJob)
    val inputReadDone = Flipped(Decoupled(Bool()))
    val jobDone = Flipped(Decoupled(Bool()))
  })

  object Role {
    val idle :: root :: downstream :: Nil = Enum(3)
  }

  val role = RegInit(Role.idle)
  val watch = Reg(new AutoWatch(params.auto))
  val watchArmed = RegInit(false.B)
  val outputValid = RegInit(false.B)
  val outputDetail = RegInit(0.U(params.auto.detailWidth.W))
  val copy = Reg(new AutoCopyRequest(params.auto))
  val readDone = RegInit(false.B)
  val copyReported = RegInit(false.B)
  val done = RegInit(false.B)
  val computeAccepted = RegInit(false.B)

  val idle = role === Role.idle && !outputValid
  val rootAddressValid = io.rootJob.bits.destination.op === watch.address
  val rootLengthValid = io.rootJob.bits.source.isize === watch.bytes
  val rootValid = rootAddressValid && rootLengthValid
  val downstreamLaunch = io.autoLink.requestCopy.valid && idle
  val rootReady = idle && watchArmed && !io.autoLink.requestCopy.valid &&
    !io.autoLink.requestCompute.valid

  io.autoLink.watchOutput.ready := !watchArmed && !outputValid
  io.autoLink.reportOutput.valid := outputValid
  io.autoLink.reportOutput.bits.status := Mux(
    outputDetail === 0.U,
    AutoLinkStatus.Success,
    AutoLinkStatus.SourceFailure)
  io.autoLink.reportOutput.bits.detail := outputDetail
  io.autoLink.reportOutput.bits.data := 0.U

  val downstreamJob = Wire(new _root_.aes.AesJob)
  downstreamJob.source.ip := io.autoLink.requestCopy.bits.sourceAddress
  downstreamJob.source.isize := io.autoLink.requestCopy.bits.bytes
  // Caliptra consumes the first key byte from the low UInt byte.
  val keyBytes = params.key.U(_root_.aes.AES256Consts.KEY_SZ_BITS.W)
    .asTypeOf(Vec(_root_.aes.AES256Consts.KEY_SZ_BYTES, UInt(8.W)))
  downstreamJob.key := Cat(keyBytes)
  downstreamJob.encrypt := params.encrypt.B
  downstreamJob.destination.op := params.ciphertextAddress.U
  downstreamJob.destination.cmpflag := params.completionAddress.U

  io.job.valid := downstreamLaunch || (io.rootJob.valid && rootReady && rootValid)
  io.job.bits := Mux(downstreamLaunch, downstreamJob, io.rootJob.bits)
  io.rootJob.ready := rootReady && Mux(rootValid, io.job.ready, true.B)
  io.autoLink.requestCopy.ready := io.job.ready && idle

  io.inputReadDone.ready := role === Role.root || (role === Role.downstream && !readDone)
  io.jobDone.ready := (role === Role.root && !outputValid) ||
    (role === Role.downstream && !done)

  io.autoLink.reportCopy.valid := role === Role.downstream && readDone && !copyReported
  io.autoLink.reportCopy.bits.task := copy.task
  io.autoLink.reportCopy.bits.status := AutoLinkStatus.Success
  io.autoLink.reportCopy.bits.detail := 0.U

  io.autoLink.requestCompute.ready := Mux(
    io.autoLink.requestCompute.bits.start,
    role === Role.downstream && copyReported && !computeAccepted,
    idle)
  io.autoLink.reportCompute.valid := role === Role.downstream && computeAccepted && done
  io.autoLink.reportCompute.bits.status := AutoLinkStatus.Success
  io.autoLink.reportCompute.bits.detail := 0.U
  io.autoLink.reportCompute.bits.data := 0.U

  when(io.autoLink.watchOutput.fire) {
    watch := io.autoLink.watchOutput.bits
    watchArmed := true.B
  }
  when(io.rootJob.fire && !rootValid) {
    outputValid := true.B
    outputDetail := Mux(
      !rootAddressValid,
      AesLinkStatus.BadAddress.U,
      AesLinkStatus.BadLength.U)
  }
  when(io.job.fire && !downstreamLaunch) {
    role := Role.root
  }
  when(io.autoLink.requestCopy.fire) {
    role := Role.downstream
    copy := io.autoLink.requestCopy.bits
    readDone := false.B
    copyReported := false.B
    done := false.B
    computeAccepted := false.B
  }
  when(io.inputReadDone.fire) {
    when(role === Role.downstream) {
      readDone := true.B
    }
  }
  when(io.jobDone.fire) {
    when(role === Role.root) {
      outputValid := true.B
      outputDetail := 0.U
    }.otherwise {
      done := true.B
    }
  }
  when(io.autoLink.reportOutput.fire) {
    role := Role.idle
    watchArmed := false.B
    outputValid := false.B
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
    role := Role.idle
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
  private val controlAddress = CgraLinkControlGenerated.baseAddress +
    2 * CgraLinkControlGenerated.pageSizeBytes
  private val device = new SimpleDevice("aes-job", Seq("coredac,aes-job"))
  val controlNode = TLRegisterNode(
    address = Seq(AddressSet(controlAddress, CgraLinkControlGenerated.pageSizeBytes - 1)),
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
      val submit = Wire(Decoupled(UInt(1.W)))
      val keyBytes = Cat(key.reverse).asTypeOf(
        Vec(_root_.aes.AES256Consts.KEY_SZ_BYTES, UInt(8.W)))

      adapter.io.rootJob.valid := submit.valid && submit.bits.asBool
      adapter.io.rootJob.bits.source.ip := source
      adapter.io.rootJob.bits.source.isize := bytes
      adapter.io.rootJob.bits.destination.op := destination
      adapter.io.rootJob.bits.destination.cmpflag := completion
      adapter.io.rootJob.bits.key := Cat(keyBytes)
      adapter.io.rootJob.bits.encrypt := encrypt
      submit.ready := Mux(
        submit.bits.asBool,
        adapter.io.rootJob.ready,
        true.B)

      import AesJobControl._
      controlNode.regmap(
        Source -> Seq(RegField(64, source)),
        Bytes -> Seq(RegField(64, bytes)),
        Destination -> Seq(RegField(64, destination)),
        Completion -> Seq(RegField(64, completion)),
        Key0 -> Seq(RegField(64, key(0))),
        Key1 -> Seq(RegField(64, key(1))),
        Key2 -> Seq(RegField(64, key(2))),
        Key3 -> Seq(RegField(64, key(3))),
        Encrypt -> Seq(RegField(1, encrypt)),
        Submit -> Seq(RegField.w(1, submit)))
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
