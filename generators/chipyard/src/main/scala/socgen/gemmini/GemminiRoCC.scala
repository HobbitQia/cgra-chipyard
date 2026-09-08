package chipyard.socgen.gemmini

import chisel3._
import chipyard.socgen.link.AutoEndpointAsyncLink
import freechips.rocketchip.diplomacy.{AddressSet, BundleBridgeSink}
import freechips.rocketchip.tile.{BuildRoCC, LazyRoCC, LazyRoCCModuleImp}
import freechips.rocketchip.tilelink.{TLFilter, TLFragmenter, TLIdentityNode, TLXbar}
import freechips.rocketchip.util.{AsyncQueueParams, FromAsyncBundle, ToAsyncBundle}
import org.chipsalliance.cde.config.{Config, Parameters}
import org.chipsalliance.diplomacy.lazymodule.LazyModule

/** Keeps automatic command injection outside the Gemmini IP. */
class GemminiRoCC(
  val config: gemmini.GemminiArrayConfig[SInt, gemmini.Float, gemmini.Float],
  val linkParams: Option[GemminiLinkParams])(implicit p: Parameters)
    extends LazyRoCC(
      opcodes = config.opcodes,
      nPTWPorts = if (config.use_shared_tlb) 1 else 2) {
  val accelerator = LazyModule(new gemmini.Gemmini(config))
  val autoNode = linkParams.map(_ => BundleBridgeSink[AutoEndpointAsyncLink]())
  val configNode = linkParams.map(params => BundleBridgeSink[GemminiLinkConfigAsync]())
  val observeNode = linkParams.map(params => BundleBridgeSink[GemminiLinkObserveAsync]())

  private val externalSpm = p(GemminiExternalSpmKey).get
  private val localRange = AddressSet(externalSpm.baseAddress, externalSpm.sizeBytes - 1)
  private val dma = TLXbar()
  private val outward = TLIdentityNode()
  val localNode = TLIdentityNode()

  dma := accelerator.node
  localNode := TLFragmenter(config.dma_buswidth / 8, config.dma_maxbytes, alwaysMin = true) := dma
  outward := TLFilter(TLFilter.mSubtract(localRange)) := dma

  override val atlNode = if (config.use_dedicated_tl_port) accelerator.atlNode else outward
  override val tlNode = if (config.use_dedicated_tl_port) outward else accelerator.tlNode
  override val stlNode = accelerator.stlNode
  override lazy val module = new GemminiRoCCModule(this)
}

class GemminiRoCCModule(outer: GemminiRoCC)(implicit p: Parameters)
    extends LazyRoCCModuleImp(outer) {
  private val gemmini = outer.accelerator.module
  private val linkBusy = outer.linkParams.map { params =>
    val endpoint = outer.autoNode.get.in.head._1
    val config = outer.configNode.get.in.head._1
    val observe = outer.observeNode.get.in.head._1
    val native = outer.config
    val spm = p(GemminiExternalSpmKey).get
    val convParams = Option.when(native.has_loop_conv)(GemminiConvParams(
      dim = native.meshColumns * native.tileColumns,
      spmRows = native.sp_banks * native.sp_bank_entries,
      accRows = native.acc_banks * native.acc_bank_entries,
      elementBytes = native.inputType.getWidth / 8,
      spmBase = spm.baseAddress,
      spmBytes = spm.sizeBytes,
      addressBits = gemmini.coreMaxAddrBits))
    val adapter = Module(new GemminiLinkAdapter(params, convParams))

    adapter.io.configIn <> FromAsyncBundle(config.config)
    config.ack <> ToAsyncBundle(adapter.io.configAck, AsyncQueueParams.singleton())
    adapter.io.event <> FromAsyncBundle(observe.event)
    adapter.io.autoLink.watchOutput <> FromAsyncBundle(endpoint.watchOutput)
    endpoint.reportOutput <> ToAsyncBundle(
      adapter.io.autoLink.reportOutput,
      AsyncQueueParams.singleton())
    adapter.io.autoLink.requestCopy <> FromAsyncBundle(endpoint.requestCopy)
    endpoint.reportCopy <> ToAsyncBundle(
      adapter.io.autoLink.reportCopy,
      AsyncQueueParams.singleton())
    adapter.io.autoLink.requestCompute <> FromAsyncBundle(endpoint.requestCompute)
    endpoint.reportCompute <> ToAsyncBundle(
      adapter.io.autoLink.reportCompute,
      AsyncQueueParams.singleton())
    adapter.io.cpuCommand <> io.cmd
    gemmini.io.cmd <> adapter.io.command
    adapter.io.nativeBusy := gemmini.io.busy
    adapter.io.autoBusy
  }.getOrElse {
    gemmini.io.cmd <> io.cmd
    false.B
  }

  io.resp <> gemmini.io.resp
  io.mem <> gemmini.io.mem
  io.ptw <> gemmini.io.ptw
  io.fpu_req <> gemmini.io.fpu_req
  io.fpu_resp <> gemmini.io.fpu_resp
  io.csrs <> gemmini.io.csrs
  gemmini.io.exception := io.exception
  io.busy := gemmini.io.busy || linkBusy
  io.interrupt := gemmini.io.interrupt
}

class WithGemminiRoCC(
  config: gemmini.GemminiArrayConfig[SInt, gemmini.Float, gemmini.Float],
  linkParams: Option[GemminiLinkParams] = None)
    extends Config((_, _, up) => {
      case BuildRoCC => up(BuildRoCC) :+ { p: Parameters =>
        implicit val q: Parameters = p
        LazyModule(new GemminiRoCC(config, linkParams))
      }
    })
