package chipyard.example

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams}

object AutoLinkStatus {
  val Width = 4
  val Success = 0.U(Width.W)
  val SourceFailure = 1.U(Width.W)
  val SinkFailure = 2.U(Width.W)
}

case class AutoBuffer(baseAddress: BigInt, sizeBytes: Int)

case class AutoEndpointSpec(
  name: String,
  buffer: Option[AutoBuffer],
  localBytes: Int)

case class AutoLinkSpec(source: String, destination: String)

case class AutoCopySpec(
  route: Int,
  sourceOffset: Int,
  destinationOffset: Int,
  bytes: Int)

case class AutoLinkParams(
  links: Seq[AutoLinkSpec],
  endpoints: Seq[AutoEndpointSpec],
  table: Seq[AutoCopySpec],
  beatBytes: Int,
  copyDepth: Int,
  addressWidth: Int = 64,
  lengthWidth: Int = 32,
  detailWidth: Int = 8,
  resultWidth: Int = 32) {
  require(links.nonEmpty)
  require(table.nonEmpty)
  require(endpoints.map(_.name).distinct.size == endpoints.size)
  require(isPow2(beatBytes))
  require(copyDepth > 0)

  private val endpointMap = endpoints.map(endpoint => endpoint.name -> endpoint).toMap
  require(links.forall(link => endpointMap.contains(link.source) && endpointMap.contains(link.destination)))
  require(table.forall(copy => links.indices.contains(copy.route)))

  table.foreach { copy =>
    val link = links(copy.route)
    val source = endpointMap(link.source)
    val destination = endpointMap(link.destination)
    require(source.buffer.nonEmpty)
    require(copy.sourceOffset >= 0)
    require(copy.destinationOffset >= 0)
    require(copy.bytes > 0)
    require(copy.sourceOffset + copy.bytes <= source.buffer.get.sizeBytes)
    require(copy.destinationOffset + copy.bytes <= destination.localBytes)
    require(copy.sourceOffset % beatBytes == 0)
    require(copy.destinationOffset % beatBytes == 0)
    require(copy.bytes % beatBytes == 0)
  }

  endpoints.foreach { endpoint =>
    val publications = table.indices
      .filter(index => route(index).source == endpoint.name)
      .map(index => table(index))
      .map(copy => (copy.sourceOffset, copy.bytes))
      .distinct
    require(publications.size <= 1)
  }

  val taskWidth: Int = math.max(1, log2Ceil(table.size))

  def endpoint(name: String): AutoEndpointSpec = endpointMap(name)
  def copy(task: Int): AutoCopySpec = table(task)
  def route(task: Int): AutoLinkSpec = links(copy(task).route)
  def sourceAddress(task: Int): BigInt = {
    val spec = route(task)
    endpoint(spec.source).buffer.get.baseAddress + copy(task).sourceOffset
  }
}

class AutoWatch(params: AutoLinkParams) extends Bundle {
  val address = UInt(params.addressWidth.W)
  val bytes = UInt(params.lengthWidth.W)
}

class AutoEvent(params: AutoLinkParams) extends Bundle {
  val status = UInt(AutoLinkStatus.Width.W)
  val detail = UInt(params.detailWidth.W)
  val data = UInt(params.resultWidth.W)
}

class AutoCopyRequest(params: AutoLinkParams) extends Bundle {
  val task = UInt(params.taskWidth.W)
  val sourceAddress = UInt(params.addressWidth.W)
  val destinationOffset = UInt(params.addressWidth.W)
  val bytes = UInt(params.lengthWidth.W)
}

class AutoCopyResult(params: AutoLinkParams) extends Bundle {
  val task = UInt(params.taskWidth.W)
  val status = UInt(AutoLinkStatus.Width.W)
  val detail = UInt(params.detailWidth.W)
}

class AutoComputeRequest extends Bundle {
  val start = Bool()
}

/** TileLink remains the data interface. These channels add automatic control. */
class AutoEndpointIO(params: AutoLinkParams) extends Bundle {
  val watchOutput = Flipped(Decoupled(new AutoWatch(params)))
  val reportOutput = Decoupled(new AutoEvent(params))
  val requestCopy = Flipped(Decoupled(new AutoCopyRequest(params)))
  val reportCopy = Decoupled(new AutoCopyResult(params))
  val requestCompute = Flipped(Decoupled(new AutoComputeRequest))
  val reportCompute = Decoupled(new AutoEvent(params))
}

class AutoEndpointAsyncLink(params: AutoLinkParams) extends Bundle {
  private val crossing = AsyncQueueParams.singleton()
  val watchOutput = new AsyncBundle(new AutoWatch(params), crossing)
  val reportOutput = Flipped(new AsyncBundle(new AutoEvent(params), crossing))
  val requestCopy = new AsyncBundle(new AutoCopyRequest(params), crossing)
  val reportCopy = Flipped(new AsyncBundle(new AutoCopyResult(params), crossing))
  val requestCompute = new AsyncBundle(new AutoComputeRequest, crossing)
  val reportCompute = Flipped(new AsyncBundle(new AutoEvent(params), crossing))
}
