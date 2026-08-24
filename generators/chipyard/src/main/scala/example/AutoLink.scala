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

case class AutoTransferSpec(
  route: Int,
  sourceOffset: Int,
  destinationOffset: Int,
  bytes: Int)

case class AutoLinkParams(
  links: Seq[AutoLinkSpec],
  endpoints: Seq[AutoEndpointSpec],
  table: Seq[AutoTransferSpec],
  beatBytes: Int,
  addressWidth: Int = 64,
  lengthWidth: Int = 32,
  detailWidth: Int = 8,
  resultWidth: Int = 32) {
  require(links.nonEmpty)
  require(table.nonEmpty)
  require(endpoints.map(_.name).distinct.size == endpoints.size)
  require(isPow2(beatBytes))

  private val endpointMap = endpoints.map(endpoint => endpoint.name -> endpoint).toMap
  require(links.forall(link => endpointMap.contains(link.source) && endpointMap.contains(link.destination)))
  require(table.forall(transfer => links.indices.contains(transfer.route)))

  table.foreach { transfer =>
    val link = links(transfer.route)
    val source = endpointMap(link.source)
    val destination = endpointMap(link.destination)
    require(source.buffer.nonEmpty)
    require(transfer.sourceOffset >= 0)
    require(transfer.destinationOffset >= 0)
    require(transfer.bytes > 0)
    require(transfer.sourceOffset + transfer.bytes <= source.buffer.get.sizeBytes)
    require(transfer.destinationOffset + transfer.bytes <= destination.localBytes)
    require(transfer.sourceOffset % beatBytes == 0)
    require(transfer.destinationOffset % beatBytes == 0)
    require(transfer.bytes % beatBytes == 0)
  }

  endpoints.foreach { endpoint =>
    val publications = table.indices
      .filter(index => route(index).source == endpoint.name)
      .map(index => table(index))
      .map(transfer => (transfer.sourceOffset, transfer.bytes))
      .distinct
    require(publications.size <= 1)
  }

  val taskWidth: Int = math.max(1, log2Ceil(table.size))

  def endpoint(name: String): AutoEndpointSpec = endpointMap(name)
  def transfer(task: Int): AutoTransferSpec = table(task)
  def route(task: Int): AutoLinkSpec = links(transfer(task).route)
  def sourceAddress(task: Int): BigInt = {
    val spec = route(task)
    endpoint(spec.source).buffer.get.baseAddress + transfer(task).sourceOffset
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

class AutoTransfer(params: AutoLinkParams) extends Bundle {
  val task = UInt(params.taskWidth.W)
  val sourceAddress = UInt(params.addressWidth.W)
  val destinationOffset = UInt(params.addressWidth.W)
  val bytes = UInt(params.lengthWidth.W)
}

class AutoTransferDone(params: AutoLinkParams) extends Bundle {
  val task = UInt(params.taskWidth.W)
  val status = UInt(AutoLinkStatus.Width.W)
  val detail = UInt(params.detailWidth.W)
}

class AutoRelease extends Bundle {
  val start = Bool()
}

/** TileLink remains the data interface. These channels add automatic control. */
class AutoEndpointIO(params: AutoLinkParams) extends Bundle {
  val watch = Flipped(Decoupled(new AutoWatch(params)))
  val produced = Decoupled(new AutoEvent(params))
  val transfer = Flipped(Decoupled(new AutoTransfer(params)))
  val transferred = Decoupled(new AutoTransferDone(params))
  val release = Flipped(Decoupled(new AutoRelease))
  val complete = Decoupled(new AutoEvent(params))
}

class AutoEndpointAsyncLink(params: AutoLinkParams) extends Bundle {
  private val crossing = AsyncQueueParams.singleton()
  val watch = new AsyncBundle(new AutoWatch(params), crossing)
  val produced = Flipped(new AsyncBundle(new AutoEvent(params), crossing))
  val transfer = new AsyncBundle(new AutoTransfer(params), crossing)
  val transferred = Flipped(new AsyncBundle(new AutoTransferDone(params), crossing))
  val release = new AsyncBundle(new AutoRelease, crossing)
  val complete = Flipped(new AsyncBundle(new AutoEvent(params), crossing))
}
