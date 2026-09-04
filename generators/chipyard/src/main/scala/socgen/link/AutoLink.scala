package chipyard.socgen.link

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

case class AutoEndpointSpec(name: String, buffer: Option[AutoBuffer], localBytes: Int)

case class AutoStageSpec(name: String, endpoint: String, job: Int)

case class AutoCopySpec(sourceOffset: Int, destinationOffset: Int, bytes: Int)

case class AutoDependencySpec(source: Option[Int], destination: Int, copy: Option[AutoCopySpec])

case class AutoLinkParams(
  stages: Seq[AutoStageSpec],
  dependencies: Seq[AutoDependencySpec],
  endpoints: Seq[AutoEndpointSpec],
  beatBytes: Int,
  controlAddress: BigInt,
  controlBytes: Int,
  addressWidth: Int = 64,
  lengthWidth: Int = 32,
  detailWidth: Int = 8,
  resultWidth: Int = 32) {
  require(stages.nonEmpty)
  require(dependencies.nonEmpty)
  require(endpoints.map(_.name).distinct.size == endpoints.size)
  require(stages.map(_.name).distinct.size == stages.size)
  require(isPow2(beatBytes))
  require(controlAddress >= 0)
  require(isPow2(controlBytes))

  private val endpointMap = endpoints.map(endpoint => endpoint.name -> endpoint).toMap
  require(stages.forall(stage => endpointMap.contains(stage.endpoint)))
  require(stages.forall(_.job >= 0))
  endpoints.foreach { endpoint =>
    val jobs = stages.filter(_.endpoint == endpoint.name).map(_.job)
    require(jobs == jobs.indices)
  }
  require(dependencies.forall(dependency =>
    dependency.source.forall(stages.indices.contains) &&
      stages.indices.contains(dependency.destination)))
  require(dependencies.forall(dependency => !dependency.source.contains(dependency.destination)))

  dependencies.foreach { dependency =>
    dependency.copy.foreach { copy =>
      require(dependency.source.nonEmpty)
      val source = endpointMap(stages(dependency.source.get).endpoint)
      val destination = endpointMap(stages(dependency.destination).endpoint)
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
  }

  stages.indices.foreach { index =>
    val publications = dependencies
      .filter(_.source.contains(index))
      .flatMap(_.copy)
      .map(copy => (copy.sourceOffset, copy.bytes))
      .distinct
    require(publications.size <= 1)
    if (!dependencies.exists(_.destination == index)) {
      require(publications.nonEmpty)
    }
  }

  val dependencyWidth: Int = math.max(1, log2Ceil(dependencies.size))
  val jobWidth: Int = math.max(1, log2Ceil(stages.map(_.job).max + 1))
  val stageWidth: Int = math.max(1, log2Ceil(stages.size))
  val resultNames: Seq[String] = dependencies.map(_.destination).distinct.map(stages(_).name)

  def endpoint(name: String): AutoEndpointSpec = endpointMap(name)
  def stage(index: Int): AutoStageSpec = stages(index)
  def sourceAddress(dependency: Int): BigInt = {
    val spec = dependencies(dependency)
    val source = stage(spec.source.get)
    endpoint(source.endpoint).buffer.get.baseAddress + spec.copy.get.sourceOffset
  }
}

class AutoWatch(params: AutoLinkParams) extends Bundle {
  val job = UInt(params.jobWidth.W)
  val address = UInt(params.addressWidth.W)
  val bytes = UInt(params.lengthWidth.W)
}

class AutoEvent(params: AutoLinkParams) extends Bundle {
  val stage = UInt(params.stageWidth.W)
  val job = UInt(params.jobWidth.W)
  val status = UInt(AutoLinkStatus.Width.W)
  val detail = UInt(params.detailWidth.W)
  val data = UInt(params.resultWidth.W)
}

class AutoCopyRequest(params: AutoLinkParams) extends Bundle {
  val task = UInt(params.dependencyWidth.W)
  val job = UInt(params.jobWidth.W)
  val sourceAddress = UInt(params.addressWidth.W)
  val destinationOffset = UInt(params.addressWidth.W)
  val bytes = UInt(params.lengthWidth.W)
}

class AutoCopyResult(params: AutoLinkParams) extends Bundle {
  val task = UInt(params.dependencyWidth.W)
  val status = UInt(AutoLinkStatus.Width.W)
  val detail = UInt(params.detailWidth.W)
}

class AutoComputeRequest(params: AutoLinkParams) extends Bundle {
  val job = UInt(params.jobWidth.W)
  val start = Bool()
}

/** TileLink remains the data interface. These channels add automatic control. */
class AutoEndpointIO(params: AutoLinkParams) extends Bundle {
  val watchOutput = Flipped(Decoupled(new AutoWatch(params)))
  val reportOutput = Decoupled(new AutoEvent(params))
  val requestCopy = Flipped(Decoupled(new AutoCopyRequest(params)))
  val reportCopy = Decoupled(new AutoCopyResult(params))
  val requestCompute = Flipped(Decoupled(new AutoComputeRequest(params)))
  val reportCompute = Decoupled(new AutoEvent(params))
}

class AutoEndpointAsyncLink(params: AutoLinkParams) extends Bundle {
  private val crossing = AsyncQueueParams.singleton()
  val watchOutput = new AsyncBundle(new AutoWatch(params), crossing)
  val reportOutput = Flipped(new AsyncBundle(new AutoEvent(params), crossing))
  val requestCopy = new AsyncBundle(new AutoCopyRequest(params), crossing)
  val reportCopy = Flipped(new AsyncBundle(new AutoCopyResult(params), crossing))
  val requestCompute = new AsyncBundle(new AutoComputeRequest(params), crossing)
  val reportCompute = Flipped(new AsyncBundle(new AutoEvent(params), crossing))
}
