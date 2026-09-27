package chipyard.socgen.link

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams}

object AutoLinkStatus {
  val Width = 4
  val Success = 0.U(Width.W)
  val SourceFailure = 1.U(Width.W)
  val SinkFailure = 2.U(Width.W)
  val ConfigFailure = 3.U(Width.W)
}

case class AutoBuffer(baseAddress: BigInt, sizeBytes: Int)

case class AutoEndpointSpec(name: String, buffer: Option[AutoBuffer], localBytes: Int,
    bufferedInput: Boolean = false, inputAlignment: Int = 1,
    bufferSlots: Int = 1, releaseOnCopy: Boolean = false, jobs: Int = 0) {
  require(isPow2(inputAlignment))
  require(bufferSlots > 0)
  val hasStorage: Boolean = buffer.nonEmpty || bufferedInput
}

case class AutoOutputSpec(address: BigInt, bytes: Int, stride: Int = 0, bytesPerPixel: Int = 0)

case class AutoStageSpec(name: String, endpoint: String, job: Int, jobs: Int = 1,
    output: Option[AutoOutputSpec] = None)

case class AutoCopySpec(sourceOffset: Int, destinationOffset: Int, bytes: Int, expansion: Int = 1,
    sourceAddress: Option[BigInt] = None, sourceStride: Int = 0, bytesPerPixel: Int = 0) {
  require(isPow2(expansion))
  val destinationBytes: BigInt = BigInt(bytes) * expansion
}

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
  resultWidth: Int = 32,
  runCapacity: Int = 0,
  stageCapacity: Int = 0,
  dependencyCapacity: Int = 0) {
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
    val jobs = stages.filter(_.endpoint == endpoint.name).flatMap(stage => stage.job until stage.job + stage.jobs)
    require(jobs == jobs.indices)
  }
  require(dependencies.forall(dependency =>
    dependency.source.forall(stages.indices.contains) &&
      stages.indices.contains(dependency.destination)))
  require(dependencies.forall(dependency => !dependency.source.contains(dependency.destination)))

  dependencies.foreach { dependency =>
    dependency.copy.foreach { copy =>
      val destination = endpointMap(stages(dependency.destination).endpoint)
      dependency.source.foreach { index =>
        val source = endpointMap(stages(index).endpoint)
        require(source.name != destination.name,
          "Data dependencies between stages on the same physical endpoint are unsupported")
        if (copy.sourceAddress.isEmpty) {
          require(source.buffer.nonEmpty)
          require(copy.sourceOffset + copy.bytes <= source.buffer.get.sizeBytes)
        }
      }
      require(copy.sourceOffset >= 0)
      require(copy.destinationOffset >= 0)
      require(copy.bytes > 0)
      require(copy.destinationOffset + copy.destinationBytes <= destination.localBytes)
      require(copy.destinationBytes < (BigInt(1) << lengthWidth))
      val alignment = if (copy.expansion == 1) beatBytes else copy.expansion
      require(copy.destinationOffset % alignment == 0)
      if (copy.expansion == 1) {
        require(copy.sourceOffset % beatBytes == 0)
        require(copy.bytes % beatBytes == 0)
      }
    }
  }

  stages.indices.foreach { index =>
    val publications = dependencies
      .filter(_.source.contains(index))
      .flatMap(_.copy)
      .map(copy => (copy.sourceOffset, copy.bytes))
      .distinct
    require(stages(index).output.nonEmpty || publications.size <= 1)
    if (!dependencies.exists(_.destination == index)) {
      require(publications.nonEmpty)
    }
  }

  val stageCount: Int = math.max(stageCapacity, stages.size)
  val dependencyCount: Int = math.max(dependencyCapacity, dependencies.size)
  val endpointWidth: Int = math.max(1, log2Ceil(endpoints.size))
  val dependencyWidth: Int = math.max(1, log2Ceil(dependencyCount))
  val jobWidth: Int = math.max(1, log2Ceil(endpoints.map(endpoint => jobCount(endpoint.name)).max))
  val stageWidth: Int = math.max(1, log2Ceil(stageCount))
  val slotWidth: Int = math.max(1, log2Ceil(endpoints.map(_.bufferSlots).max))
  val resultNames: Seq[String] = stages.map(_.name) ++ (stages.size until stageCount).map(index => s"stage$index")

  def endpoint(name: String): AutoEndpointSpec = endpointMap(name)
  def jobCount(name: String): Int = math.max(endpoint(name).jobs, stages.filter(_.endpoint == name).map(stage => stage.job + stage.jobs).foldLeft(0)(math.max))
  def stage(index: Int): AutoStageSpec = stages(index)
  def sourceAddress(dependency: Int): BigInt = {
    val spec = dependencies(dependency)
    val copy = spec.copy.get
    val base = copy.sourceAddress.getOrElse(endpoint(stage(spec.source.get).endpoint).buffer.get.baseAddress)
    base + copy.sourceOffset
  }
}

class AutoWatch(params: AutoLinkParams) extends Bundle {
  val job = UInt(params.jobWidth.W)
  val slot = UInt(params.slotWidth.W)
  val tile = new AutoTile(params.lengthWidth)
  val address = UInt(params.addressWidth.W)
  val bytes = UInt(params.lengthWidth.W)
  val writeback = Bool()
}

class AutoEvent(params: AutoLinkParams) extends Bundle {
  val stage = UInt(params.stageWidth.W)
  val job = UInt(params.jobWidth.W)
  val status = UInt(AutoLinkStatus.Width.W)
  val detail = UInt(params.detailWidth.W)
  val data = UInt(params.resultWidth.W)
}

class AutoTileEvent(params: AutoLinkParams) extends Bundle {
  val event = new AutoEvent(params)
  val tile = new AutoTile(params.lengthWidth)
  val slot = UInt(params.slotWidth.W)
}

class AutoCopyRequest(params: AutoLinkParams) extends Bundle {
  val task = UInt(params.dependencyWidth.W)
  val job = UInt(params.jobWidth.W)
  val tile = new AutoTile(params.lengthWidth)
  val sourceTile = new AutoTile(params.lengthWidth)
  val sourceSlot = UInt(params.slotWidth.W)
  val destinationSlot = UInt(params.slotWidth.W)
  val sourceAddress = UInt(params.addressWidth.W)
  val destinationOffset = UInt(params.addressWidth.W)
  val bytes = UInt(params.lengthWidth.W)
  val destinationBytes = UInt(params.lengthWidth.W)
}

class AutoCopyResult(params: AutoLinkParams) extends Bundle {
  val task = UInt(params.dependencyWidth.W)
  val status = UInt(AutoLinkStatus.Width.W)
  val detail = UInt(params.detailWidth.W)
}

class AutoComputeRequest(params: AutoLinkParams) extends Bundle {
  val job = UInt(params.jobWidth.W)
  val slot = UInt(params.slotWidth.W)
  val tile = new AutoTile(params.lengthWidth)
  val start = Bool()
  val hasInput = Bool()
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
