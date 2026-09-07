package chipyard.socgen.link

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.subsystem.{BaseSubsystem, InstantiatesHierarchicalElements, PBUS}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams, FromAsyncBundle, ToAsyncBundle}
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import org.chipsalliance.diplomacy.lazymodule.LazyModule

case object AutoLinkKey extends Field[Option[AutoLinkParams]](None)

class WithAutoLink(params: AutoLinkParams) extends Config((_, _, _) => { case AutoLinkKey => Some(params) })

class AutoBroadcast(params: AutoLinkParams, outputCount: Int) extends Module {
  require(outputCount > 0)

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AutoTileEvent(params)))
    val out = Vec(outputCount, Decoupled(new AutoTileEvent(params)))
  })

  val event = Reg(new AutoTileEvent(params))
  val pending = RegInit(VecInit(Seq.fill(outputCount)(false.B)))
  val idle = !pending.reduce(_ || _)

  io.in.ready := idle
  io.out.zipWithIndex.foreach { case (output, index) =>
    output.valid := pending(index)
    output.bits := event
    when(output.fire) {
      pending(index) := false.B
    }
  }
  when(io.in.fire) {
    event := io.in.bits
    pending.foreach(_ := true.B)
  }
}

class AutoStage(params: AutoLinkParams, index: Int) extends Module {
  private val spec = params.stage(index)
  private val incoming = params.dependencies.indices.filter(params.dependencies(_).destination == index)
  private val outgoing = params.dependencies.indices.filter(params.dependencies(_).source.contains(index))
  private val incomingCopies = incoming.filter(params.dependencies(_).copy.nonEmpty)
  private val dataOutputs = outgoing.filter(params.dependencies(_).copy.nonEmpty)
  private val controlOutputs = outgoing.filter(params.dependencies(_).copy.isEmpty)
  private val publication = dataOutputs.headOption.map(params.dependencies(_).copy.get)
  private val external = incoming.isEmpty

  val io = IO(new Bundle {
    val dependency = Flipped(Vec(incoming.size, Decoupled(new AutoTileEvent(params))))
    val output = Vec(outgoing.size, Decoupled(new AutoTileEvent(params)))
    val claim = Decoupled(UInt(params.jobWidth.W))
    val release = Output(Bool())
    val finished = Output(Bool())
    val rearm = Input(Bool())
    val watchOutput = Decoupled(new AutoWatch(params))
    val reportOutput = Flipped(Decoupled(new AutoEvent(params)))
    val requestCopy = Decoupled(new AutoCopyRequest(params))
    val reportCopy = Flipped(Decoupled(new AutoCopyResult(params)))
    val requestCompute = Decoupled(new AutoComputeRequest(params))
    val reportCompute = Flipped(Decoupled(new AutoEvent(params)))
    val result = Decoupled(new AutoEvent(params))
  })

  object State {
    val waitDependencies :: claim :: armWatch :: requestCopy :: waitCopy :: requestCompute :: waitCompute :: waitExternal :: release :: waitRearm :: Nil = Enum(10)
  }

  val state = RegInit(if (external) State.claim else State.waitDependencies)
  val dependency = Reg(Vec(math.max(1, incoming.size), new AutoEvent(params)))
  val singleTile = WireDefault(0.U.asTypeOf(new AutoTile(params.lengthWidth)))
  singleTile.rows := 1.U
  singleTile.columns := 1.U
  singleTile.last := true.B
  val tile = RegInit(singleTile)
  val dependencyValid = RegInit(VecInit(Seq.fill(math.max(1, incoming.size))(false.B)))
  val output = Reg(Vec(math.max(1, outgoing.size), new AutoEvent(params)))
  val outputValid = RegInit(VecInit(Seq.fill(math.max(1, outgoing.size))(false.B)))
  val copyIndex = RegInit(0.U(math.max(1, log2Ceil(math.max(1, incomingCopies.size))).W))
  val failed = RegInit(false.B)
  val failure = Reg(new AutoEvent(params))
  val publicationSeen = RegInit(false.B)
  val computeSeen = RegInit(false.B)
  val resultValid = RegInit(false.B)
  val result = Reg(new AutoEvent(params))

  val allDependencies = if (incoming.nonEmpty) dependencyValid.reduce(_ && _) else true.B
  val dependencyFailures = incoming.indices.map { input =>
    dependencyValid(input) && dependency(input).status =/= AutoLinkStatus.Success
  }
  val hasDependencyFailure = if (dependencyFailures.nonEmpty) dependencyFailures.reduce(_ || _) else false.B
  val firstFailure = if (dependencyFailures.nonEmpty) {
    PriorityMux(dependencyFailures.zip(dependency))
  } else {
    0.U.asTypeOf(new AutoEvent(params))
  }
  val outputsPending = if (outgoing.nonEmpty) outputValid.reduce(_ || _) else false.B
  val currentCopy = if (incomingCopies.nonEmpty) {
    VecInit(incomingCopies.map(_.U(params.dependencyWidth.W)))(copyIndex)
  } else {
    0.U(params.dependencyWidth.W)
  }

  io.dependency.zipWithIndex.foreach { case (input, inputIndex) =>
    input.ready := !dependencyValid(inputIndex) && state === State.waitDependencies
    when(input.fire) {
      dependency(inputIndex) := input.bits.event
      tile := input.bits.tile
      dependencyValid(inputIndex) := true.B
    }
  }
  io.output.zipWithIndex.foreach { case (event, outputIndex) =>
    event.valid := outputValid(outputIndex)
    event.bits.event := output(outputIndex)
    event.bits.tile := tile
    when(event.fire) {
      outputValid(outputIndex) := false.B
    }
  }

  io.claim.valid := state === State.claim
  io.claim.bits := spec.job.U
  io.release := state === State.release
  io.finished := state === State.waitRearm

  io.watchOutput.valid := state === State.armWatch
  io.watchOutput.bits.job := spec.job.U
  io.watchOutput.bits.tile := tile
  io.watchOutput.bits.address := publication
    .map(copy => params.endpoint(spec.endpoint).buffer.get.baseAddress + copy.sourceOffset)
    .getOrElse(BigInt(0)).U
  io.watchOutput.bits.bytes := publication.map(_.bytes).getOrElse(0).U

  io.reportOutput.ready := (state === State.requestCopy || state === State.waitCopy ||
    state === State.requestCompute || state === State.waitCompute || state === State.waitExternal) &&
    publication.nonEmpty.B && !publicationSeen && io.reportOutput.bits.job === spec.job.U

  io.requestCopy.valid := state === State.requestCopy
  io.requestCopy.bits := 0.U.asTypeOf(new AutoCopyRequest(params))
  if (incomingCopies.nonEmpty) {
    io.requestCopy.bits.task := currentCopy
    io.requestCopy.bits.job := spec.job.U
    io.requestCopy.bits.tile := tile
    io.requestCopy.bits.sourceAddress := VecInit(incomingCopies.map(task => params.sourceAddress(task).U))(copyIndex)
    io.requestCopy.bits.destinationOffset := VecInit(incomingCopies.map(task =>
      params.dependencies(task).copy.get.destinationOffset.U))(copyIndex)
    io.requestCopy.bits.bytes := VecInit(incomingCopies.map(task =>
      params.dependencies(task).copy.get.bytes.U))(copyIndex)
    io.requestCopy.bits.destinationBytes := VecInit(incomingCopies.map(task =>
      params.dependencies(task).copy.get.destinationBytes.U))(copyIndex)
  }
  io.reportCopy.ready := state === State.waitCopy && io.reportCopy.bits.task === currentCopy

  io.requestCompute.valid := state === State.requestCompute
  io.requestCompute.bits.job := spec.job.U
  io.requestCompute.bits.tile := tile
  io.requestCompute.bits.start := !failed
  io.reportCompute.ready := state === State.waitCompute && !computeSeen &&
    io.reportCompute.bits.job === spec.job.U

  io.result.valid := resultValid
  io.result.bits := result
  when(io.result.fire) {
    resultValid := false.B
  }

  when(state === State.waitDependencies && allDependencies) {
    failed := hasDependencyFailure
    when(hasDependencyFailure) {
      failure := firstFailure
      failure.stage := index.U
      failure.job := spec.job.U
    }
    state := State.claim
  }
  when(io.claim.fire) {
    when(failed) {
      state := State.requestCompute
    }.elsewhen(publication.nonEmpty.B) {
      state := State.armWatch
    }.elsewhen(incomingCopies.nonEmpty.B) {
      copyIndex := 0.U
      state := State.requestCopy
    }.otherwise {
      state := State.requestCompute
    }
  }
  when(io.watchOutput.fire) {
    when(external.B) {
      state := State.waitExternal
    }.elsewhen(incomingCopies.nonEmpty.B) {
      copyIndex := 0.U
      state := State.requestCopy
    }.otherwise {
      state := State.requestCompute
    }
  }
  when(io.requestCopy.fire) {
    state := State.waitCopy
  }
  when(io.reportCopy.fire) {
    when(io.reportCopy.bits.status =/= AutoLinkStatus.Success) {
      failed := true.B
      failure.stage := index.U
      failure.job := spec.job.U
      failure.status := io.reportCopy.bits.status
      failure.detail := io.reportCopy.bits.detail
      failure.data := 0.U
      state := State.requestCompute
    }.elsewhen(copyIndex + 1.U === incomingCopies.size.U) {
      state := State.requestCompute
    }.otherwise {
      copyIndex := copyIndex + 1.U
      state := State.requestCopy
    }
  }
  when(io.requestCompute.fire) {
    when(io.requestCompute.bits.start) {
      state := State.waitCompute
    }.otherwise {
      result := failure
      resultValid := true.B
      computeSeen := true.B
      publicationSeen := true.B
      outgoing.indices.foreach { outputIndex =>
        output(outputIndex) := failure
        outputValid(outputIndex) := true.B
      }
      state := State.waitCompute
    }
  }
  when(io.reportOutput.fire) {
    publicationSeen := true.B
    dataOutputs.foreach { dependencyIndex =>
      val outputIndex = outgoing.indexOf(dependencyIndex)
      output(outputIndex) := io.reportOutput.bits
      output(outputIndex).stage := index.U
      outputValid(outputIndex) := true.B
    }
  }
  when(io.reportCompute.fire) {
    computeSeen := true.B
    result := io.reportCompute.bits
    result.stage := index.U
    resultValid := true.B
    controlOutputs.foreach { dependencyIndex =>
      val outputIndex = outgoing.indexOf(dependencyIndex)
      output(outputIndex) := io.reportCompute.bits
      output(outputIndex).stage := index.U
      outputValid(outputIndex) := true.B
    }
  }

  val outputComplete = (!publication.nonEmpty.B || publicationSeen) && computeSeen
  when(state === State.waitCompute && outputComplete && !outputsPending && !resultValid) {
    state := State.release
  }
  when(state === State.waitExternal && publicationSeen && !outputsPending) {
    state := State.release
  }
  when(state === State.release) {
    dependency.foreach(_ := 0.U.asTypeOf(new AutoEvent(params)))
    dependencyValid.foreach(_ := false.B)
    output.foreach(_ := 0.U.asTypeOf(new AutoEvent(params)))
    outputValid.foreach(_ := false.B)
    copyIndex := 0.U
    failed := false.B
    failure := 0.U.asTypeOf(new AutoEvent(params))
    publicationSeen := false.B
    computeSeen := false.B
    resultValid := false.B
    result := 0.U.asTypeOf(new AutoEvent(params))
    state := State.waitRearm
  }
  when(state === State.waitRearm && io.rearm) {
    state := (if (external) State.claim else State.waitDependencies)
  }
}

/** Owns the logical stage graph, dependency state, and automatic control routing. */
class AutoLinkFabric(params: AutoLinkParams)(implicit p: Parameters) extends ClockSinkDomain(ClockSinkParameters())(p) {
  private val endpointNodes = params.endpoints.map { endpoint =>
    endpoint.name -> BundleBridgeSource(() => new AutoEndpointAsyncLink(params))
  }.toMap
  private val resultNodes = params.resultNames.map { name =>
    name -> BundleBridgeSource(() => new AsyncBundle(new AutoEvent(params), AsyncQueueParams.singleton()))
  }.toMap
  val rootNode = BundleBridgeSink[AsyncBundle[AutoTileEvent]]()

  def endpoint(name: String): BundleBridgeSource[AutoEndpointAsyncLink] = endpointNodes(name)
  def result(name: String): BundleBridgeSource[AsyncBundle[AutoEvent]] = resultNodes(name)

  override lazy val module = new FabricImpl
  class FabricImpl extends Impl {
    case class Port(
      watchOutput: DecoupledIO[AutoWatch],
      reportOutput: DecoupledIO[AutoEvent],
      requestCopy: DecoupledIO[AutoCopyRequest],
      reportCopy: DecoupledIO[AutoCopyResult],
      requestCompute: DecoupledIO[AutoComputeRequest],
      reportCompute: DecoupledIO[AutoEvent])

    withClockAndReset(clock, reset) {
      val ports = params.endpoints.map { endpoint =>
        val async = endpointNodes(endpoint.name).out.head._1
        val watchOutput = Wire(Decoupled(new AutoWatch(params)))
        val requestCopy = Wire(Decoupled(new AutoCopyRequest(params)))
        val requestCompute = Wire(Decoupled(new AutoComputeRequest(params)))
        async.watchOutput <> ToAsyncBundle(watchOutput, AsyncQueueParams.singleton())
        async.requestCopy <> ToAsyncBundle(requestCopy, AsyncQueueParams.singleton())
        async.requestCompute <> ToAsyncBundle(requestCompute, AsyncQueueParams.singleton())
        endpoint.name -> Port(
          watchOutput,
          FromAsyncBundle(async.reportOutput),
          requestCopy,
          FromAsyncBundle(async.reportCopy),
          requestCompute,
          FromAsyncBundle(async.reportCompute))
      }.toMap
      val stages = params.stages.indices.map(stage => Module(new AutoStage(params, stage)))
      // Start the next round only after every stage has released its endpoint.
      val rearm = stages.map(_.io.finished).reduce(_ && _)
      stages.foreach(_.io.rearm := rearm)

      params.dependencies.indices.foreach { dependency =>
        val spec = params.dependencies(dependency)
        val destinationInputs = params.dependencies.indices
          .filter(params.dependencies(_).destination == spec.destination)
        val input = destinationInputs.indexOf(dependency)
        spec.source.foreach { source =>
          val sourceOutputs = params.dependencies.indices
            .filter(params.dependencies(_).source.contains(source))
          val output = sourceOutputs.indexOf(dependency)
          stages(spec.destination).io.dependency(input) <> stages(source).io.output(output)
        }
      }

      val root = FromAsyncBundle(rootNode.in.head._1)
      val rootDependencies = params.dependencies.indices.filter(params.dependencies(_).source.isEmpty)
      if (rootDependencies.nonEmpty) {
        val broadcast = Module(new AutoBroadcast(params, rootDependencies.size))
        broadcast.io.in <> root
        rootDependencies.zipWithIndex.foreach { case (dependency, output) =>
          val destination = params.dependencies(dependency).destination
          val destinationInputs = params.dependencies.indices
            .filter(params.dependencies(_).destination == destination)
          val input = destinationInputs.indexOf(dependency)
          stages(destination).io.dependency(input) <> broadcast.io.out(output)
        }
      } else {
        root.ready := true.B
      }

      params.endpoints.foreach { endpoint =>
        val port = ports(endpoint.name)
        val endpointStages = params.stages.indices.filter(params.stage(_).endpoint == endpoint.name)
        val arbiter = Module(new RRArbiter(UInt(params.jobWidth.W), endpointStages.size))
        val owner = RegInit(0.U(params.jobWidth.W))
        val ownerValid = RegInit(false.B)

        endpointStages.zipWithIndex.foreach { case (stage, input) =>
          arbiter.io.in(input) <> stages(stage).io.claim
        }
        arbiter.io.out.ready := !ownerValid
        when(arbiter.io.out.fire) {
          owner := arbiter.io.out.bits
          ownerValid := true.B
        }

        port.watchOutput.valid := false.B
        port.watchOutput.bits := 0.U.asTypeOf(new AutoWatch(params))
        port.requestCopy.valid := false.B
        port.requestCopy.bits := 0.U.asTypeOf(new AutoCopyRequest(params))
        port.requestCompute.valid := false.B
        port.requestCompute.bits := 0.U.asTypeOf(new AutoComputeRequest(params))
        port.reportOutput.ready := false.B
        port.reportCopy.ready := false.B
        port.reportCompute.ready := false.B

        endpointStages.foreach { stageIndex =>
          val stage = stages(stageIndex)
          val selected = ownerValid && owner === params.stage(stageIndex).job.U

          stage.io.watchOutput.ready := selected && port.watchOutput.ready
          stage.io.requestCopy.ready := selected && port.requestCopy.ready
          stage.io.requestCompute.ready := selected && port.requestCompute.ready
          stage.io.reportOutput.valid := selected && port.reportOutput.valid
          stage.io.reportOutput.bits := port.reportOutput.bits
          stage.io.reportCopy.valid := selected && port.reportCopy.valid
          stage.io.reportCopy.bits := port.reportCopy.bits
          stage.io.reportCompute.valid := selected && port.reportCompute.valid
          stage.io.reportCompute.bits := port.reportCompute.bits

          when(selected) {
            port.watchOutput.valid := stage.io.watchOutput.valid
            port.watchOutput.bits := stage.io.watchOutput.bits
            port.requestCopy.valid := stage.io.requestCopy.valid
            port.requestCopy.bits := stage.io.requestCopy.bits
            port.requestCompute.valid := stage.io.requestCompute.valid
            port.requestCompute.bits := stage.io.requestCompute.bits
            port.reportOutput.ready := stage.io.reportOutput.ready
            port.reportCopy.ready := stage.io.reportCopy.ready
            port.reportCompute.ready := stage.io.reportCompute.ready
            when(stage.io.release) {
              ownerValid := false.B
            }
          }
        }
      }

      params.resultNames.foreach { name =>
        val stage = params.stages.indexWhere(_.name == name)
        resultNodes(name).out.head._1 <>
          ToAsyncBundle(stages(stage).io.result, AsyncQueueParams.singleton())
      }
      params.stages.indices.filterNot(stage => params.resultNames.contains(params.stage(stage).name)).foreach { stage =>
        stages(stage).io.result.ready := true.B
      }
    }
  }
}

trait CanHaveAutoLink {
  this: BaseSubsystem with InstantiatesHierarchicalElements =>
  private val pbus = locateTLBusWrapper(PBUS)

  val autoLink = p(AutoLinkKey).map { params =>
    val fabric = LazyModule(new AutoLinkFabric(params))
    val root = LazyModule(new AutoLinkRoot(params))
    fabric.rootNode := root.eventNode
    fabric.clockNode := pbus.fixedClockNode
    root.clockNode := pbus.fixedClockNode
    pbus.coupleTo("auto-link-root") {
      root.controlNode := TLBuffer() := TLFragmenter(
        root.controlNode.beatBytes,
        pbus.blockBytes) := _
    }
    fabric
  }
}
