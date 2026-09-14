package chipyard.socgen.link

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.subsystem.{BaseSubsystem, InstantiatesHierarchicalElements, PBUS}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{AsyncQueueParams, FromAsyncBundle, ToAsyncBundle}
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import org.chipsalliance.diplomacy.lazymodule.LazyModule

case object AutoLinkKey extends Field[Option[AutoLinkParams]](None)

class WithAutoLink(params: AutoLinkParams) extends Config((_, _, _) => { case AutoLinkKey => Some(params) })

class AutoBroadcast(params: AutoLinkParams, outputCount: Int) extends Module {
  require(outputCount > 0)

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AutoTileEvent(params)))
    val out = Vec(outputCount, Decoupled(new AutoTileEvent(params)))
    val busy = Output(Bool())
  })

  val event = Reg(new AutoTileEvent(params))
  val pending = RegInit(VecInit(Seq.fill(outputCount)(false.B)))
  val idle = !pending.reduce(_ || _)
  io.busy := !idle

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
  private val endpoint = params.endpoint(spec.endpoint)
  private val buffered = endpoint.bufferedInput && !external

  val io = IO(new Bundle {
    val transfers = Input(Vec(params.dependencies.size, new AutoTransfer(params)))
    val regions = Input(Vec(params.stages.size, new AutoRegion(params.lengthWidth)))
    val dependency = Flipped(Vec(incoming.size, Decoupled(new AutoTileEvent(params))))
    val output = Vec(outgoing.size, Decoupled(new AutoTileEvent(params)))
    val claim = Decoupled(UInt(params.jobWidth.W))
    val execute = Decoupled(UInt(params.jobWidth.W))
    val slot = Input(UInt(params.slotWidth.W))
    val consumed = Vec(incomingCopies.size, Valid(UInt(params.slotWidth.W)))
    val release = Output(Bool())
    val releaseTile = Output(new AutoTile(params.lengthWidth))
    val releaseSlot = Output(UInt(params.slotWidth.W))
    val busy = Output(Bool())
    val working = Output(Bool())
    val computing = Output(Bool())
    val workingTile = Output(UInt((2 * params.lengthWidth).W))
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
  // Buffered input can advance after compute acceptance while execution keeps its output context.
  val inputState = if (buffered) RegInit(State.waitDependencies) else state
  val dependency = Reg(Vec(math.max(1, incoming.size), new AutoEvent(params)))
  val singleTile = WireDefault(0.U.asTypeOf(new AutoTile(params.lengthWidth)))
  singleTile.rows := 1.U
  singleTile.columns := 1.U
  singleTile.last := true.B
  val tile = RegInit(singleTile)
  val slot = RegInit(0.U(params.slotWidth.W))
  val sourceSlots = Reg(Vec(math.max(1, incoming.size), UInt(params.slotWidth.W)))
  val consumed = RegInit(VecInit(Seq.fill(math.max(1, incomingCopies.size))(false.B)))
  val dependencyValid = RegInit(VecInit(Seq.fill(math.max(1, incoming.size))(false.B)))
  val output = Reg(Vec(math.max(1, outgoing.size), new AutoEvent(params)))
  val outputValid = RegInit(VecInit(Seq.fill(math.max(1, outgoing.size))(false.B)))
  val copyIndex = RegInit(0.U(math.max(1, log2Ceil(math.max(1, incomingCopies.size))).W))
  val failed = RegInit(false.B)
  val failure = Reg(new AutoEvent(params))
  val activeTile = if (buffered) RegInit(singleTile) else tile
  val activeSlot = if (buffered) RegInit(0.U(params.slotWidth.W)) else slot
  val activeSources = if (buffered && !endpoint.releaseOnCopy) Reg(chiselTypeOf(sourceSlots)) else sourceSlots
  val activeConsumed = if (buffered && !endpoint.releaseOnCopy) {
    RegInit(VecInit(Seq.fill(math.max(1, incomingCopies.size))(false.B)))
  } else consumed
  val publicationSeen = RegInit(false.B)
  val computeSeen = RegInit(false.B)
  val resultValid = RegInit(false.B)
  val result = Reg(new AutoEvent(params))
  val resultSent = RegInit(false.B)
  val runFailed = RegInit(false.B)
  val runFailure = Reg(new AutoEvent(params))

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

  val region = AutoTileBinding.region(activeTile, io.regions(index))
  val currentSlot = Mux(inputState === State.claim, io.slot, slot)
  val copies = (incomingCopies ++ dataOutputs).distinct.map { task =>
    val dependency = params.dependencies(task)
    val copy = dependency.copy.get
    val source = params.endpoint(params.stage(dependency.source.get).endpoint).buffer.get
    val destination = params.endpoint(params.stage(dependency.destination).endpoint)
    val transfer = io.transfers(task)
    val copyTile = if (dependency.destination == index) tile else activeTile
    val sourceTile = AutoTileBinding.region(copyTile, io.regions(dependency.source.get))
    val bytes = Mux(transfer.bytesPerPixel === 0.U, copy.bytes.U,
      sourceTile.rows * sourceTile.columns * transfer.bytesPerPixel)
    val destinationBytes = bytes * copy.expansion.U
    val sourceSlot = if (dependency.destination == index) sourceSlots(incoming.indexOf(task)) else activeSlot
    val destinationSlot = if (dependency.destination == index) currentSlot else 0.U
    val sourceOffset = transfer.sourceOffset +& sourceSlot * transfer.sourceStride
    val destinationOffset = if (destination.bufferedInput) {
      transfer.destinationOffset +& destinationSlot * transfer.destinationStride
    } else transfer.destinationOffset
    val request = WireDefault(0.U.asTypeOf(new AutoCopyRequest(params)))
    request.task := task.U
    request.job := params.stage(dependency.destination).job.U
    request.tile := AutoTileBinding.region(copyTile, io.regions(dependency.destination))
    request.sourceTile := sourceTile
    request.sourceSlot := sourceSlot
    request.destinationSlot := destinationSlot
    request.sourceAddress := source.baseAddress.U + sourceOffset
    request.destinationOffset := destinationOffset
    request.bytes := bytes
    request.destinationBytes := destinationBytes
    task -> request
  }.toMap

  val haveDependency = dependencyValid.reduce(_ || _)
  val firstIncoming = if (incoming.nonEmpty) {
    PriorityMux(io.dependency.map(input => input.valid -> input.bits.tile))
  } else {
    singleTile
  }
  val joinTile = Mux(haveDependency, tile, firstIncoming)
  io.dependency.zipWithIndex.foreach { case (input, inputIndex) =>
    input.ready := !dependencyValid(inputIndex) && inputState === State.waitDependencies &&
      input.bits.tile.asUInt === joinTile.asUInt
    when(input.fire) {
      dependency(inputIndex) := input.bits.event
      sourceSlots(inputIndex) := input.bits.slot
      tile := input.bits.tile
      dependencyValid(inputIndex) := true.B
    }
  }
  io.output.zipWithIndex.foreach { case (event, outputIndex) =>
    event.valid := outputValid(outputIndex)
    event.bits.event := output(outputIndex)
    event.bits.tile := activeTile
    event.bits.slot := activeSlot
    when(event.fire) {
      outputValid(outputIndex) := false.B
    }
  }

  io.claim.valid := inputState === State.claim
  io.claim.bits := spec.job.U
  io.execute.valid := buffered.B && inputState === State.requestCompute && state === State.waitDependencies
  io.execute.bits := spec.job.U
  io.release := state === State.release
  io.releaseTile := activeTile
  io.releaseSlot := activeSlot
  io.busy := (state =/= State.waitDependencies && state =/= State.waitRearm) ||
    (inputState =/= State.waitDependencies && inputState =/= State.waitRearm) || haveDependency || outputsPending || resultValid
  io.computing := (state === State.requestCompute && !failed) || (state === State.waitCompute && !computeSeen)
  io.working := io.computing || (!failed && (inputState === State.waitCopy || inputState === State.requestCompute))
  io.workingTile := Mux(io.computing, activeTile.id, tile.id)
  io.finished := state === State.waitRearm && inputState === State.waitRearm

  io.watchOutput.valid := state === State.armWatch
  io.watchOutput.bits.job := spec.job.U
  io.watchOutput.bits.slot := activeSlot
  io.watchOutput.bits.tile := region
  io.watchOutput.bits.address := dataOutputs.headOption.map(task => copies(task).sourceAddress).getOrElse(0.U)
  io.watchOutput.bits.bytes := dataOutputs.headOption.map(task => copies(task).bytes).getOrElse(0.U)

  io.reportOutput.ready := (state === State.requestCopy || state === State.waitCopy ||
    state === State.requestCompute || state === State.waitCompute || state === State.waitExternal) &&
    publication.nonEmpty.B && !publicationSeen && io.reportOutput.bits.job === spec.job.U

  io.requestCopy.valid := inputState === State.requestCopy
  io.requestCopy.bits := 0.U.asTypeOf(new AutoCopyRequest(params))
  if (incomingCopies.nonEmpty) {
    io.requestCopy.bits := VecInit(incomingCopies.map(task => copies(task)))(copyIndex)
  }
  io.reportCopy.ready := inputState === State.waitCopy && io.reportCopy.bits.task === currentCopy

  io.requestCompute.valid := state === State.requestCompute
  io.requestCompute.bits.job := spec.job.U
  io.requestCompute.bits.slot := activeSlot
  io.requestCompute.bits.tile := region
  io.requestCompute.bits.start := !failed
  io.reportCompute.ready := state === State.waitCompute && !computeSeen &&
    io.reportCompute.bits.job === spec.job.U

  incomingCopies.zipWithIndex.foreach { case (task, input) =>
    val copied = endpoint.releaseOnCopy.B && io.reportCopy.fire && io.reportCopy.bits.task === task.U
    val complete = (!endpoint.releaseOnCopy.B && io.reportCompute.fire) ||
      (io.requestCompute.fire && !io.requestCompute.bits.start)
    io.consumed(input).valid := (!consumed(input) && copied) || (!activeConsumed(input) && complete)
    io.consumed(input).bits := Mux(copied, sourceSlots(incoming.indexOf(task)), activeSources(incoming.indexOf(task)))
    when(copied) {
      consumed(input) := true.B
    }
    when(complete) {
      activeConsumed(input) := true.B
    }
  }

  io.result.valid := resultValid
  io.result.bits := result
  when(io.result.fire) {
    resultValid := false.B
  }

  when(inputState === State.waitDependencies && allDependencies) {
    failed := hasDependencyFailure
    when(hasDependencyFailure) {
      failure := firstFailure
      failure.stage := index.U
      failure.job := spec.job.U
    }
    inputState := State.claim
  }
  when(io.claim.fire) {
    slot := io.slot
    when(failed) {
      inputState := State.requestCompute
    }.elsewhen((publication.nonEmpty && !buffered).B) {
      inputState := State.armWatch
    }.elsewhen(incomingCopies.nonEmpty.B) {
      copyIndex := 0.U
      inputState := State.requestCopy
    }.otherwise {
      inputState := State.requestCompute
    }
  }
  if (buffered) {
    when(io.execute.fire) {
      activeTile := tile
      activeSlot := slot
      if (!endpoint.releaseOnCopy) {
        activeSources := sourceSlots
        activeConsumed := consumed
      }
      state := Mux(!failed && publication.nonEmpty.B, State.armWatch, State.requestCompute)
    }
  }
  when(io.watchOutput.fire) {
    when(external.B) {
      state := State.waitExternal
    }.elsewhen((incomingCopies.nonEmpty && !buffered).B) {
      copyIndex := 0.U
      state := State.requestCopy
    }.otherwise {
      state := State.requestCompute
    }
  }
  when(io.requestCopy.fire) {
    inputState := State.waitCopy
  }
  when(io.reportCopy.fire) {
    when(io.reportCopy.bits.status =/= AutoLinkStatus.Success) {
      failed := true.B
      failure.stage := index.U
      failure.job := spec.job.U
      failure.status := io.reportCopy.bits.status
      failure.detail := io.reportCopy.bits.detail
      failure.data := 0.U
      inputState := State.requestCompute
    }.elsewhen(copyIndex +& 1.U === incomingCopies.size.U) {
      inputState := State.requestCompute
    }.otherwise {
      copyIndex := copyIndex + 1.U
      inputState := State.requestCopy
    }
  }
  when(io.requestCompute.fire) {
    when(io.requestCompute.bits.start) {
      state := State.waitCompute
    }.otherwise {
      result := failure
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
    controlOutputs.foreach { dependencyIndex =>
      val outputIndex = outgoing.indexOf(dependencyIndex)
      output(outputIndex) := io.reportCompute.bits
      output(outputIndex).stage := index.U
      outputValid(outputIndex) := true.B
    }
  }

  val errors = Seq(
    (!buffered.B && inputState === State.waitDependencies && allDependencies && hasDependencyFailure) -> firstFailure,
    (io.requestCompute.fire && !io.requestCompute.bits.start) -> failure,
    (io.reportOutput.fire && io.reportOutput.bits.status =/= AutoLinkStatus.Success) -> io.reportOutput.bits,
    (io.reportCompute.fire && io.reportCompute.bits.status =/= AutoLinkStatus.Success) -> io.reportCompute.bits)
  when(!runFailed && errors.map(_._1).reduce(_ || _)) {
    runFailed := true.B
    runFailure := PriorityMux(errors)
    runFailure.stage := index.U
    runFailure.job := spec.job.U
  }

  val outputComplete = (!publication.nonEmpty.B || publicationSeen) && computeSeen
  when(state === State.waitCompute && outputComplete && !outputsPending) {
    when(!activeTile.last || (resultSent && !resultValid)) {
      state := State.release
    }.elsewhen(!resultSent) {
      when(runFailed) {
        result := runFailure
      }
      resultValid := true.B
      resultSent := true.B
    }
  }
  when(state === State.waitExternal && publicationSeen && !outputsPending) {
    state := State.release
  }
  when(state === State.release) {
    output.foreach(_ := 0.U.asTypeOf(new AutoEvent(params)))
    outputValid.foreach(_ := false.B)
    if (buffered && !endpoint.releaseOnCopy) {
      activeConsumed.foreach(_ := false.B)
    }
    publicationSeen := false.B
    computeSeen := false.B
    resultValid := false.B
    resultSent := false.B
    result := 0.U.asTypeOf(new AutoEvent(params))
    when(activeTile.last) {
      runFailed := false.B
    }
    state := State.waitRearm
  }
  when(state === State.waitRearm && io.rearm) {
    state := (if (external) State.claim else State.waitDependencies)
  }
  when(if (buffered) io.requestCompute.fire else io.release) {
    // Keep the prepared job's elements stable until the adapter accepts compute.
    dependency.foreach(_ := 0.U.asTypeOf(new AutoEvent(params)))
    dependencyValid.foreach(_ := false.B)
    consumed.foreach(_ := false.B)
    copyIndex := 0.U
    failed := false.B
    failure := 0.U.asTypeOf(new AutoEvent(params))
    inputState := State.waitRearm
  }
  if (buffered) {
    when(inputState === State.waitRearm && io.rearm && (!tile.last || state === State.waitRearm)) {
      inputState := State.waitDependencies
    }
  }
}

/** Synchronous graph scheduling shared by the fabric and its protocol tests. */
class AutoScheduler(params: AutoLinkParams) extends Module {
  val io = IO(new Bundle {
    val transfers = Input(Vec(params.dependencies.size, new AutoTransfer(params)))
    val regions = Input(Vec(params.stages.size, new AutoRegion(params.lengthWidth)))
    val root = Flipped(Decoupled(new AutoTileEvent(params)))
    val endpoint = Vec(params.endpoints.size, Flipped(new AutoEndpointIO(params)))
    val result = Vec(params.stages.size, Decoupled(new AutoEvent(params)))
    val busy = Output(Bool())
    val overlap = Output(Bool())
    val activeCount = Output(UInt(math.max(1, log2Ceil(params.endpoints.size + 1)).W))
  })
  private val rooted = params.stages.indices.forall(stage =>
    params.dependencies.exists(_.destination == stage))
  val ports = params.endpoints.zipWithIndex.map { case (endpoint, index) =>
    endpoint.name -> io.endpoint(index)
  }.toMap
  val stages = params.stages.indices.map(stage => Module(new AutoStage(params, stage)))
  val working = params.endpoints.map { endpoint =>
    val local = params.stages.indices.filter(params.stage(_).endpoint == endpoint.name).map(stages(_))
    val candidates = local.map(stage => stage.io.computing -> stage.io.workingTile) ++
      local.map(stage => stage.io.working -> stage.io.workingTile)
    (local.map(_.io.working).reduce(_ || _), PriorityMux(candidates))
  }
  io.activeCount := PopCount(working.indices.map { index =>
    val (active, tile) = working(index)
    val duplicate = working.take(index).map { case (otherActive, otherTile) =>
      otherActive && otherTile === tile
    }.reduceOption(_ || _).getOrElse(false.B)
    active && !duplicate
  })
  io.overlap := io.activeCount >= 2.U
  stages.foreach { stage =>
    stage.io.transfers := io.transfers
    stage.io.regions := io.regions
  }
  val rearm = stages.map(_.io.finished).reduce(_ && _)
  stages.foreach(_.io.rearm := (if (rooted) true.B else rearm))

  val pending = scala.collection.mutable.ArrayBuffer.empty[Bool]
  pending ++= stages.map(_.io.busy)

  params.dependencies.indices.foreach { dependency =>
    val spec = params.dependencies(dependency)
    val destinationInputs = params.dependencies.indices
      .filter(params.dependencies(_).destination == spec.destination)
    val input = destinationInputs.indexOf(dependency)
    spec.source.foreach { source =>
      val sourceOutputs = params.dependencies.indices
        .filter(params.dependencies(_).source.contains(source))
      val output = sourceOutputs.indexOf(dependency)
      val depth = params.endpoint(params.stage(source).endpoint).bufferSlots
      val queue = Module(new Queue(new AutoTileEvent(params), depth))
      queue.io.enq <> stages(source).io.output(output)
      stages(spec.destination).io.dependency(input) <> queue.io.deq
      pending += queue.io.deq.valid
    }
  }

  val root = io.root
  val rootDependencies = params.dependencies.indices.filter(params.dependencies(_).source.isEmpty)
  if (rootDependencies.nonEmpty) {
    val broadcast = Module(new AutoBroadcast(params, rootDependencies.size))
    broadcast.io.in <> root
    pending += broadcast.io.busy
    rootDependencies.zipWithIndex.foreach { case (dependency, output) =>
      val destination = params.dependencies(dependency).destination
      val destinationInputs = params.dependencies.indices
        .filter(params.dependencies(_).destination == destination)
      val input = destinationInputs.indexOf(dependency)
      val depth = params.endpoint(params.stage(destination).endpoint).bufferSlots
      val queue = Module(new Queue(new AutoTileEvent(params), depth))
      queue.io.enq <> broadcast.io.out(output)
      stages(destination).io.dependency(input) <> queue.io.deq
      pending += queue.io.deq.valid
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
    val copyOwner = if (endpoint.bufferedInput) RegInit(0.U(params.jobWidth.W)) else owner
    val copyOwnerValid = if (endpoint.bufferedInput) RegInit(false.B) else ownerValid
    val available = WireDefault(true.B)
    val selectedSlot = WireDefault(0.U(params.slotWidth.W))

    if (endpoint.hasStorage) {
      val occupied = RegInit(VecInit(Seq.fill(endpoint.bufferSlots)(false.B)))
      val produced = RegInit(VecInit(Seq.fill(endpoint.bufferSlots)(false.B)))
      val consumers = RegInit(VecInit(Seq.fill(endpoint.bufferSlots)(
        VecInit(Seq.fill(params.dependencies.size)(false.B)))))
      available := !occupied.reduce(_ && _)
      selectedSlot := PriorityEncoder(occupied.map(!_))
      pending += occupied.reduce(_ || _)

      for (slot <- 0 until endpoint.bufferSlots) {
        when(occupied(slot) && produced(slot) && !consumers(slot).reduce(_ || _)) {
          occupied(slot) := false.B
        }
        when(arbiter.io.out.fire && selectedSlot === slot.U) {
          occupied(slot) := true.B
          produced(slot) := false.B
          params.dependencies.zipWithIndex.foreach { case (dependency, task) =>
            consumers(slot)(task) := dependency.source.filter(source =>
              dependency.copy.nonEmpty && params.stage(source).endpoint == endpoint.name)
              .map(source => arbiter.io.out.bits === params.stage(source).job.U).getOrElse(false.B)
          }
        }
        endpointStages.foreach { stage =>
          when(stages(stage).io.release && stages(stage).io.releaseSlot === slot.U) {
            produced(slot) := true.B
          }
        }
        params.dependencies.zipWithIndex.foreach { case (dependency, task) =>
          dependency.source.filter(source => dependency.copy.nonEmpty &&
            params.stage(source).endpoint == endpoint.name).foreach { _ =>
            val copies = params.dependencies.indices.filter(index =>
              params.dependencies(index).destination == dependency.destination && params.dependencies(index).copy.nonEmpty)
            val consumed = stages(dependency.destination).io.consumed(copies.indexOf(task))
            when(consumed.valid && consumed.bits === slot.U) {
              consumers(slot)(task) := false.B
            }
          }
        }
      }
    }

    endpointStages.zipWithIndex.foreach { case (stage, input) =>
      arbiter.io.in(input) <> stages(stage).io.claim
      val external = !params.dependencies.exists(_.destination == stage)
      if (endpoint.bufferedInput && external) {
        arbiter.io.in(input).valid := stages(stage).io.claim.valid && !ownerValid
        stages(stage).io.claim.ready := arbiter.io.in(input).ready && !ownerValid
        when(stages(stage).io.claim.fire) {
          owner := params.stage(stage).job.U
          ownerValid := true.B
        }
      }
      stages(stage).io.slot := selectedSlot
    }
    arbiter.io.out.ready := !copyOwnerValid && available
    when(arbiter.io.out.fire) {
      copyOwner := arbiter.io.out.bits
      copyOwnerValid := true.B
    }
    if (endpoint.bufferedInput) {
      val execution = Module(new RRArbiter(UInt(params.jobWidth.W), endpointStages.size))
      endpointStages.zipWithIndex.foreach { case (stage, input) =>
        execution.io.in(input) <> stages(stage).io.execute
      }
      execution.io.out.ready := !ownerValid
      when(execution.io.out.fire) {
        owner := execution.io.out.bits
        ownerValid := true.B
      }
    } else {
      endpointStages.foreach(stage => stages(stage).io.execute.ready := false.B)
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
      val copying = copyOwnerValid && copyOwner === params.stage(stageIndex).job.U

      stage.io.watchOutput.ready := selected && port.watchOutput.ready
      stage.io.requestCopy.ready := copying && port.requestCopy.ready
      stage.io.requestCompute.ready := selected && port.requestCompute.ready
      stage.io.reportOutput.valid := selected && port.reportOutput.valid
      stage.io.reportOutput.bits := port.reportOutput.bits
      stage.io.reportCopy.valid := copying && port.reportCopy.valid
      stage.io.reportCopy.bits := port.reportCopy.bits
      stage.io.reportCompute.valid := selected && port.reportCompute.valid
      stage.io.reportCompute.bits := port.reportCompute.bits

      when(selected) {
        port.watchOutput.valid := stage.io.watchOutput.valid
        port.watchOutput.bits := stage.io.watchOutput.bits
        port.requestCompute.valid := stage.io.requestCompute.valid
        port.requestCompute.bits := stage.io.requestCompute.bits
        port.reportOutput.ready := stage.io.reportOutput.ready
        port.reportCompute.ready := stage.io.reportCompute.ready
        when(stage.io.release) {
          ownerValid := false.B
        }
      }
      when(copying) {
        port.requestCopy.valid := stage.io.requestCopy.valid
        port.requestCopy.bits := stage.io.requestCopy.bits
        port.reportCopy.ready := stage.io.reportCopy.ready
        val external = !params.dependencies.exists(_.destination == stageIndex)
        when(if (endpoint.bufferedInput && !external) stage.io.requestCompute.fire else stage.io.release) {
          copyOwnerValid := false.B
        }
      }
    }
  }

  stages.zipWithIndex.foreach { case (stage, index) =>
    io.result(index) <> stage.io.result
  }
  io.busy := pending.reduce(_ || _)
}

/** Clock crossings around the logical stage scheduler. */
class AutoLinkFabric(params: AutoLinkParams)(implicit p: Parameters) extends ClockSinkDomain(ClockSinkParameters())(p) {
  private val endpointNodes = params.endpoints.map { endpoint =>
    endpoint.name -> BundleBridgeSource(() => new AutoEndpointAsyncLink(params))
  }.toMap
  private val resultNodes = params.resultNames.map { name =>
    name -> BundleBridgeSource(() => Decoupled(new AutoEvent(params)))
  }.toMap
  val rootNode = BundleBridgeSink[DecoupledIO[AutoRun]]()
  val stateNode = BundleBridgeSource(() => new AutoProgress)

  def endpoint(name: String): BundleBridgeSource[AutoEndpointAsyncLink] = endpointNodes(name)
  def result(name: String): BundleBridgeSource[DecoupledIO[AutoEvent]] = resultNodes(name)

  override lazy val module = new FabricImpl
  class FabricImpl extends Impl {
    withClockAndReset(clock, reset) {
      val scheduler = Module(new AutoScheduler(params))
      val run = rootNode.in.head._1
      val transfers = RegInit(AutoTileBinding.defaults(params))
      val regions = RegInit(0.U.asTypeOf(Vec(params.stages.size, new AutoRegion(params.lengthWidth))))
      val cursor = Module(new AutoTileCursor(params.lengthWidth))
      val active = RegInit(false.B)
      val cycles = RegInit(0.U(64.W))
      val overlap = RegInit(0.U(64.W))
      val peakActive = RegInit(0.U(64.W))
      cursor.io.start.valid := run.valid && !active
      cursor.io.start.bits := run.bits.plan
      run.ready := cursor.io.start.ready && !active
      when(run.fire) {
        transfers := run.bits.transfers
        regions := run.bits.regions
        active := true.B
        cycles := 0.U
        overlap := 0.U
        peakActive := 0.U
      }
      when(active && !cursor.io.busy && !scheduler.io.busy) {
        active := false.B
      }
      when(active) {
        cycles := cycles + 1.U
        when(scheduler.io.overlap) {
          overlap := overlap + 1.U
        }
        when(scheduler.io.activeCount > peakActive) {
          peakActive := scheduler.io.activeCount
        }
      }
      scheduler.io.transfers := transfers
      scheduler.io.regions := regions
      scheduler.io.root.valid := cursor.io.out.valid
      scheduler.io.root.bits.event := 0.U.asTypeOf(new AutoEvent(params))
      scheduler.io.root.bits.tile := cursor.io.out.bits
      scheduler.io.root.bits.slot := 0.U
      cursor.io.out.ready := scheduler.io.root.ready
      stateNode.out.head._1.running := active
      stateNode.out.head._1.emitting := cursor.io.busy
      stateNode.out.head._1.cycles := cycles
      stateNode.out.head._1.overlap := overlap
      stateNode.out.head._1.peakActive := peakActive
      params.endpoints.zipWithIndex.foreach { case (endpoint, index) =>
        val async = endpointNodes(endpoint.name).out.head._1
        val port = scheduler.io.endpoint(index)
        async.watchOutput <> ToAsyncBundle(port.watchOutput, AsyncQueueParams.singleton())
        async.requestCopy <> ToAsyncBundle(port.requestCopy, AsyncQueueParams.singleton())
        async.requestCompute <> ToAsyncBundle(port.requestCompute, AsyncQueueParams.singleton())
        port.reportOutput <> FromAsyncBundle(async.reportOutput)
        port.reportCopy <> FromAsyncBundle(async.reportCopy)
        port.reportCompute <> FromAsyncBundle(async.reportCompute)
      }
      params.stages.zipWithIndex.foreach { case (stage, index) =>
        if (params.resultNames.contains(stage.name)) {
          resultNodes(stage.name).out.head._1 <> scheduler.io.result(index)
        } else {
          scheduler.io.result(index).ready := true.B
        }
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
    fabric.rootNode := root.runNode
    root.stateNode := fabric.stateNode
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
