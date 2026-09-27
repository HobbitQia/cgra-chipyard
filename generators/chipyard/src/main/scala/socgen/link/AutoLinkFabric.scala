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

class AutoStage(params: AutoLinkParams, index: Int) extends Module {
  private val tasks = 0 until params.dependencyCount

  val io = IO(new Bundle {
    val transfers = Input(Vec(params.dependencyCount, new AutoTransfer(params)))
    val regions = Input(Vec(params.stageCount, new AutoRegion(params.lengthWidth)))
    val jobs = Input(Vec(params.stageCount, UInt(params.jobWidth.W)))
    val stages = Input(Vec(params.stageCount, new AutoGraphStage(params)))
    val edges = Input(Vec(params.dependencyCount, new AutoGraphEdge(params)))
    val dependency = Flipped(Vec(params.dependencyCount, Decoupled(new AutoTileEvent(params))))
    val output = Vec(params.dependencyCount, Decoupled(new AutoTileEvent(params)))
    val claim = Decoupled(UInt(params.stageWidth.W))
    val execute = Decoupled(UInt(params.stageWidth.W))
    val slot = Input(UInt(params.slotWidth.W))
    val consumed = Vec(params.dependencyCount, Valid(UInt(params.slotWidth.W)))
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

  val spec = io.stages(index)
  val incoming = VecInit(io.edges.map(edge => edge.enabled && edge.destination === index.U))
  val outgoing = VecInit(io.edges.map(edge => edge.enabled && !edge.root && edge.source === index.U))
  val incomingCopies = VecInit(tasks.map(task => incoming(task) && io.edges(task).copy))
  val dataOutputs = VecInit(tasks.map(task => outgoing(task) && io.edges(task).copy))
  val controlOutputs = VecInit(tasks.map(task => outgoing(task) && !io.edges(task).copy))
  val publication = dataOutputs.asUInt.orR || spec.output.writeback
  val external = !incoming.asUInt.orR
  val buffered = VecInit(params.endpoints.map(_.bufferedInput.B))(spec.endpoint) && !external
  val releaseOnCopy = VecInit(params.endpoints.map(_.releaseOnCopy.B))(spec.endpoint)
  val state = RegInit(State.waitDependencies)
  // Buffered input can advance after compute acceptance while execution keeps its output context.
  val preparation = RegInit(State.waitDependencies)
  val inputState = Mux(buffered, preparation, state)
  def setInput(next: UInt): Unit = {
    when(buffered) { preparation := next }.otherwise { state := next }
  }
  val dependency = Reg(Vec(params.dependencyCount, new AutoEvent(params)))
  val singleTile = WireDefault(0.U.asTypeOf(new AutoTile(params.lengthWidth)))
  singleTile.rows := 1.U
  singleTile.columns := 1.U
  singleTile.last := true.B
  val tile = RegInit(singleTile)
  val slot = RegInit(0.U(params.slotWidth.W))
  val sourceSlots = Reg(Vec(params.dependencyCount, UInt(params.slotWidth.W)))
  val consumed = RegInit(VecInit(Seq.fill(params.dependencyCount)(false.B)))
  val dependencyValid = RegInit(VecInit(Seq.fill(params.dependencyCount)(false.B)))
  val output = Reg(Vec(params.dependencyCount, new AutoEvent(params)))
  val outputValid = RegInit(VecInit(Seq.fill(params.dependencyCount)(false.B)))
  val copyIndex = RegInit(0.U(params.dependencyWidth.W))
  val failed = RegInit(false.B)
  val failure = Reg(new AutoEvent(params))
  val executionTile = RegInit(singleTile)
  val executionSlot = RegInit(0.U(params.slotWidth.W))
  val activeTile = Mux(buffered, executionTile, tile)
  val activeSlot = Mux(buffered, executionSlot, slot)
  val executionSources = Reg(chiselTypeOf(sourceSlots))
  val executionConsumed = RegInit(VecInit(Seq.fill(params.dependencyCount)(false.B)))
  val activeSources = Mux(buffered, executionSources, sourceSlots)
  val activeConsumed = Mux(buffered, executionConsumed, consumed)
  val publicationSeen = RegInit(false.B)
  val computeSeen = RegInit(false.B)
  val resultValid = RegInit(false.B)
  val result = Reg(new AutoEvent(params))
  val resultSent = RegInit(false.B)
  val runFailed = RegInit(false.B)
  val runFailure = Reg(new AutoEvent(params))

  val allDependencies = tasks.map(task => !incoming(task) || dependencyValid(task)).reduce(_ && _)
  val dependencyFailures = tasks.map { input =>
    incoming(input) && dependencyValid(input) && dependency(input).status =/= AutoLinkStatus.Success
  }
  val hasDependencyFailure = dependencyFailures.reduce(_ || _)
  val firstFailure = PriorityMux(dependencyFailures.zip(dependency))
  val outputsPending = outputValid.reduce(_ || _)
  val currentCopy = copyIndex
  val nextCopies = VecInit(tasks.map(task => incomingCopies(task) && task.U > copyIndex))

  val region = AutoTileBinding.region(activeTile, io.regions(index))
  val currentSlot = Mux(inputState === State.claim, io.slot, slot)
  val copies = VecInit(tasks.map { task =>
    val edge = io.edges(task)
    val transfer = io.transfers(task)
    val copyTile = Mux(incoming(task), tile, activeTile)
    val sourceTile = Mux(edge.root, copyTile, AutoTileBinding.region(copyTile, io.regions(edge.source)))
    val bytes = Mux(transfer.bytesPerPixel === 0.U, edge.bytes,
      sourceTile.rows * sourceTile.columns * transfer.bytesPerPixel)
    val destinationBytes = bytes << edge.expansion
    val sourceSlot = Mux(incoming(task), sourceSlots(task), activeSlot)
    val destinationSlot = Mux(incoming(task), currentSlot, 0.U)
    val sourceOffset = transfer.sourceOffset +& sourceSlot * transfer.sourceStride
    val destinationBuffered = VecInit(params.endpoints.map(_.bufferedInput.B))(io.stages(edge.destination).endpoint)
    val destinationOffset = transfer.destinationOffset + Mux(destinationBuffered, destinationSlot * transfer.destinationStride, 0.U)
    val request = WireDefault(0.U.asTypeOf(new AutoCopyRequest(params)))
    request.task := task.U
    request.job := io.jobs(edge.destination)
    request.tile := AutoTileBinding.region(copyTile, io.regions(edge.destination))
    request.sourceTile := sourceTile
    request.sourceSlot := sourceSlot
    request.destinationSlot := destinationSlot
    request.sourceAddress := transfer.sourceBase + sourceOffset
    request.destinationOffset := destinationOffset
    request.bytes := bytes
    request.destinationBytes := destinationBytes
    request
  })

  val haveDependency = dependencyValid.reduce(_ || _)
  val firstIncoming = PriorityMux(io.dependency.map(input => input.valid -> input.bits.tile))
  val joinTile = Mux(haveDependency, tile, firstIncoming)
  io.dependency.zipWithIndex.foreach { case (input, inputIndex) =>
    input.ready := spec.enabled && incoming(inputIndex) && !dependencyValid(inputIndex) && inputState === State.waitDependencies &&
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
  io.claim.bits := index.U
  io.execute.valid := buffered && inputState === State.requestCompute && state === State.waitDependencies
  io.execute.bits := index.U
  io.release := state === State.release
  io.releaseTile := activeTile
  io.releaseSlot := activeSlot
  io.busy := (state =/= State.waitDependencies && state =/= State.waitRearm) ||
    (inputState =/= State.waitDependencies && inputState =/= State.waitRearm) || haveDependency || outputsPending || resultValid
  io.computing := (state === State.requestCompute && !failed) || (state === State.waitCompute && !computeSeen)
  io.working := io.computing || (!failed && (inputState === State.waitCopy || inputState === State.requestCompute))
  io.workingTile := Mux(io.computing, activeTile.id, tile.id)
  io.finished := !spec.enabled || (state === State.waitRearm && inputState === State.waitRearm)

  io.watchOutput.valid := state === State.armWatch
  io.watchOutput.bits.job := io.jobs(index)
  io.watchOutput.bits.slot := activeSlot
  io.watchOutput.bits.tile := region
  io.watchOutput.bits.address := copies(PriorityEncoder(dataOutputs)).sourceAddress
  io.watchOutput.bits.bytes := copies(PriorityEncoder(dataOutputs)).bytes
  io.watchOutput.bits.writeback := spec.output.writeback
  when(spec.output.writeback) {
    io.watchOutput.bits.address := spec.output.address + activeSlot * spec.output.stride
    io.watchOutput.bits.bytes := Mux(spec.output.bytesPerPixel === 0.U, spec.output.bytes,
      region.rows * region.columns * spec.output.bytesPerPixel)
  }

  io.reportOutput.ready := (state === State.requestCopy || state === State.waitCopy ||
    state === State.requestCompute || state === State.waitCompute || state === State.waitExternal) &&
    publication && !publicationSeen && io.reportOutput.bits.job === io.jobs(index)

  io.requestCopy.valid := inputState === State.requestCopy
  io.requestCopy.bits := copies(copyIndex)
  io.reportCopy.ready := inputState === State.waitCopy && io.reportCopy.bits.task === currentCopy

  io.requestCompute.valid := state === State.requestCompute
  io.requestCompute.bits.job := io.jobs(index)
  io.requestCompute.bits.slot := activeSlot
  io.requestCompute.bits.tile := region
  io.requestCompute.bits.start := !failed
  io.requestCompute.bits.hasInput := incomingCopies.asUInt.orR
  io.reportCompute.ready := state === State.waitCompute && !computeSeen &&
    io.reportCompute.bits.job === io.jobs(index)

  tasks.foreach { input =>
    val copied = incomingCopies(input) && releaseOnCopy && io.reportCopy.fire && io.reportCopy.bits.task === input.U
    val complete = incomingCopies(input) && ((!releaseOnCopy && io.reportCompute.fire) ||
      (io.requestCompute.fire && !io.requestCompute.bits.start))
    io.consumed(input).valid := (!consumed(input) && copied) || (!activeConsumed(input) && complete)
    io.consumed(input).bits := Mux(copied, sourceSlots(input), activeSources(input))
    when(copied) {
      consumed(input) := true.B
    }
    when(complete) {
      when(buffered) { executionConsumed(input) := true.B }.otherwise { consumed(input) := true.B }
    }
  }

  io.result.valid := resultValid
  io.result.bits := result
  when(io.result.fire) {
    resultValid := false.B
  }

  when(spec.enabled && inputState === State.waitDependencies && allDependencies) {
    failed := hasDependencyFailure
    when(hasDependencyFailure) {
      failure := firstFailure
      failure.stage := index.U
      failure.job := io.jobs(index)
    }
    setInput(State.claim)
  }
  when(io.claim.fire) {
    slot := io.slot
    when(failed) {
      setInput(State.requestCompute)
    }.elsewhen(publication && !buffered) {
      setInput(State.armWatch)
    }.elsewhen(incomingCopies.asUInt.orR) {
      copyIndex := PriorityEncoder(incomingCopies)
      setInput(State.requestCopy)
    }.otherwise {
      setInput(State.requestCompute)
    }
  }
  when(buffered) {
    when(io.execute.fire) {
      executionTile := tile
      executionSlot := slot
      executionSources := sourceSlots
      executionConsumed := consumed
      state := Mux(!failed && publication, State.armWatch, State.requestCompute)
    }
  }
  when(io.watchOutput.fire) {
    when(external) {
      state := State.waitExternal
    }.elsewhen(incomingCopies.asUInt.orR && !buffered) {
      copyIndex := PriorityEncoder(incomingCopies)
      state := State.requestCopy
    }.otherwise {
      state := State.requestCompute
    }
  }
  when(io.requestCopy.fire) {
    setInput(State.waitCopy)
  }
  when(io.reportCopy.fire) {
    when(io.reportCopy.bits.status =/= AutoLinkStatus.Success) {
      failed := true.B
      failure.stage := index.U
      failure.job := io.jobs(index)
      failure.status := io.reportCopy.bits.status
      failure.detail := io.reportCopy.bits.detail
      failure.data := 0.U
      setInput(State.requestCompute)
    }.elsewhen(!nextCopies.asUInt.orR) {
      setInput(State.requestCompute)
    }.otherwise {
      copyIndex := PriorityEncoder(nextCopies)
      setInput(State.requestCopy)
    }
  }
  when(io.requestCompute.fire) {
    when(io.requestCompute.bits.start) {
      state := State.waitCompute
    }.otherwise {
      result := failure
      computeSeen := true.B
      publicationSeen := true.B
      tasks.foreach { outputIndex =>
        output(outputIndex) := failure
        outputValid(outputIndex) := outgoing(outputIndex)
      }
      state := State.waitCompute
    }
  }
  when(io.reportOutput.fire) {
    publicationSeen := true.B
    tasks.foreach { outputIndex =>
      when(dataOutputs(outputIndex)) {
        output(outputIndex) := io.reportOutput.bits
        output(outputIndex).stage := index.U
        outputValid(outputIndex) := true.B
      }
    }
  }
  when(io.reportCompute.fire) {
    computeSeen := true.B
    result := io.reportCompute.bits
    result.stage := index.U
    tasks.foreach { outputIndex =>
      when(controlOutputs(outputIndex)) {
        output(outputIndex) := io.reportCompute.bits
        output(outputIndex).stage := index.U
        outputValid(outputIndex) := true.B
      }
    }
  }

  val errors = Seq(
    (!buffered && inputState === State.waitDependencies && allDependencies && hasDependencyFailure) -> firstFailure,
    (io.requestCompute.fire && !io.requestCompute.bits.start) -> failure,
    (io.reportOutput.fire && io.reportOutput.bits.status =/= AutoLinkStatus.Success) -> io.reportOutput.bits,
    (io.reportCompute.fire && io.reportCompute.bits.status =/= AutoLinkStatus.Success) -> io.reportCompute.bits)
  when(!runFailed && errors.map(_._1).reduce(_ || _)) {
    runFailed := true.B
    runFailure := PriorityMux(errors)
    runFailure.stage := index.U
    runFailure.job := io.jobs(index)
  }

  val outputComplete = (!publication || publicationSeen) && computeSeen
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
    executionConsumed.foreach(_ := false.B)
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
    state := State.waitDependencies
  }
  when(Mux(buffered, io.requestCompute.fire, io.release)) {
    // Keep the prepared job's elements stable until the adapter accepts compute.
    dependency.foreach(_ := 0.U.asTypeOf(new AutoEvent(params)))
    dependencyValid.foreach(_ := false.B)
    consumed.foreach(_ := false.B)
    copyIndex := 0.U
    failed := false.B
    failure := 0.U.asTypeOf(new AutoEvent(params))
    setInput(State.waitRearm)
  }
  when(buffered) {
    when(inputState === State.waitRearm && io.rearm && (!tile.last || state === State.waitRearm)) {
      preparation := State.waitDependencies
    }
  }
}

/** Synchronous graph scheduling shared by the fabric and its protocol tests. */
class AutoScheduler(params: AutoLinkParams) extends Module {
  val io = IO(new Bundle {
    val transfers = Input(Vec(params.dependencyCount, new AutoTransfer(params)))
    val regions = Input(Vec(params.stageCount, new AutoRegion(params.lengthWidth)))
    val jobs = Input(Vec(params.stageCount, UInt(params.jobWidth.W)))
    val stages = Input(Vec(params.stageCount, new AutoGraphStage(params)))
    val edges = Input(Vec(params.dependencyCount, new AutoGraphEdge(params)))
    val root = Flipped(Decoupled(new AutoTileEvent(params)))
    val endpoint = Vec(params.endpoints.size, Flipped(new AutoEndpointIO(params)))
    val result = Vec(params.stageCount, Decoupled(new AutoEvent(params)))
    val busy = Output(Bool())
    val overlap = Output(Bool())
    val activeCount = Output(UInt(math.max(1, log2Ceil(params.endpoints.size + 1)).W))
  })
  private val stageIds = 0 until params.stageCount
  private val taskIds = 0 until params.dependencyCount
  val stages = stageIds.map(stage => Module(new AutoStage(params, stage)))
  val external = VecInit(stageIds.map(stage =>
    !io.edges.map(edge => edge.enabled && edge.destination === stage.U).reduce(_ || _)))
  val rooted = stageIds.map(stage => !io.stages(stage).enabled || !external(stage)).reduce(_ && _)
  val rearm = stages.map(_.io.finished).reduce(_ && _)
  val pending = scala.collection.mutable.ArrayBuffer.empty[Bool]
  pending ++= stages.map(_.io.busy)
  stages.foreach { stage =>
    stage.io.transfers := io.transfers
    stage.io.regions := io.regions
    stage.io.jobs := io.jobs
    stage.io.stages := io.stages
    stage.io.edges := io.edges
    stage.io.rearm := rooted || rearm
    stage.io.claim.ready := false.B
    stage.io.execute.ready := false.B
    stage.io.slot := 0.U
    stage.io.watchOutput.ready := false.B
    stage.io.requestCopy.ready := false.B
    stage.io.requestCompute.ready := false.B
    stage.io.reportOutput.valid := false.B
    stage.io.reportOutput.bits := 0.U.asTypeOf(new AutoEvent(params))
    stage.io.reportCopy.valid := false.B
    stage.io.reportCopy.bits := 0.U.asTypeOf(new AutoCopyResult(params))
    stage.io.reportCompute.valid := false.B
    stage.io.reportCompute.bits := 0.U.asTypeOf(new AutoEvent(params))
  }

  val working = params.endpoints.indices.map { endpoint =>
    val local = stageIds.map(stage => io.stages(stage).enabled && io.stages(stage).endpoint === endpoint.U)
    val candidates = stageIds.map(stage => (local(stage) && stages(stage).io.computing) -> stages(stage).io.workingTile) ++
      stageIds.map(stage => (local(stage) && stages(stage).io.working) -> stages(stage).io.workingTile)
    (stageIds.map(stage => local(stage) && stages(stage).io.working).reduce(_ || _), PriorityMux(candidates))
  }
  io.activeCount := PopCount(working.indices.map { index =>
    val (active, tile) = working(index)
    val duplicate = working.take(index).map { case (otherActive, otherTile) =>
      otherActive && otherTile === tile
    }.reduceOption(_ || _).getOrElse(false.B)
    active && !duplicate
  })
  io.overlap := io.activeCount >= 2.U

  val rootEvent = Reg(new AutoTileEvent(params))
  val rootPending = RegInit(VecInit(Seq.fill(params.dependencyCount)(false.B)))
  io.root.ready := !rootPending.asUInt.orR
  when(io.root.fire) {
    rootEvent := io.root.bits
    taskIds.foreach(task => rootPending(task) := io.edges(task).enabled && io.edges(task).root)
  }
  pending += rootPending.asUInt.orR
  val queueDepth = params.endpoints.map(_.bufferSlots).max
  taskIds.foreach { task =>
    val edge = io.edges(task)
    val queue = Module(new Queue(new AutoTileEvent(params), queueDepth))
    val source = VecInit(stages.map(_.io.output(task).bits))(edge.source)
    val sourceValid = VecInit(stages.map(_.io.output(task).valid))(edge.source)
    queue.io.enq.valid := edge.enabled && Mux(edge.root, rootPending(task), sourceValid)
    queue.io.enq.bits := Mux(edge.root, rootEvent, source)
    when(queue.io.enq.fire && edge.root) {
      rootPending(task) := false.B
    }
    stageIds.foreach { stage =>
      stages(stage).io.output(task).ready := edge.enabled && !edge.root && edge.source === stage.U && queue.io.enq.ready
      stages(stage).io.dependency(task).valid := edge.enabled && edge.destination === stage.U && queue.io.deq.valid
      stages(stage).io.dependency(task).bits := queue.io.deq.bits
    }
    queue.io.deq.ready := VecInit(stages.map(_.io.dependency(task).ready))(edge.destination)
    pending += queue.io.deq.valid
  }

  params.endpoints.zipWithIndex.foreach { case (endpoint, endpointIndex) =>
    val port = io.endpoint(endpointIndex)
    val arbiter = Module(new RRArbiter(UInt(params.stageWidth.W), params.stageCount))
    val owner = RegInit(0.U(params.stageWidth.W))
    val ownerValid = RegInit(false.B)
    val copyOwner = if (endpoint.bufferedInput) RegInit(0.U(params.stageWidth.W)) else owner
    val copyOwnerValid = if (endpoint.bufferedInput) RegInit(false.B) else ownerValid
    val available = WireDefault(true.B)
    val selectedSlot = WireDefault(0.U(params.slotWidth.W))
    val local = VecInit(io.stages.map(stage => stage.enabled && stage.endpoint === endpointIndex.U))

    val occupied = RegInit(VecInit(Seq.fill(endpoint.bufferSlots)(false.B)))
    val produced = RegInit(VecInit(Seq.fill(endpoint.bufferSlots)(false.B)))
    val consumers = RegInit(VecInit(Seq.fill(endpoint.bufferSlots)(
      VecInit(Seq.fill(params.dependencyCount)(false.B)))))
    val readers = VecInit(io.edges.map(edge => edge.enabled && !edge.root && edge.copy && edge.source === arbiter.io.out.bits))
    available := !occupied.reduce(_ && _)
    selectedSlot := PriorityEncoder(occupied.map(!_))
    pending += occupied.asUInt.orR
    for (slot <- 0 until endpoint.bufferSlots) {
      when(occupied(slot) && produced(slot) && !consumers(slot).asUInt.orR) {
        occupied(slot) := false.B
      }
      when(arbiter.io.out.fire && selectedSlot === slot.U) {
        // Streaming producers retain output ownership; streaming terminals need no slot.
        occupied(slot) := endpoint.hasStorage.B || readers.asUInt.orR
        produced(slot) := false.B
        consumers(slot) := readers
      }
      stageIds.foreach { stage =>
        when(local(stage) && stages(stage).io.release && stages(stage).io.releaseSlot === slot.U) {
          produced(slot) := true.B
        }
      }
      taskIds.foreach { task =>
        val edge = io.edges(task)
        val consumed = VecInit(stages.map(_.io.consumed(task)))(edge.destination)
        when(edge.enabled && !edge.root && edge.copy && local(edge.source) &&
          consumed.valid && consumed.bits === slot.U) {
          consumers(slot)(task) := false.B
        }
      }
    }

    stageIds.foreach { stage =>
      val access = local(stage) && (if (endpoint.bufferedInput) !external(stage) || !ownerValid else true.B)
      arbiter.io.in(stage).valid := access && stages(stage).io.claim.valid
      arbiter.io.in(stage).bits := stage.U
      when(local(stage)) {
        stages(stage).io.claim.ready := access && arbiter.io.in(stage).ready
        stages(stage).io.slot := selectedSlot
      }
      if (endpoint.bufferedInput) {
        when(arbiter.io.in(stage).fire && external(stage)) {
          owner := stage.U
          ownerValid := true.B
        }
      }
    }
    arbiter.io.out.ready := !copyOwnerValid && available
    when(arbiter.io.out.fire) {
      copyOwner := arbiter.io.out.bits
      copyOwnerValid := true.B
    }
    if (endpoint.bufferedInput) {
      val execution = Module(new RRArbiter(UInt(params.stageWidth.W), params.stageCount))
      stageIds.foreach { stage =>
        execution.io.in(stage).valid := local(stage) && stages(stage).io.execute.valid
        execution.io.in(stage).bits := stage.U
        when(local(stage)) {
          stages(stage).io.execute.ready := execution.io.in(stage).ready
        }
      }
      execution.io.out.ready := !ownerValid
      when(execution.io.out.fire) {
        owner := execution.io.out.bits
        ownerValid := true.B
      }
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
    stageIds.foreach { stageIndex =>
      val stage = stages(stageIndex)
      val selected = ownerValid && owner === stageIndex.U
      val copying = copyOwnerValid && copyOwner === stageIndex.U
      when(local(stageIndex)) {
        stage.io.watchOutput.ready := selected && port.watchOutput.ready
        stage.io.requestCopy.ready := copying && port.requestCopy.ready
        stage.io.requestCompute.ready := selected && port.requestCompute.ready
        stage.io.reportOutput.valid := selected && port.reportOutput.valid
        stage.io.reportOutput.bits := port.reportOutput.bits
        stage.io.reportCopy.valid := copying && port.reportCopy.valid
        stage.io.reportCopy.bits := port.reportCopy.bits
        stage.io.reportCompute.valid := selected && port.reportCompute.valid
        stage.io.reportCompute.bits := port.reportCompute.bits
      }
      when(selected) {
        port.watchOutput.valid := stage.io.watchOutput.valid
        port.watchOutput.bits := stage.io.watchOutput.bits
        port.requestCompute.valid := stage.io.requestCompute.valid
        port.requestCompute.bits := stage.io.requestCompute.bits
        port.reportOutput.ready := stage.io.reportOutput.ready
        port.reportCompute.ready := stage.io.reportCompute.ready
        when(stage.io.release) { ownerValid := false.B }
      }
      when(copying) {
        port.requestCopy.valid := stage.io.requestCopy.valid
        port.requestCopy.bits := stage.io.requestCopy.bits
        port.reportCopy.ready := stage.io.reportCopy.ready
        when(Mux(endpoint.bufferedInput.B && !external(stageIndex), stage.io.requestCompute.fire, stage.io.release)) {
          copyOwnerValid := false.B
        }
      }
    }
  }
  stages.zipWithIndex.foreach { case (stage, index) => io.result(index) <> stage.io.result }
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
      val regions = RegInit(0.U.asTypeOf(Vec(params.stageCount, new AutoRegion(params.lengthWidth))))
      val jobs = RegInit(AutoTileBinding.jobs(params))
      val stages = RegInit(AutoTileBinding.stages(params))
      val edges = RegInit(AutoTileBinding.edges(params))
      val cursor = Module(new AutoTileCursor(params.lengthWidth))
      val active = RegInit(false.B)
      val failed = RegInit(false.B)
      val cycles = RegInit(0.U(64.W))
      val overlap = RegInit(0.U(64.W))
      val peakActive = RegInit(0.U(64.W))
      cursor.io.start.valid := run.valid && !active
      cursor.io.start.bits := run.bits.plan
      run.ready := cursor.io.start.ready && !active
      when(scheduler.io.result.map(result => result.fire && result.bits.status =/= AutoLinkStatus.Success).reduce(_ || _)) {
        failed := true.B
      }
      when(run.fire) {
        transfers := run.bits.transfers
        regions := run.bits.regions
        jobs := run.bits.jobs
        stages := run.bits.stages
        edges := run.bits.edges
        active := true.B
        failed := false.B
        cycles := 0.U
        overlap := 0.U
        peakActive := 0.U
      }
      val done = active && !cursor.io.busy && !scheduler.io.busy
      when(done) {
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
      scheduler.io.jobs := jobs
      scheduler.io.stages := stages
      scheduler.io.edges := edges
      scheduler.io.root.valid := cursor.io.out.valid
      scheduler.io.root.bits.event := 0.U.asTypeOf(new AutoEvent(params))
      scheduler.io.root.bits.tile := cursor.io.out.bits
      scheduler.io.root.bits.slot := 0.U
      cursor.io.out.ready := scheduler.io.root.ready
      stateNode.out.head._1.running := active
      stateNode.out.head._1.done := done
      stateNode.out.head._1.failed := failed
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
      params.resultNames.zipWithIndex.foreach { case (name, index) =>
        resultNodes(name).out.head._1 <> scheduler.io.result(index)
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
