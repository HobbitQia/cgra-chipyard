package chipyard.socgen.link

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.subsystem.{BaseSubsystem, InstantiatesHierarchicalElements, PBUS}
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams, FromAsyncBundle, ToAsyncBundle}
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import org.chipsalliance.diplomacy.lazymodule.LazyModule

case object AutoLinkKey extends Field[Option[AutoLinkParams]](None)

class WithAutoLink(params: AutoLinkParams) extends Config((_, _, _) => { case AutoLinkKey => Some(params) })

class AutoCopyTask(params: AutoLinkParams, index: Int) extends Module {
  private val spec = params.copy(index)

  val io = IO(new Bundle {
    val reportOutput = Flipped(Decoupled(new AutoEvent(params)))
    val requestCopy = Decoupled(new AutoCopyRequest(params))
    val reportCopy = Flipped(Decoupled(new AutoCopyResult(params)))
    val reportDependency = Decoupled(new AutoEvent(params))
  })

  val idle :: requestCopy :: waitCopy :: reportDependency :: Nil = Enum(4)
  val state = RegInit(idle)
  val event = Reg(new AutoEvent(params))

  io.reportOutput.ready := state === idle
  io.requestCopy.valid := state === requestCopy
  io.requestCopy.bits.task := index.U
  io.requestCopy.bits.sourceAddress := params.sourceAddress(index).U
  io.requestCopy.bits.destinationOffset := spec.destinationOffset.U
  io.requestCopy.bits.bytes := spec.bytes.U
  io.reportCopy.ready := state === waitCopy
  io.reportDependency.valid := state === reportDependency
  io.reportDependency.bits := event

  when(io.reportOutput.fire) {
    event := io.reportOutput.bits
    state := Mux(
      io.reportOutput.bits.status === AutoLinkStatus.Success,
      requestCopy,
      reportDependency)
  }
  when(io.requestCopy.fire) {
    state := waitCopy
  }
  when(io.reportCopy.fire) {
    event.status := io.reportCopy.bits.status
    event.detail := io.reportCopy.bits.detail
    event.data := 0.U
    state := reportDependency
  }
  when(io.reportDependency.fire) {
    state := idle
  }
}

class AutoJoin(params: AutoLinkParams, inputCount: Int) extends Module {
  val io = IO(new Bundle {
    val dependency = Flipped(Vec(inputCount, Decoupled(new AutoEvent(params))))
    val requestCompute = Decoupled(new AutoComputeRequest)
    val reportCompute = Flipped(Decoupled(new AutoEvent(params)))
    val result = Decoupled(new AutoEvent(params))
  })

  val waitDependencies :: requestCompute :: waitCompute :: reportResult :: Nil = Enum(4)
  val state = RegInit(waitDependencies)
  val dependencies = Reg(Vec(inputCount, new AutoEvent(params)))
  val dependencyValid = RegInit(VecInit(Seq.fill(inputCount)(false.B)))
  val resultEvent = Reg(new AutoEvent(params))
  val failures = (0 until inputCount).map { index =>
    dependencyValid(index) && dependencies(index).status =/= AutoLinkStatus.Success
  }
  val allReceived = dependencyValid.reduce(_ && _)
  val failed = failures.reduce(_ || _)

  for (index <- 0 until inputCount) {
    io.dependency(index).ready := !dependencyValid(index)
    when(io.dependency(index).fire) {
      dependencies(index) := io.dependency(index).bits
      dependencyValid(index) := true.B
    }
  }

  io.requestCompute.valid := state === requestCompute
  io.requestCompute.bits.start := !failed
  io.reportCompute.ready := state === waitCompute
  io.result.valid := state === reportResult
  io.result.bits := resultEvent

  when(state === waitDependencies && allReceived) {
    state := requestCompute
  }
  when(io.requestCompute.fire) {
    for (index <- 0 until inputCount) {
      dependencyValid(index) := false.B
    }
    when(failed) {
      resultEvent := PriorityMux(failures.zip(dependencies))
    }
    state := Mux(failed, reportResult, waitCompute)
  }
  when(io.reportCompute.fire) {
    resultEvent := io.reportCompute.bits
    state := reportResult
  }
  when(io.result.fire) {
    state := waitDependencies
  }
}

/** Owns the task table, dependency state, and automatic control routing. */
class AutoLinkFabric(params: AutoLinkParams)(implicit p: Parameters) extends ClockSinkDomain(ClockSinkParameters())(p) {
  private val endpointNodes = params.endpoints.map { endpoint =>
    endpoint.name -> BundleBridgeSource(() => new AutoEndpointAsyncLink(params))
  }.toMap
  private val resultNodes = params.table.indices.map(params.route(_).destination).distinct.map { name =>
    name -> BundleBridgeSource(() => new AsyncBundle(new AutoEvent(params), AsyncQueueParams.singleton()))
  }.toMap

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
        val requestCompute = Wire(Decoupled(new AutoComputeRequest))
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
      val tasks = params.table.indices.map(index => Module(new AutoCopyTask(params, index)))

      params.endpoints.foreach { endpoint =>
        val port = ports(endpoint.name)
        val outgoing = params.table.indices.filter(index =>
          params.route(index).source == endpoint.name)

        if (outgoing.isEmpty) {
          port.watchOutput.valid := false.B
          port.watchOutput.bits := 0.U.asTypeOf(new AutoWatch(params))
          port.reportOutput.ready := false.B
        } else {
          val copy = params.copy(outgoing.head)
          val armed = RegInit(false.B)
          val tasksReady = outgoing.map(index => tasks(index).io.reportOutput.ready).reduce(_ && _)

          port.watchOutput.valid := !armed && tasksReady
          port.watchOutput.bits.address := params.sourceAddress(outgoing.head).U
          port.watchOutput.bits.bytes := copy.bytes.U
          port.reportOutput.ready := armed && tasksReady
          outgoing.foreach { index =>
            tasks(index).io.reportOutput.valid := port.reportOutput.valid && armed && tasksReady
            tasks(index).io.reportOutput.bits := port.reportOutput.bits
          }
          when(port.watchOutput.fire) {
            armed := true.B
          }
          when(port.reportOutput.fire) {
            armed := false.B
          }
        }
      }

      params.endpoints.foreach { endpoint =>
        val port = ports(endpoint.name)
        val incoming = params.table.indices.filter(index =>
          params.route(index).destination == endpoint.name)

        if (incoming.isEmpty) {
          port.requestCopy.valid := false.B
          port.requestCopy.bits := 0.U.asTypeOf(new AutoCopyRequest(params))
          port.reportCopy.ready := false.B
          port.requestCompute.valid := false.B
          port.requestCompute.bits := 0.U.asTypeOf(new AutoComputeRequest)
          port.reportCompute.ready := false.B
        } else {
          val copyArbiter = Module(new Arbiter(new AutoCopyRequest(params), incoming.size))
          incoming.zipWithIndex.foreach { case (task, input) =>
            copyArbiter.io.in(input) <> tasks(task).io.requestCopy
          }
          val copyQueue = Module(new Queue(new AutoCopyRequest(params), params.copyDepth))
          copyQueue.io.enq <> copyArbiter.io.out
          port.requestCopy <> copyQueue.io.deq

          incoming.foreach { index =>
            tasks(index).io.reportCopy.valid := port.reportCopy.valid &&
              port.reportCopy.bits.task === index.U
            tasks(index).io.reportCopy.bits := port.reportCopy.bits
          }
          port.reportCopy.ready := incoming.map { index =>
            tasks(index).io.reportCopy.ready && port.reportCopy.bits.task === index.U
          }.reduce(_ || _)

          val join = Module(new AutoJoin(params, incoming.size))
          incoming.zipWithIndex.foreach { case (task, input) =>
            join.io.dependency(input) <> tasks(task).io.reportDependency
          }
          port.requestCompute <> join.io.requestCompute
          join.io.reportCompute <> port.reportCompute

          val result = resultNodes(endpoint.name).out.head._1
          result <> ToAsyncBundle(join.io.result, AsyncQueueParams.singleton())
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
    fabric.clockNode := pbus.fixedClockNode
    fabric
  }
}
