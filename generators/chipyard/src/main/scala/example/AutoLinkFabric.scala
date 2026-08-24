package chipyard.example

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.subsystem.{BaseSubsystem, InstantiatesHierarchicalElements, PBUS}
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams, FromAsyncBundle, ToAsyncBundle}
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import org.chipsalliance.diplomacy.lazymodule.LazyModule

case object AutoLinkKey extends Field[Option[AutoLinkParams]](None)

class WithAutoLink(params: AutoLinkParams)
    extends Config((_, _, _) => { case AutoLinkKey => Some(params) })

class AutoTask(params: AutoLinkParams, index: Int) extends Module {
  private val spec = params.transfer(index)

  val io = IO(new Bundle {
    val produced = Flipped(Decoupled(new AutoEvent(params)))
    val transfer = Decoupled(new AutoTransfer(params))
    val transferred = Flipped(Decoupled(new AutoTransferDone(params)))
    val finished = Valid(new AutoEvent(params))
    val retire = Input(Bool())
    val idle = Output(Bool())
  })

  val idle :: issue :: waitDone :: finished :: Nil = Enum(4)
  val state = RegInit(idle)
  val event = Reg(new AutoEvent(params))

  io.produced.ready := state === idle
  io.transfer.valid := state === issue
  io.transfer.bits.task := index.U
  io.transfer.bits.sourceAddress := params.sourceAddress(index).U
  io.transfer.bits.destinationOffset := spec.destinationOffset.U
  io.transfer.bits.bytes := spec.bytes.U
  io.transferred.ready := state === waitDone
  io.finished.valid := state === finished
  io.finished.bits := event
  io.idle := state === idle

  when(io.produced.fire) {
    event := io.produced.bits
    state := Mux(io.produced.bits.status === AutoLinkStatus.Success, issue, finished)
  }
  when(io.transfer.fire) {
    state := waitDone
  }
  when(io.transferred.fire) {
    event.status := io.transferred.bits.status
    event.detail := io.transferred.bits.detail
    event.data := 0.U
    state := finished
  }
  when(io.retire) {
    state := idle
  }
}

class AutoJoin(params: AutoLinkParams, inputCount: Int) extends Module {
  val io = IO(new Bundle {
    val finished = Input(Vec(inputCount, Valid(new AutoEvent(params))))
    val release = Decoupled(new AutoRelease)
    val complete = Flipped(Decoupled(new AutoEvent(params)))
    val result = Decoupled(new AutoEvent(params))
    val retire = Output(Bool())
  })

  val waitTransfers :: release :: waitComplete :: result :: Nil = Enum(4)
  val state = RegInit(waitTransfers)
  val resultEvent = Reg(new AutoEvent(params))
  val failures = io.finished.map(event => event.valid && event.bits.status =/= AutoLinkStatus.Success)
  val allFinished = io.finished.map(_.valid).reduce(_ && _)
  val failed = failures.reduce(_ || _)

  io.release.valid := state === release
  io.release.bits.start := !failed
  io.complete.ready := state === waitComplete
  io.result.valid := state === result
  io.result.bits := resultEvent
  io.retire := io.result.fire

  when(state === waitTransfers && allFinished) {
    when(failed) {
      resultEvent := PriorityMux(failures.zip(io.finished.map(_.bits)))
    }
    state := release
  }
  when(io.release.fire) {
    state := Mux(io.release.bits.start, waitComplete, result)
  }
  when(io.complete.fire) {
    resultEvent := io.complete.bits
    state := result
  }
  when(io.result.fire) {
    state := waitTransfers
  }
}

/** Owns the task table, dependency state, and automatic control routing. */
class AutoLinkFabric(params: AutoLinkParams)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  private val endpointNodes = params.endpoints.map { endpoint =>
    endpoint.name -> BundleBridgeSource(() => new AutoEndpointAsyncLink(params))
  }.toMap
  private val resultNodes = params.table.indices.map(params.route(_).destination).distinct.map { name =>
    name -> BundleBridgeSource(() =>
      new AsyncBundle(new AutoEvent(params), AsyncQueueParams.singleton()))
  }.toMap

  def endpoint(name: String): BundleBridgeSource[AutoEndpointAsyncLink] = endpointNodes(name)
  def result(name: String): BundleBridgeSource[AsyncBundle[AutoEvent]] = resultNodes(name)

  override lazy val module = new FabricImpl
  class FabricImpl extends Impl {
    case class Port(
      watch: DecoupledIO[AutoWatch],
      produced: DecoupledIO[AutoEvent],
      transfer: DecoupledIO[AutoTransfer],
      transferred: DecoupledIO[AutoTransferDone],
      release: DecoupledIO[AutoRelease],
      complete: DecoupledIO[AutoEvent])

    withClockAndReset(clock, reset) {
      val ports = params.endpoints.map { endpoint =>
        val async = endpointNodes(endpoint.name).out.head._1
        val watch = Wire(Decoupled(new AutoWatch(params)))
        val transfer = Wire(Decoupled(new AutoTransfer(params)))
        val release = Wire(Decoupled(new AutoRelease))
        async.watch <> ToAsyncBundle(watch, AsyncQueueParams.singleton())
        async.transfer <> ToAsyncBundle(transfer, AsyncQueueParams.singleton())
        async.release <> ToAsyncBundle(release, AsyncQueueParams.singleton())
        endpoint.name -> Port(
          watch,
          FromAsyncBundle(async.produced),
          transfer,
          FromAsyncBundle(async.transferred),
          release,
          FromAsyncBundle(async.complete))
      }.toMap
      val tasks = params.table.indices.map(index =>
        Module(new AutoTask(params, index)))

      params.endpoints.foreach { endpoint =>
        val port = ports(endpoint.name)
        val outgoing = params.table.indices.filter(index =>
          params.route(index).source == endpoint.name)

        if (outgoing.isEmpty) {
          port.watch.valid := false.B
          port.watch.bits := 0.U.asTypeOf(new AutoWatch(params))
          port.produced.ready := false.B
        } else {
          val task = params.transfer(outgoing.head)
          val armed = RegInit(false.B)
          val tasksIdle = outgoing.map(index => tasks(index).io.idle).reduce(_ && _)
          val tasksReady = outgoing.map(index => tasks(index).io.produced.ready).reduce(_ && _)

          port.watch.valid := !armed && tasksIdle
          port.watch.bits.address := params.sourceAddress(outgoing.head).U
          port.watch.bits.bytes := task.bytes.U
          port.produced.ready := armed && tasksReady
          outgoing.foreach { index =>
            tasks(index).io.produced.valid := port.produced.valid && armed && tasksReady
            tasks(index).io.produced.bits := port.produced.bits
          }
          when(port.watch.fire) {
            armed := true.B
          }
          when(port.produced.fire) {
            armed := false.B
          }
        }
      }

      params.endpoints.foreach { endpoint =>
        val port = ports(endpoint.name)
        val incoming = params.table.indices.filter(index =>
          params.route(index).destination == endpoint.name)

        if (incoming.isEmpty) {
          port.transfer.valid := false.B
          port.transfer.bits := 0.U.asTypeOf(new AutoTransfer(params))
          port.transferred.ready := false.B
          port.release.valid := false.B
          port.release.bits := 0.U.asTypeOf(new AutoRelease)
          port.complete.ready := false.B
        } else {
          val transfer = Module(new Arbiter(new AutoTransfer(params), incoming.size))
          incoming.zipWithIndex.foreach { case (link, input) =>
            transfer.io.in(input) <> tasks(link).io.transfer
          }
          port.transfer <> transfer.io.out

          incoming.foreach { index =>
            tasks(index).io.transferred.valid := port.transferred.valid &&
              port.transferred.bits.task === index.U
            tasks(index).io.transferred.bits := port.transferred.bits
          }
          port.transferred.ready := incoming.map { index =>
            tasks(index).io.transferred.ready && port.transferred.bits.task === index.U
          }.reduce(_ || _)

          val join = Module(new AutoJoin(params, incoming.size))
          incoming.zipWithIndex.foreach { case (link, input) =>
            join.io.finished(input) := tasks(link).io.finished
            tasks(link).io.retire := join.io.retire
          }
          port.release <> join.io.release
          join.io.complete <> port.complete

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
