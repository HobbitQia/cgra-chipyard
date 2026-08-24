package chipyard.example

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}
import freechips.rocketchip.subsystem.{BaseSubsystem, InstantiatesHierarchicalElements, PBUS}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams, FromAsyncBundle, ToAsyncBundle}
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import org.chipsalliance.diplomacy.lazymodule.LazyModule

case class SpmAutoLinkParams(
  spm: SharedSpmParams,
  link: SpmLinkParams,
  links: Seq[SpmLinkSpec],
  endpoints: Seq[SpmEndpointSpec],
  readBeatBytes: Int,
  writeBeatBytes: Int) {
  require(links.size == link.linkCount)
  require(endpoints.map(_.name).distinct.size == endpoints.size)
  require(endpoints.map(_.name).toSet == links.flatMap(route => Seq(route.source, route.destination)).toSet)
  endpoints.foreach { endpoint =>
    endpoint.table.validate(link)
    endpoint.table.waitFor.foreach(rule => require(links(rule.link).destination == endpoint.name))
    endpoint.table.publishTo.foreach(rule => require(links(rule.link).source == endpoint.name))
  }
}

case object SpmAutoLinkKey extends Field[Option[SpmAutoLinkParams]](None)

class WithSpmAutoLink(params: SpmAutoLinkParams)
    extends Config((_, _, _) => { case SpmAutoLinkKey => Some(params) })

class SpmFabric(params: SpmAutoLinkParams)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  private val endpointNodes = params.endpoints.map { endpoint =>
    endpoint.name -> BundleBridgeSource(() => new SpmEndpointAsyncLink(params.link))
  }.toMap
  private val resultNodes = params.links.map(_.destination).distinct.map { name =>
    name -> BundleBridgeSource(() =>
      new AsyncBundle(new SpmLinkEvent(params.link), AsyncQueueParams.singleton()))
  }.toMap

  def endpoint(name: String): BundleBridgeSource[SpmEndpointAsyncLink] = endpointNodes(name)
  def result(name: String): BundleBridgeSource[AsyncBundle[SpmLinkEvent]] = resultNodes(name)

  override lazy val module = new FabricImpl
  class FabricImpl extends Impl {
    case class Port(
      produced: DecoupledIO[SpmLinkEvent],
      deliver: DecoupledIO[SpmLinkEvent],
      done: DecoupledIO[SpmLinkEvent])

    withClockAndReset(clock, reset) {
      val ports = endpointNodes.map { case (name, node) =>
        val async = node.out.head._1
        val deliver = Wire(Decoupled(new SpmLinkEvent(params.link)))
        async.deliver <> ToAsyncBundle(deliver, AsyncQueueParams.singleton())
        name -> Port(FromAsyncBundle(async.produced), deliver, FromAsyncBundle(async.done))
      }
      val links = params.links.indices.map { index =>
        Module(new SpmAutoLink(params.link, index))
      }

      params.endpoints.foreach { endpoint =>
        val port = ports(endpoint.name)
        val outgoing = params.links.indices.filter(index => params.links(index).source == endpoint.name)
        outgoing.foreach { index =>
          links(index).io.produced.valid := port.produced.valid && port.produced.bits.link === index.U
          links(index).io.produced.bits := port.produced.bits
        }
        port.produced.ready := outgoing.map { index =>
          links(index).io.produced.ready && port.produced.bits.link === index.U
        }.reduceOption(_ || _).getOrElse(false.B)

        val incoming = params.links.indices.filter(index => params.links(index).destination == endpoint.name)
        if (incoming.isEmpty) {
          port.deliver.valid := false.B
          port.deliver.bits := 0.U.asTypeOf(new SpmLinkEvent(params.link))
        } else {
          val deliver = Module(new Arbiter(new SpmLinkEvent(params.link), incoming.size))
          incoming.zipWithIndex.foreach { case (link, input) =>
            deliver.io.in(input) <> links(link).io.deliver
          }
          port.deliver <> deliver.io.out
        }

        incoming.foreach { index =>
          links(index).io.done.valid := port.done.valid && port.done.bits.link === index.U
          links(index).io.done.bits := port.done.bits
        }
        port.done.ready := incoming.map { index =>
          links(index).io.done.ready && port.done.bits.link === index.U
        }.reduceOption(_ || _).getOrElse(false.B)
      }

      resultNodes.foreach { case (name, node) =>
        val incoming = params.links.indices.filter(index => params.links(index).destination == name)
        val result = Wire(Decoupled(new SpmLinkEvent(params.link)))
        val arbiter = Module(new Arbiter(new SpmLinkEvent(params.link), incoming.size))
        incoming.zipWithIndex.foreach { case (link, input) =>
          arbiter.io.in(input) <> links(link).io.result
        }
        result <> arbiter.io.out
        node.out.head._1 <> ToAsyncBundle(result, AsyncQueueParams.singleton())
      }
    }
  }
}

case class SpmAutoLinkSystem(spm: SharedSpm, fabric: SpmFabric)

trait CanHaveSpmAutoLink {
  this: BaseSubsystem with InstantiatesHierarchicalElements =>
  private val pbus = locateTLBusWrapper(PBUS)

  val spmAutoLink = p(SpmAutoLinkKey).map { params =>
    val spm = LazyModule(new SharedSpm(params.spm, params.readBeatBytes, params.writeBeatBytes))
    val fabric = LazyModule(new SpmFabric(params))
    spm.clockNode := pbus.fixedClockNode
    fabric.clockNode := pbus.fixedClockNode
    pbus.coupleTo("shared-spm") {
      spm.readers := TLFragmenter(params.link.beatBytes, pbus.blockBytes) := TLWidthWidget(pbus) := _
    }
    SpmAutoLinkSystem(spm, fabric)
  }
}
