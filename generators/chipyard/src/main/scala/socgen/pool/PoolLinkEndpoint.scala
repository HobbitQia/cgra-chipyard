package chipyard.socgen.pool

import chipyard.socgen.link.CanHaveAutoLink
import freechips.rocketchip.subsystem.{BaseSubsystem, InstantiatesHierarchicalElements}
import freechips.rocketchip.tile.RocketTile
import org.chipsalliance.cde.config.{Config, Field}

case class PoolLinkAttachParams(adapter: PoolLinkParams, portName: String) {
  require(adapter.auto.endpoints.exists(_.name == portName))
}

case object PoolLinkKey extends Field[Option[PoolLinkAttachParams]](None)

class WithPoolLink(params: PoolLinkAttachParams)
    extends Config((_, _, _) => { case PoolLinkKey => Some(params) })

trait CanHavePoolLink {
  this: BaseSubsystem with InstantiatesHierarchicalElements with CanHaveAutoLink =>

  val poolLink = p(PoolLinkKey).map { attach =>
    val accelerators = totalTiles.values.toSeq.flatMap {
      case tile: RocketTile =>
        tile.roccs.collect { case accelerator: PoolAccelerator => accelerator }
      case _ => Nil
    }
    require(accelerators.size == 1)
    val accelerator = accelerators.head
    require(accelerator.autoNode.nonEmpty)
    accelerator.autoNode.get := autoLink.get.endpoint(attach.portName)
    accelerator
  }
}
