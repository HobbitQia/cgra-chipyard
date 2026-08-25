package chipyard.socgen.config

import chipyard.example.CGRAGenerated
import chipyard.socgen.generated.{AutoLinksGenerated, GemminiExternalSpmGenerated}
import chipyard.socgen.link._

object AutoLinkExample {
  val externalSpm = GemminiExternalSpmGenerated.params
  val links: Seq[AutoLinkSpec] = AutoLinksGenerated.links
  val copyBytes = 128
  val endpoints: Seq[AutoEndpointSpec] = Seq(
    AutoEndpointSpec(name = "gemmini", buffer = Some(AutoBuffer(externalSpm.baseAddress, externalSpm.sizeBytes)), localBytes = externalSpm.sizeBytes),
    AutoEndpointSpec(name = "cgra", buffer = None, localBytes = CGRAGenerated.params.dma.spmWords * CGRAGenerated.params.dataPayloadWidth / 8))
  val table: Seq[AutoCopySpec] = Seq(AutoCopySpec(route = 0, sourceOffset = externalSpm.sizeBytes - copyBytes, destinationOffset = 0, bytes = copyBytes))
  val params = AutoLinkParams(links = links, endpoints = endpoints, table = table, beatBytes = CGRAGenerated.params.dma.dramDataWidth / 8, copyDepth = 2)
}
