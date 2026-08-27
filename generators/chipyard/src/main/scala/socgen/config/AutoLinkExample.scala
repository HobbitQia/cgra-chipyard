package chipyard.socgen.config

import chipyard.example.CGRAGenerated
import chipyard.socgen.generated.{AutoLinksGenerated, CGRASpmWindowGenerated, GemminiExternalSpmGenerated}
import chipyard.socgen.link._

private object AutoLinkRoutes {
  def find(source: String, destination: String): AutoLinkSpec =
    AutoLinksGenerated.links.find(link =>
      link.source == source && link.destination == destination).getOrElse(
      throw new IllegalArgumentException(s"missing AutoLink route $source -> $destination"))
}

object AutoLinkExample {
  val externalSpm = GemminiExternalSpmGenerated.params
  val links: Seq[AutoLinkSpec] = Seq(AutoLinkRoutes.find("gemmini", "cgra"))
  val copyBytes = 128
  val endpoints: Seq[AutoEndpointSpec] = Seq(
    AutoEndpointSpec(name = "gemmini", buffer = Some(AutoBuffer(externalSpm.baseAddress, externalSpm.sizeBytes)), localBytes = externalSpm.sizeBytes),
    AutoEndpointSpec(name = "cgra", buffer = None, localBytes = CGRAGenerated.params.dma.spmWords * CGRAGenerated.params.dataPayloadWidth / 8))
  val table: Seq[AutoCopySpec] = Seq(AutoCopySpec(route = 0, sourceOffset = externalSpm.sizeBytes - copyBytes, destinationOffset = 0, bytes = copyBytes))
  val params = AutoLinkParams(links = links, endpoints = endpoints, table = table, beatBytes = CGRAGenerated.params.dma.dramDataWidth / 8, copyDepth = 2)
}

object AutoLinkAesExample {
  val externalSpm = GemminiExternalSpmGenerated.params
  val copyBytes = 128
  val links: Seq[AutoLinkSpec] = Seq(
    AutoLinkRoutes.find("gemmini", "cgra"),
    AutoLinkRoutes.find("cgra", "aes"))
  private def route(source: String, destination: String): Int =
    links.indexWhere(link => link.source == source && link.destination == destination)
  val endpoints: Seq[AutoEndpointSpec] = Seq(
    AutoEndpointSpec(name = "gemmini", buffer = Some(AutoBuffer(externalSpm.baseAddress, externalSpm.sizeBytes)), localBytes = externalSpm.sizeBytes),
    AutoEndpointSpec(name = "cgra", buffer = Some(AutoBuffer(CGRASpmWindowGenerated.baseAddress, CGRASpmWindowGenerated.sizeBytes)), localBytes = CGRASpmWindowGenerated.sizeBytes),
    AutoEndpointSpec(name = "aes", buffer = None, localBytes = copyBytes))
  val table: Seq[AutoCopySpec] = Seq(
    AutoCopySpec(route = route("gemmini", "cgra"), sourceOffset = externalSpm.sizeBytes - copyBytes, destinationOffset = 0, bytes = copyBytes),
    AutoCopySpec(route = route("cgra", "aes"), sourceOffset = 0, destinationOffset = 0, bytes = copyBytes))
  val params = AutoLinkParams(links = links, endpoints = endpoints, table = table, beatBytes = CGRAGenerated.params.dma.dramDataWidth / 8, copyDepth = 2)
}
