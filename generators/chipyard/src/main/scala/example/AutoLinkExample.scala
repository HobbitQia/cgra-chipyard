package chipyard.example

object AutoLinkExample {
  val links: Seq[SpmLinkSpec] = SpmLinksGenerated.links
  val transfer = SpmCommunicationRule(link = 0, slot = 0, bytes = 128)
  val source = SpmEndpointSpec(
    name = "gemmini",
    table = SpmCommunicationTable(waitFor = Nil, publishTo = Seq(transfer)))
  val destination = SpmEndpointSpec(
    name = "cgra",
    table = SpmCommunicationTable(waitFor = Seq(transfer), publishTo = Nil))
  val endpoints: Seq[SpmEndpointSpec] = Seq(source, destination)
}
