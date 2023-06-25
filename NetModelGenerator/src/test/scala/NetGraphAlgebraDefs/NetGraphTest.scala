package NetGraphAlgebraDefs

import NetGraphAlgebraDefs.NetModelAlgebra.outputDirectory
import NetGraphAlgebraDefs.{NetGraph, NetModel, NetModelAlgebra}
import Randomizer.SupplierOfRandomness
import Utilz.ConfigReader.getConfigEntry
import Utilz.CreateLogger
import org.apache.commons.io.FileUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.Logger

import java.io.File

class NetGraphTest extends AnyFlatSpec with Matchers {
  val logger: Logger = CreateLogger(this.getClass)
  behavior of "Net graph generation and (de)serialization"

  it should "create a small net graph and then serialize it" in {
    val graph: NetGraph = NetModelAlgebra()
    graph.persist(outputDirectory, "testGraph.ser")
    val res = FileUtils.getFile(outputDirectory+"testGraph.ser").exists()
    FileUtils.forceDelete(FileUtils.getFile(outputDirectory+"testGraph.ser"))
    res shouldEqual true
  }
}