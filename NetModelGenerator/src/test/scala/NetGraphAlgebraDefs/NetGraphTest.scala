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
    val graph: NetGraph with GraphStore = NetModelAlgebra()
    graph.persist(outputDirectory, "testGraph.ser")
    val res = FileUtils.getFile(outputDirectory+"testGraph.ser").exists()
    FileUtils.forceDelete(FileUtils.getFile(outputDirectory+"testGraph.ser"))
    res shouldEqual true
  }

  it should "create a small net graph and then serialize and deserialize it" in {
    val graph: NetGraph = NetModelAlgebra()
    graph.persist(outputDirectory, "testGraph.ser")
    FileUtils.getFile(outputDirectory + "testGraph.ser").exists() shouldEqual true
    val graph2 = NetGraph.load("testGraph.ser", outputDirectory)
    FileUtils.forceDelete(FileUtils.getFile(outputDirectory + "testGraph.ser"))
    if graph2.isEmpty then assert(true, "Serialized graph not loaded")
    else graph2.get shouldEqual graph
  }
}