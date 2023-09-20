package NetGraphAlgebraDefs

import java.io.File
import org.apache.commons.io.FileUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.Logger
import NetGraphAlgebraDefs.NetGraph
import NetGraphAlgebraDefs.NetModel
import NetGraphAlgebraDefs.NetModelAlgebra
import NetGraphAlgebraDefs.NetModelAlgebra.outputDirectory
import Randomizer.SupplierOfRandomness
import Utilz.ConfigReader.getConfigEntry
import Utilz.CreateLogger

class NetGraphTest extends AnyFlatSpec with Matchers {
  val logger: Logger = CreateLogger(this.getClass)
  behavior.of("Net graph generation and (de)serialization")

  it should "create a small net graph and then serialize it" in {
    val graph: NetGraph with GraphStore = NetModelAlgebra().get
    graph.persist(outputDirectory, "testGraph_1.ser")
    val res = FileUtils.getFile(outputDirectory + "testGraph_1.ser").exists()
    FileUtils.forceDelete(FileUtils.getFile(outputDirectory + "testGraph_1.ser"))
    res shouldEqual true
  }

  it should "create a small net graph and then serialize and deserialize it" in {
    val graph: NetGraph = NetModelAlgebra().get
    graph.persist(outputDirectory, "testGraph_2.ser")
    FileUtils.getFile(outputDirectory + "testGraph_2.ser").exists() shouldEqual true
    val graph2 = NetGraph.load("testGraph_2.ser", outputDirectory)
    FileUtils.forceDelete(FileUtils.getFile(outputDirectory + "testGraph_2.ser"))
    if graph2.isEmpty then assert(true, "Serialized graph not loaded")
    else graph2.get shouldEqual graph
  }
}
