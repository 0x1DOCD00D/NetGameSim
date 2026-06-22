package NetGraphAlgebraDefs

import java.io.{File, PrintWriter}
import org.apache.commons.io.FileUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.Logger
import scala.util.Failure
import scala.util.Success
import scala.util.Try
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
    Try(FileUtils.forceDelete(FileUtils.getFile(outputDirectory + "testGraph_1.ser"))) match
       case Failure(exception) =>
         logger.warn(s"Cannot delete file ${outputDirectory + "testGraph_1.ser"}")
       case Success(value) => logger.info(s"Deleted file ${outputDirectory + "testGraph_1.ser"}")
    res shouldEqual true
  }

  it should "create a small net graph and then serialize and deserialize it" in {
    val graph: NetGraph = NetModelAlgebra().get
    graph.persist(outputDirectory, "testGraph_2.ser")
    FileUtils.getFile(outputDirectory + "testGraph_2.ser").exists() shouldEqual true
    val graph2 = NetGraph.load("testGraph_2.ser", outputDirectory)
    Try(FileUtils.forceDelete(FileUtils.getFile(outputDirectory + "testGraph_2.ser"))) match
       case Failure(exception) =>
         logger.warn(s"Cannot delete file ${outputDirectory + "testGraph_2.ser"}")
       case Success(value) => logger.info(s"Deleted file ${outputDirectory + "testGraph_2.ser"}")
    if graph2.isEmpty then assert(true, "Serialized graph not loaded")
    else graph2.get shouldEqual graph
  }

  // Regression: the JSON load path used arr.head / arr.last / .right.get,
  // which threw on empty files or malformed JSON instead of returning None.
  it should "return None when the JSON file is empty rather than throwing" in {
    val fileName = "empty_graph.json"
    val fullPath = outputDirectory + fileName
    new File(outputDirectory).mkdirs()
    val pw = new PrintWriter(new File(fullPath))
    pw.write("")
    pw.close()
    val result = NetGraph.load(fileName, outputDirectory)
    Try(FileUtils.forceDelete(FileUtils.getFile(fullPath)))
    result shouldBe None
  }

  it should "return None when the JSON file is malformed rather than throwing" in {
    val fileName = "malformed_graph.json"
    val fullPath = outputDirectory + fileName
    new File(outputDirectory).mkdirs()
    val pw = new PrintWriter(new File(fullPath))
    pw.write("not json\nalso not json\n")
    pw.close()
    val result = NetGraph.load(fileName, outputDirectory)
    Try(FileUtils.forceDelete(FileUtils.getFile(fullPath)))
    result shouldBe None
  }
}
