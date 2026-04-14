package NetGraphAlgebraDefs

import NetGraphAlgebraDefs.NetGraph.load
import NetGraphAlgebraDefs.{Action, NetGraphComponent, NodeObject}
import Utilz.NGSConstants.{globalConfig, obtainConfigModule}
import com.typesafe.config.ConfigFactory
import io.circe.generic.auto.*
import io.circe.syntax.*
import org.slf4j.LoggerFactory

import java.io.*
import java.net.URL
import scala.util.{Failure, Success, Try}

// ConvertGraph is a standalone utility that loads original and perturbed .ngs graphs
// from disk and writes their nodes (as JSON) and edges (as id pairs) to separate
// plain-text files. These files can be consumed by external analysis tools.
//
// The edge output respects graph directionality: for directed graphs, edges are
// printed as "source -> target"; for undirected graphs, edges use nodeU/nodeV
// which is order-independent. Without this distinction, directed edge semantics
// would be lost when converting.
object ConvertGraph {
  private val logger = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]): Unit = {
    val config = obtainConfigModule(globalConfig, "Graph")
    val original = config.getString("fileName")
    val perturbed = s"${original}.perturbed"
    val dir = config.getString("outputPath")
    val sourceDir = config.getString("graphLocation")
    // Read the directionality setting so we use the correct Guava API for edges.
    // For directed graphs, source()/target() preserves edge direction.
    // For undirected graphs, nodeU()/nodeV() is the correct accessor.
    val graphDirectionality = config.getString("directionality")

    val originalGraph = NetGraph.load(original, sourceDir)
    val perturbedGraph = NetGraph.load(perturbed, sourceDir)

    // Helper that writes edges using the correct accessor based on directionality.
    // Using nodeU()/nodeV() on a directed graph would lose direction information;
    // using source()/target() on an undirected graph would throw an exception.
    def writeEdges(graph: NetGraph, writer: PrintWriter): Unit = {
      graph.sm.edges().forEach(edge => {
        if graphDirectionality == "undirected" then
          writer.print(s"${edge.nodeU().id} ${edge.nodeV().id}\n")
        else
          writer.print(s"${edge.source().id} ${edge.target().id}\n")
      })
    }

    (originalGraph, perturbedGraph) match {
      case (Some(originalGraph), Some(perturbedGraph)) =>
        logger.info("Starting the conversion operation.")

        val originalNodes = new PrintWriter(s"${dir}/originalNodes")
        val originalEdges = new PrintWriter(s"${dir}/originalEdges")
        val perturbedNodes = new PrintWriter(s"${dir}/perturbedNodes")
        val perturbedEdges = new PrintWriter(s"${dir}/perturbedEdges")

        originalGraph.sm.nodes().forEach(on => {
          originalNodes.print(s"${on.asJson.noSpaces}\n")
        })

        perturbedGraph.sm.nodes().forEach(on => {
          perturbedNodes.print(s"${on.asJson.noSpaces}\n")
        })

        // Use directionality-aware edge writing instead of always using nodeU()/nodeV()
        writeEdges(originalGraph, originalEdges)
        writeEdges(perturbedGraph, perturbedEdges)

        logger.info("Conversion operation completed")

        originalNodes.close()
        originalEdges.close()
        perturbedNodes.close()
        perturbedEdges.close()

      case _ =>
        logger.error(s"Failed to load one or both graphs from $sourceDir. " +
          s"Original='$original' loaded=${originalGraph.isDefined}, " +
          s"Perturbed='$perturbed' loaded=${perturbedGraph.isDefined}")
    }
  }
}