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

object ConvertGraph {
  private val logger = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]): Unit = {
    val config = obtainConfigModule(globalConfig, "Graph")
    val original = config.getString("fileName") //One of the configuration parameters is the name of the file
    val perturbed = s"${original}.perturbed"
    val dir = config.getString("outputPath") //Where to save the files
    val sourceDir = config.getString("graphLocation") //Where the graphs are located

    val originalGraph = NetGraph.load(original, sourceDir)
    val perturbedGraph = NetGraph.load(perturbed, sourceDir)

    val logger = LoggerFactory.getLogger(getClass)

    val originalNodes: PrintWriter = {
      new PrintWriter(s"${dir}/originalNodes")
    }
    val originalEdges: PrintWriter = {
      new PrintWriter(s"${dir}/originalEdges")
    }
    val perturbedNodes: PrintWriter = {
      new PrintWriter(s"${dir}/perturbedNodes")
    }
    val perturbedEdges: PrintWriter = {
      new PrintWriter(s"${dir}/perturbedEdges")
    }

    (originalGraph, perturbedGraph) match {
      case (Some(originalGraph), Some(perturbedGraph)) =>

        logger.info("Starting the conversion operation.")

        originalGraph.sm.nodes().forEach(on => {
          originalNodes.print(s"${on.asJson.noSpaces}\n")
        })

        perturbedGraph.sm.nodes().forEach(on => {
          perturbedNodes.print(s"${on.asJson.noSpaces}\n")
        })

        originalGraph.sm.edges().forEach(oe => {
          originalEdges.print(s"${oe.nodeU().id} ${oe.nodeV().id}\n")
        })

        perturbedGraph.sm.edges().forEach(pe => {
          perturbedEdges.print(s"${pe.nodeU().id} ${pe.nodeV().id}\n")
        })

        logger.info("Conversion operation completed")

        originalNodes.close()
        originalEdges.close()
        perturbedNodes.close()
        perturbedEdges.close()
    }
  }
}