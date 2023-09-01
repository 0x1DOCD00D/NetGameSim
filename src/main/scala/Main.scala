package com.lsc

import NetGraphAlgebraDefs.NetModelAlgebra.{actionType, outputDirectory}
import NetGraphAlgebraDefs.{NetGraph, NetModelAlgebra}
import NetModelAnalyzer.Analyzer
import Randomizer.SupplierOfRandomness
import Utilz.CreateLogger
import com.google.common.graph.ValueGraph

import java.util.concurrent.{ThreadLocalRandom, TimeUnit}
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import com.typesafe.config.ConfigFactory
import guru.nidi.graphviz.engine.Format
import org.slf4j.Logger

import java.net.{InetAddress, NetworkInterface, Socket}
import scala.util.{Failure, Success}

object Main:
  val logger:Logger = CreateLogger(classOf[Main.type])
  val ipAddr: InetAddress = InetAddress.getLocalHost
  val hostName: String = ipAddr.getHostName
  val hostAddress: String = ipAddr.getHostAddress

  def main(args: Array[String]): Unit =
    import scala.jdk.CollectionConverters.*
    val outGraphFileName = if args.isEmpty then "NetGraph.ser" else args(0)
    val perturbedOutGraphFileName = outGraphFileName.concat(".perturbed")
    logger.info(s"Output graph file is $outputDirectory$outGraphFileName and its perturbed counterpart is $outputDirectory$perturbedOutGraphFileName")
    logger.info(s"The netgraphsim program is run at the host $hostName with the following IP addresses:")
    logger.info(ipAddr.getHostAddress)
    NetworkInterface.getNetworkInterfaces.asScala
        .flatMap(_.getInetAddresses.asScala)
        .filterNot(_.getHostAddress == ipAddr.getHostAddress)
        .filterNot(_.getHostAddress == "127.0.0.1")
        .filterNot(_.getHostAddress.contains(":"))
        .map(_.getHostAddress).toList.foreach(a => logger.info(a))

    val existingGraph = java.io.File(s"$outputDirectory$outGraphFileName").exists
    val g = if existingGraph then
      logger.warn(s"File $outputDirectory$outGraphFileName is located, loading it up. If you want a new generated graph please delete the existing file or change the file name.")
      NetGraph.load(fileName = s"$outputDirectory$outGraphFileName") /*match
        case Some(graph) =>
          val diff = graph.compare(graph1)
          if diff == 0 then logger.info("Graph is")
          else logger.error(s"Graphs are not equal, $diff differences found")
        case None => logger.error("Failed to load the graph")*/
    else
      val config = ConfigFactory.load()
      logger.info("for the main entry")
      config.getConfig("NGSimulator").entrySet().forEach(e => logger.info(s"key: ${e.getKey} value: ${e.getValue.unwrapped()}"))
      logger.info("for the NetModel entry")
      config.getConfig("NGSimulator").getConfig("NetModel").entrySet().forEach(e => logger.info(s"key: ${e.getKey} value: ${e.getValue.unwrapped()}"))
      NetModelAlgebra()

    if g.isEmpty then logger.error("Failed to generate a graph. Exiting...")
    else
      if existingGraph then
        g.get.persist(fileName = outGraphFileName.concat(".ser"))
        logger.info(s"Generating DOT file for graph with ${g.get.totalNodes} nodes for visualization as $outputDirectory$outGraphFileName.dot")
        g.get.toDotVizFormat(name = s"Net Graph with ${g.get.totalNodes} nodes", dir = outputDirectory, fileName = outGraphFileName, outputImageFormat = Format.DOT)
        logger.info(s"A graph image file can be generated using the following command: sfdp -x -Goverlap=scale -Tpng $outputDirectory$outGraphFileName.dot > $outputDirectory$outGraphFileName.png")
      else
        logger.info("Done!")
/*

      match
        case None => logger.error("Failed to create NetModelAlgebra")
        case Some(graph) =>
          val algres = Analyzer(graph)
          algres.foreach(e => logger.info(e.mkString(", ")))
          graph.persist(fileName = outGraphFileName.concat(".ser"))
          logger.info(s"Generating DOT file for graph with ${graph.totalNodes} nodes for visualization as $outputDirectory$outGraphFileName.dot")
          graph.toDotVizFormat(name = s"Net Graph with ${graph.totalNodes} nodes", dir = outputDirectory, fileName = outGraphFileName, outputImageFormat = Format.DOT)
          logger.info(s"A graph image file can be generated using the following command: sfdp -x -Goverlap=scale -Tpng $outputDirectory$outGraphFileName.dot > $outputDirectory$outGraphFileName.png")
*/