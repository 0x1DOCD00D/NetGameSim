package com.lsc

import NetGraphAlgebraDefs.NetModelAlgebra.outputDirectory
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

//  @main def runLauncher(args: String*): Unit =
  def main(args: Array[String]): Unit =
    import scala.jdk.CollectionConverters.*
    logger.info("File /Users/drmark/Library/CloudStorage/OneDrive-UniversityofIllinoisChicago/Github/SeaPhish/src/main/scala/Main.scala created at time 3:04 PM")
    logger.info(s"Hostname: $hostName")
    logger.info(ipAddr.getHostAddress)
    logger.info(ipAddr.getAddress.toList.mkString(","))
    val thisCompIpAddress = NetworkInterface.getNetworkInterfaces.asScala
        .flatMap(_.getInetAddresses.asScala)
        .filterNot(_.getHostAddress == "127.0.0.1")
        .filterNot(_.getHostAddress.contains(":"))
        .map(_.getHostAddress).toList.headOption.getOrElse("INVALID IP ADDRESS")

    logger.info(s"thisCompIpAddress: $thisCompIpAddress")

    val config = ConfigFactory.load()
    logger.info("for the main entry")
    config.getConfig("NGSimulator").entrySet().forEach(e => logger.info(s"key: ${e.getKey} value: ${e.getValue.unwrapped()}"))
    logger.info("for the NetModel entry")
    config.getConfig("NGSimulator").getConfig("NetModel").entrySet().forEach(e => logger.info(s"key: ${e.getKey} value: ${e.getValue.unwrapped()}"))
    val graph: NetGraph = NetModelAlgebra()
//    val cover = Analyzer(graph)
    graph.persist(fileName = "NetGraph.ser")
    NetGraph.load(  fileName = "NetGraph.ser") match
      case Some(graph1) => if graph1 == graph then logger.info("Graphs are equal") else logger.info("Graphs are not equal")
      case None => logger.error("Failed to load the graph")
    graph.toDotVizFormat(name = s"Net Graph with ${graph.totalNodes} nodes", dir = outputDirectory, fileName = "GraphViz", outputImageFormat = Format.PNG)
