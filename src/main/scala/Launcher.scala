package com.lsc

import Randomizer.SupplierOfRandomness
import Utilz.CreateLogger

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import com.typesafe.config.ConfigFactory
import org.slf4j.Logger

import java.net.{InetAddress, NetworkInterface, Socket}

object Launcher:
  val logger:Logger = CreateLogger(classOf[Launcher.type])
  val ipAddr: InetAddress = InetAddress.getLocalHost
  val hostName: String = ipAddr.getHostName
  val hostAddress: String = ipAddr.getHostAddress

  @main def runLauncher(args: String*): Unit =
    import scala.jdk.CollectionConverters.*
    logger.info("File /Users/drmark/Library/CloudStorage/OneDrive-UniversityofIllinoisChicago/Github/SeaPhish/src/main/scala/Launcher.scala created at time 3:04 PM")
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
    val currentDirectory = new java.io.File(".").getCanonicalPath + "/"
    logger.info(s"currentDirectory: $currentDirectory")



