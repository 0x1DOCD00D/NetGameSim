package com.lsc

import scala.io.Source
import scala.util.matching.Regex
import java.io.{BufferedWriter, File, PrintWriter, FileWriter, InputStream, FileInputStream}
import org.yaml.snakeyaml.Yaml
import java.util.{HashMap, Map as JavaMap}
import scala.collection.mutable
import scala.collection.mutable.Set
import scala.jdk.CollectionConverters._
import Utilz.{CreateLogger, NGSConstants}
import org.slf4j.Logger
import org.slf4j.LoggerFactory

object DotFileParser {
  val logger: Logger = LoggerFactory.getLogger(classOf[DotFileParser.type])
  def parseFiles(originalDotFilePath: String, perturbedDotFilePath: String, filename: String): Unit = {
    type NodeId = String
    type Edge1 = (NodeId, NodeId)

    val addedNodesLogMutable = mutable.Set[Int]()
    val modifiedNodesLogMutable = mutable.Set[Int]()
    val removedNodesLogMutable = mutable.Set[Int]()
    val addedEdgesLog = mutable.Set[Edge1]()
    val removedEdgesLog = mutable.Set[Edge1]()
    val modifiedEdgesLog = mutable.Set[Edge1]()

    //val filename = "D:\\NetGameSim-DNGSimulator.ngs.yaml"
    val source = Source.fromFile(filename)
    val lines = source.getLines().toList
    source.close()
    // Replace tabs with spaces
    val fixedLines = lines.map(_.replace("\t", "  ")) // Assuming you want to replace each tab with two spaces

    val correctedYaml = fixedLines.mkString("\n")

    // Read the golden set from the yaml
    val yaml = new Yaml()
    val parsedYaml = yaml.load(correctedYaml).asInstanceOf[java.util.Map[String, java.util.Map[String, Any]]]

    val nodesData = parsedYaml.get("Nodes").asScala
    addedNodesLogMutable ++= nodesData("Added").asInstanceOf[java.util.Map[Integer, Integer]].keySet.asScala.map(_.toInt)
    modifiedNodesLogMutable ++= nodesData("Modified").asInstanceOf[java.util.List[Integer]].asScala.map(_.toInt)
    removedNodesLogMutable ++= nodesData("Removed").asInstanceOf[java.util.List[Integer]].asScala.map(_.toInt)


    val edgesData = parsedYaml.get("Edges").asScala
    addedEdgesLog ++= edgesData("Added").asInstanceOf[java.util.Map[Integer, Integer]].asScala.map { case (k, v) => (k.toString, v.toString) }
    modifiedEdgesLog ++= edgesData("Modified").asInstanceOf[java.util.Map[Integer, Integer]].asScala.map { case (k, v) => (k.toString, v.toString) }
    removedEdgesLog ++= edgesData("Removed").asInstanceOf[java.util.Map[Integer, Integer]].asScala.map { case (k, v) => (k.toString, v.toString) }

    val addedNodesLog = addedNodesLogMutable.toSet
    val modifiedNodesLog = modifiedNodesLogMutable.toSet
    val removedNodesLog = removedNodesLogMutable.toSet


    /*val originalDotFilePath = "D:\\NetGameSim-DNGSimulator.ngs.dot"
    val perturbedDotFilePath = "D:\\NetGameSim-DNGSimulator.ngs.perturbed.dot"*/


    // Load the graph from the dot file
    val (originalNodeMap, originalEdges) = loadGraph(originalDotFilePath)
    val (perturbedNodeMap, perturbedEdges) = loadGraph(perturbedDotFilePath)
    logger.info(s"originalNodeMap: $originalNodeMap")
    logger.info(s"originalEdges: $originalEdges")
    val similarityThreshold = 0.8
    // Calculate edge similarities
    val edgeSimilarities = computeEdgeSimilarities(originalEdges, perturbedEdges)
    val sharedEdges = computeSharedEdges(originalEdges, perturbedEdges, similarityThreshold)

    // Save the edgeSimilarities to the CSV
    writeSharedEdgesToCSV(sharedEdges, "shared_edges.csv")

    // implement the adjacement map for the second neighbor
    def getSecondOrderNeighbors(adjMap: Map[String, mutable.Set[String]], node: String): mutable.Set[String] = {
      val firstOrder = adjMap.getOrElse(node, mutable.Set.empty)
      val secondOrder = firstOrder.flatMap(neighbor => adjMap.getOrElse(neighbor, mutable.Set.empty))
      firstOrder ++ secondOrder
    }

    def buildAdjacencyMap(edges: List[(String, String, Map[String, String])]): Map[String, mutable.Set[String]] = {
      edges.foldLeft(Map.empty[String, mutable.Set[String]]) {
        case (map, (src, dst, _)) =>
          val updatedSrc = map.updated(src, map.getOrElse(src, mutable.Set.empty) += dst)
          updatedSrc.updated(dst, updatedSrc.getOrElse(dst, mutable.Set.empty) += src)
      }
    }

    //Intermidiate result generation
    def saveAdjacencyMapToFile(adjMap: Map[String, mutable.Set[String]], filename: String): Unit = {
      val writer = new PrintWriter(new File(filename))
      try {
        adjMap.foreach { case (node, neighbors) =>
          writer.println(s"$node\t${neighbors.mkString(",")}")
        }
      } finally {
        writer.close()
      }
    }

    val originalAdjacencyMap = buildAdjacencyMap(originalEdges)
    val perturbedAdjacencyMap = buildAdjacencyMap(perturbedEdges)
    /*
    saveAdjacencyMapToFile(originalAdjacencyMap, "D:\\originalAdjacencyMap.txt")
    saveAdjacencyMapToFile(perturbedAdjacencyMap, "D:\\perturbedAdjacencyMap.txt")*/

    val addedNodes = perturbedAdjacencyMap.keySet.diff(originalAdjacencyMap.keySet)
    logger.info(s"Added Nodes: $addedNodes")
    val deletedNodes = originalAdjacencyMap.keySet.diff(perturbedAdjacencyMap.keySet)
    logger.info(s"Deleted Nodes: $deletedNodes")

    // Find the modified nodes by comparing the adjacency maps
    val modifiedNodes = (originalAdjacencyMap.keySet ++ perturbedAdjacencyMap.keySet).filter { node =>
      val originalNeighbors = getSecondOrderNeighbors(originalAdjacencyMap, node)
      val perturbedNeighbors = getSecondOrderNeighbors(perturbedAdjacencyMap, node)
      originalNeighbors != perturbedNeighbors
    }
    logger.info(s"Modified Nodes: $modifiedNodes")

    //Compute the result.
    val modifiedNodes_tp = modifiedNodes.intersect(modifiedNodesLog.map(_.toString).toSet).size
    val modifiedNodes_fp = modifiedNodesLog.map(_.toString).toSet.diff(modifiedNodes).size
    val modifiedNodes_fn = modifiedNodes.diff(modifiedNodesLog.map(_.toString).toSet).size
    val modifiedNodes_precision = modifiedNodes_tp.toDouble / (modifiedNodes_tp + modifiedNodes_fp)
    val modifiedNodes_recall = modifiedNodes_tp.toDouble / (modifiedNodes_tp + modifiedNodes_fn)

    val deletedNodes_tp = deletedNodes.intersect(removedNodesLog.map(_.toString).toSet).size
    val deletedNodes_fp = removedNodesLog.map(_.toString).toSet.diff(deletedNodes).size
    val deletedNodes_fn = deletedNodes.diff(removedNodesLog.map(_.toString).toSet).size
    val deletedNodes_precision = deletedNodes_tp.toDouble / (deletedNodes_tp + deletedNodes_fp)
    val deletedNodes_recall = deletedNodes_tp.toDouble / (deletedNodes_tp + deletedNodes_fn)

    logger.info(s"ModifiedNodes Precision: $modifiedNodes_precision")
    logger.info(s"ModifiedNodes Recall: $modifiedNodes_recall")
    logger.info(s"DeletedNodes Precision: $deletedNodes_precision")
    logger.info(s"DeletedNodes Recall: $deletedNodes_recall")

    type NodeAttributes = Map[String, String]
    type Edge = (NodeId, NodeId, Map[String, String])
    type NodeMap = Map[NodeId, NodeAttributes]

    case class Graph(nodes: NodeMap, edges: mutable.Set[Edge])

    val originalGraph = Graph(originalNodeMap, mutable.Set(originalEdges: _*))
    val perturbedGraph = Graph(perturbedNodeMap, mutable.Set(perturbedEdges: _*))
    //Build LCS for edge calculation
    def hashGraph(graph: Graph, threshold: Int): mutable.Set[Int] = {
      val hashedEdges = graph.edges.map(edge => {
        val hash = edge.hashCode()
        (hash / threshold) * threshold
      })
      mutable.Set(hashedEdges.toSeq: _*)
    }

    val threshold = 1000
    val originalHashes = hashGraph(originalGraph, threshold)
    val perturbedHashes = hashGraph(perturbedGraph, threshold)


    val addedEdges = perturbedGraph.edges.diff(originalGraph.edges)
    val removedEdges = originalGraph.edges.diff(perturbedGraph.edges)

    val commonEdges = originalGraph.edges.intersect(perturbedGraph.edges)
    val modifiedEdges = originalGraph.edges.flatMap { originalEdge =>
      perturbedGraph.edges.collect {
        case perturbedEdge if originalEdge._1 == perturbedEdge._1 && originalEdge._2 == perturbedEdge._2 && originalEdge._3 != perturbedEdge._3 =>
          perturbedEdge
      }
    }.toSet

    logger.info(s"addedEdges: $addedEdges")
    logger.info(s"removedEdges: $removedEdges")
    logger.info(s"modifiedEdges: $modifiedEdges")

    val addedEdgesSet: scala.collection.mutable.Set[Edge1] = addedEdges.map {
      case (node1, node2, _) => (node1, node2)
    }

    val removedEdgesSet: scala.collection.mutable.Set[Edge1] = removedEdges.map {
      case (node1, node2, _) => (node1, node2)
    }


    //Compute the result
    val addedTp = addedEdgesSet.intersect(addedEdgesLog).size
    val addedFp = addedEdgesLog.diff(addedEdgesSet).size
    val addedFn = addedEdgesSet.diff(addedEdgesLog).size
    val addedPrecision = addedTp.toDouble / (addedTp + addedFp)
    val addedRecall = addedTp.toDouble / (addedTp + addedFn)
    logger.info(s"addedEdgesPrecision: $addedPrecision")
    logger.info(s"addedEdgesRecall: $addedRecall")

    val removedTp = removedEdgesSet.intersect(removedEdgesLog).size
    val removedFp = removedEdgesLog.diff(removedEdgesSet).size
    val removedFn = removedEdgesSet.diff(removedEdgesLog).size
    val removedPrecision = removedTp.toDouble / (removedTp + removedFp)
    val removedRecall = removedTp.toDouble / (removedTp + removedFn)
    logger.info(s"removedEdgesPrecision: $addedPrecision")
    logger.info(s"removedEdgesRecall: $addedRecall")

    // Transform modifiedEdges to only include the NodeId pairs
    val modifiedEdgesTransformed = modifiedEdges.map(edge => (edge._1, edge._2))

    val modifiedTp = modifiedEdgesTransformed.intersect(modifiedEdgesLog).size
    val modifiedFp = modifiedEdgesLog.diff(modifiedEdgesTransformed).size
    val modifiedFn = modifiedEdgesTransformed.diff(modifiedEdgesLog).size

    val modifiedPrecision = if (modifiedTp + modifiedFp == 0) 1.0 else modifiedTp.toDouble / (modifiedTp + modifiedFp)
    val modifiedRecall = if (modifiedTp + modifiedFn == 0) 1.0 else modifiedTp.toDouble / (modifiedTp + modifiedFn)

    logger.info(s"modifiedEdgesPrecision: $modifiedPrecision")
    logger.info(s"modifiedEdgesRecall: $modifiedRecall")
    
  }

  //Dot file loading
  def loadGraph(dotFilePath: String): (Map[String, Map[String, String]], List[(String, String, Map[String, String])]) = {
    var source: Source = null
    try {
      source = Source.fromFile(dotFilePath)
      val dotFileLines = source.getLines().toList.tail // skip the first line

      var nodeAttributes: Map[String, Map[String, String]] = Map()
      var edges: List[(String, String, Map[String, String])] = Nil

      for (line <- dotFileLines) {
        if (line.trim.startsWith("\"") && line.contains("[") && !line.contains("->")) {
          val parts = line.split("\\[", 2).map(_.trim) // handle nodes with HTML-like labels
          val nodeId = parts(0).stripPrefix("\"").stripSuffix("\"")
          val attributes = parts(1).stripSuffix("]").split(",").map { attribute =>
            val keyValue = attribute.split("=")
            keyValue(0).trim -> keyValue(1).trim.stripPrefix("\"").stripSuffix("\"")
          }.toMap

          nodeAttributes += (nodeId -> attributes)
        } else if (line.trim.contains("->")) {
          val parts = line.split("->").map(_.trim)
          val sourceNode = parts(0).stripPrefix("\"").stripSuffix("\"")
          val targetNodeParts = parts(1).split("\\[")
          val targetNode = targetNodeParts(0).trim.stripPrefix("\"").stripSuffix("\"")
          val attributes = if (targetNodeParts.length > 1) {
            targetNodeParts(1).stripPrefix("[").stripSuffix("]").split(",").map { attribute =>
              val keyValue = attribute.split("=")
              keyValue(0).trim -> keyValue(1).trim.stripPrefix("\"").stripSuffix("\"")
            }.toMap
          } else {
            Map.empty[String, String]
          }

          edges ::= (sourceNode, targetNode, attributes)

          // Add nodes from edges into nodeAttributes if they don't already exist
          if (!nodeAttributes.contains(sourceNode)) {
            nodeAttributes += (sourceNode -> Map())
          }
          if (!nodeAttributes.contains(targetNode)) {
            nodeAttributes += (targetNode -> Map())
          }
        }
      }

      (nodeAttributes, edges)
    } finally {
      if (source != null) source.close()
    }
  }

  // Compute EdgeSimilarities
  def computeEdgeSimilarities(originalEdges: List[(String, String, Map[String, String])],
                              perturbedEdges: List[(String, String, Map[String, String])]): List[(String, String, Double)] = {
    for {
      (src, dest, attrs) <- originalEdges
      (pSrc, pDest, pAttrs) <- perturbedEdges if src == pSrc && dest == pDest
      similarity = computeSimilarity(attrs, pAttrs)
    } yield (src, dest, similarity)
  }

  def writeSharedEdgesToCSV(edges: List[(String, String, Double)], filePath: String): Unit = {
    val file = new File(filePath)
    val bw = new BufferedWriter(new FileWriter(file))

    // Write the headers
    bw.write("source_node,target_node,similarity\n")

    // Write the data
    for ((src, dest, similarity) <- edges) {
      bw.write(s"$src,$dest,$similarity\n")
    }

    bw.close()
  }

  def computeSimilarity(map1: Map[String, String], map2: Map[String, String]): Double = {
    val commonKeys = map1.keys.toSet.intersect(map2.keys.toSet)
    val unionKeys = map1.keys.toSet.union(map2.keys.toSet)

    if (unionKeys.isEmpty) 1.0
    else commonKeys.size.toDouble / unionKeys.size.toDouble
  }

  def computeSharedEdges(originalEdges: List[(String, String, Map[String, String])],
                         perturbedEdges: List[(String, String, Map[String, String])],
                         similarityThreshold: Double):
  List[(String, String, Double)] = {

    for {
      (src, dest, attrs) <- originalEdges
      (pSrc, pDest, pAttrs) <- perturbedEdges if src == pSrc && dest == pDest
      similarity = computeSimilarity(attrs, pAttrs) if similarity >= similarityThreshold
    } yield (src, dest, similarity)
  }
}