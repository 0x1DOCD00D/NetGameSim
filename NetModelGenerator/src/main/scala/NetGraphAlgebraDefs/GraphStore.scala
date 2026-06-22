package NetGraphAlgebraDefs

import NetGraphAlgebraDefs.NetModelAlgebra.{logger, outputDirectory}
import com.typesafe.config.ConfigFactory
import Utilz.NGSConstants
import guru.nidi.graphviz.attribute.Attributes.attr
import guru.nidi.graphviz.attribute.LinkAttr.weight
import guru.nidi.graphviz.attribute.{Color, Font, Label, LinkAttr, Rank, Style}

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters._
import scala.util.{Failure, Success, Try, Using}
import guru.nidi.graphviz.engine.{EngineResult, Format, Graphviz, GraphvizCmdLineEngine, GraphvizJdkEngine, GraphvizServerEngine}
import guru.nidi.graphviz.model.Factory.{graph, linkAttrs, node, to}
import guru.nidi.graphviz.model.{Graph, Node}

import java.io.{File, FileWriter}
import java.util.concurrent.TimeUnit
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.syntax.*

trait GraphStore:
  self: NetGraph =>

  // Persists the graph to disk in the format specified by the OutputGraphRepresentation.contentType
  // configuration parameter. Supported formats:
  //   - "json"  : Circe JSON with nodes on the first line and edges on the second
  //   - "yaml"  : Human-readable YAML listing all nodes and edges with their properties
  //   - "ngs"   : Java binary serialization (default)
  // The graph directionality (directed/undirected) is read from the Graph.directionality
  // config and controls which Guava API is used to retrieve edge values.
  def persist(dir: String = outputDirectory, fileName: String = NGSConstants.OUTPUTFILENAME()): Unit =
    val config = ConfigFactory.load()
    val outputGraphRepresentation = config.getConfig("NGSimulator").getConfig("OutputGraphRepresentation").getString("contentType")
    val graphDirectionality = config.getConfig("NGSimulator").getConfig("Graph").getString("directionality")
    if (outputGraphRepresentation == "json") then
        // JSON export. Previously the FileWriter was constructed inside a Try
        // block and closed only on the happy path, so any exception thrown
        // by write() (e.g. disk full, I/O error) would leak the file handle
        // because close() was never reached. Wrapping in Using guarantees
        // the handle is released in every code path via try-with-resources.
        Using(new FileWriter(s"$dir$fileName")) { file =>
          val nodesInGraph: String = sm.nodes().asScala.asJson.noSpaces
          val edgesInGraph: String = sm.edges().asScala.toList.map { edge =>
            val edgeValue = (if (graphDirectionality == "undirected") then
              sm.edgeValue(edge.nodeU(), edge.nodeV())
            else
            sm.edgeValue(edge.source(), edge.target())
            ).toScala

            edgeValue match {
              case Some(value) => value
              case None => throw new IllegalArgumentException("Edge without value")
            }
          }.asJson.noSpaces

          file.write(nodesInGraph + "\n" + edgesInGraph)
        }.map(_ => NetGraph.logger.info(s"Successfully persisted the graph in json to $dir$fileName"))
          .recover { case e => NetGraph.logger.error(s"Failed to persist the graph in json to $dir$fileName : ", e) }
    else if (outputGraphRepresentation == "yaml") then
        // YAML export: writes a human-readable representation of the graph that is easier
        // to inspect visually than binary .ngs or dense JSON. The format lists every node
        // with its properties followed by every edge with its action attributes. This uses
        // the same directionality-aware edge value retrieval as the JSON and binary formats.
        // FileWriter is wrapped in Using so the file handle is released even if write()
        // throws mid-stream (same leak that the JSON branch suffered from previously).
        Using(new FileWriter(s"$dir$fileName")) { file =>
          val sb = new StringBuilder
          sb.append("# NetGameSim graph exported in YAML format\n")
          sb.append(s"directionality: $graphDirectionality\n")
          sb.append(s"nodeCount: ${sm.nodes().size()}\n")
          sb.append(s"edgeCount: ${sm.edges().size()}\n")
          sb.append("nodes:\n")
          sm.nodes().asScala.toList.sortBy(_.id).foreach { node =>
            sb.append(s"  - id: ${node.id}\n")
            sb.append(s"    children: ${node.children}\n")
            sb.append(s"    props: ${node.props}\n")
            sb.append(s"    currentDepth: ${node.currentDepth}\n")
            sb.append(s"    propValueRange: ${node.propValueRange}\n")
            sb.append(s"    maxDepth: ${node.maxDepth}\n")
            sb.append(s"    maxBranchingFactor: ${node.maxBranchingFactor}\n")
            sb.append(s"    maxProperties: ${node.maxProperties}\n")
            sb.append(s"    storedValue: ${node.storedValue}\n")
            sb.append(s"    valuableData: ${node.valuableData}\n")
          }
          sb.append("edges:\n")
          sm.edges().asScala.toList.foreach { edge =>
            val edgeValue = (if (graphDirectionality == "undirected") then
              sm.edgeValue(edge.nodeU(), edge.nodeV())
            else
              sm.edgeValue(edge.source(), edge.target())
            ).toScala
            edgeValue match {
              case Some(action) =>
                sb.append(s"  - actionType: ${action.actionType}\n")
                sb.append(s"    fromId: ${action.fromId}\n")
                sb.append(s"    toId: ${action.toId}\n")
                sb.append(s"    cost: ${action.cost}\n")
                sb.append(s"    resultingValue: ${action.resultingValue.getOrElse("null")}\n")
              case None =>
                NetGraph.logger.warn("Encountered edge without a value during YAML export, skipping")
            }
          }
          file.write(sb.toString)
        }.map(_ => NetGraph.logger.info(s"Successfully persisted the graph in yaml to $dir$fileName"))
          .recover { case e => NetGraph.logger.error(s"Failed to persist the graph in yaml to $dir$fileName : ", e) }
    else {
      import java.io._
      import java.util.Base64
      import java.nio.charset.StandardCharsets.UTF_8

      val fullGraphAsList: List[NetGraphComponent] = sm.nodes().asScala.toList ::: sm.edges().asScala.toList.map { edge =>
        (if graphDirectionality == "undirected" then
          sm.edgeValue(edge.nodeU(), edge.nodeV())
        else
          sm.edgeValue(edge.source(), edge.target())
          ).get
      }.asInstanceOf[List[NetGraphComponent]]
      // Binary .ngs export. The previous .map chain had two leak paths:
      //   (a) if `new ObjectOutputStream(fos)` threw, the outer
      //       FileOutputStream was never closed.
      //   (b) if `writeObject` threw before the explicit `oos.close()` line,
      //       neither stream was closed.
      // Nesting two Using blocks guarantees both streams are released in
      // every exit path. FileOutputStream.close() is idempotent, so the
      // redundant inner close (via ObjectOutputStream) followed by the
      // outer Using's close is safe.
      Using(new FileOutputStream(s"$dir$fileName", false)) { fos =>
        Using(new ObjectOutputStream(fos)) { oos =>
          oos.writeObject(fullGraphAsList)
          oos.flush()
        }.get
      }.map(_ => NetGraph.logger.info(s"Successfully persisted the graph to $dir$fileName"))
        .recover { case e => NetGraph.logger.error(s"Failed to persist the graph to $dir$fileName : ", e) }
    }

  //  Use the following graphviz command to render the graph to an image:
  //  sfdp -x -Goverlap=scale -Tpng graph.dot > graph.png
  def toDotVizFormat(name: String, dir: String = outputDirectory, fileName: String, outputImageFormat: Format = Format.DOT): Unit =
    val config = ConfigFactory.load()
    val graphDirectionality = config.getConfig("NGSimulator").getConfig("Graph").getString("directionality")
    val nodes: List[NodeObject] = initState :: sm.nodes().asScala.toList
    if nodes.count(_.id == 0) < 1 then
      logger.error("The graph does not contain a start node with id 0")
    else if (graphDirectionality == "directed") then
      val edges: List[Action] = sm.edges().asScala.toList.map { edge =>
        sm.edgeValue(edge.source(), edge.target()).get
      }.sortBy(_.fromNode.id)
      val nodesMap = nodes.foldLeft(Map[Int, Node]()) { case (acc, nd) =>
        acc + (nd.id -> (
          if nd.id == 0 then
            node(nd.id.toString).`with`(Color.RED).`with`(Label.markdown("**Init**"), Color.rgb("1020d0").font())
          else
            node(nd.id.toString)))
      }
      val linkedGraph = edges.foldLeft(nodesMap) { case (acc, edge) =>
        if acc.contains(edge.fromNode.id) && acc.contains(edge.toNode.id) then
          acc + (edge.fromNode.id -> acc(edge.fromNode.id).link(to(acc(edge.toNode.id)).`with`(weight(if (edge.cost*10).floor < 1 then 1 else (edge.cost*10).floor))))
        else
          logger.error(s"Edge $edge is not valid because it contains a node that is not in the graph")
          acc
      }
      val g = graph(name).directed().`with`(linkedGraph.values.toList: _*).
        linkAttr().`with` ("class", "link-class").`with`(linkedGraph.values.toList: _*)
      Try(new GraphvizCmdLineEngine()).map(cmdlnEngine => cmdlnEngine.timeout(2, TimeUnit.MINUTES)).map { cmdlnEngine =>
          Graphviz.useEngine(cmdlnEngine)
          Graphviz.fromGraph(g).render(Format.DOT).toFile(new File(s"$dir$fileName.${Format.DOT.fileExtension}"))
        }.map(_ => NetGraph.logger.info(s"Successfully rendered the graph to $dir$fileName.${outputImageFormat.fileExtension}"))
        .recover { case e => NetGraph.logger.error(s"Failed to render the graph to $dir$fileName.${outputImageFormat.fileExtension} : ", e) }
    else
      val edges: List[Action] = sm.edges().asScala.toList.map { edge =>
        val nodeU = edge.nodeU()
        val nodeV = edge.nodeV()
        val edgeValue = sm.edgeValue(nodeU, nodeV).get // Assuming edgeValue method can accept node objects directly
        edgeValue
      }.sortBy(_.fromNode.id) // You might need to adapt this part as well, depending on your data structure

      val nodesMap = nodes.foldLeft(Map[Int, Node]()) { case (acc, nd) =>
        acc + (nd.id -> (
          if nd.id == 0 then
            node(nd.id.toString).`with`(Color.RED).`with`(Label.markdown("**Init**"), Color.rgb("1020d0").font())
          else
            node(nd.id.toString)))
      }
      val linkedGraph = edges.map { edge =>
        nodesMap(edge.fromNode.id).link(to(nodesMap(edge.toNode.id))
          .`with`(weight(if (edge.cost * 10).floor < 1 then 1 else (edge.cost * 10).floor))
        )
      }

      val g = graph(name).`with`(nodesMap.values.toList: _*).
        linkAttr().`with`("class", "link-class").`with`(linkedGraph: _*)
      Try(new GraphvizCmdLineEngine()).map(cmdlnEngine => cmdlnEngine.timeout(2, TimeUnit.MINUTES)).map { cmdlnEngine =>
          Graphviz.useEngine(cmdlnEngine)
          Graphviz.fromGraph(g).render(Format.DOT).toFile(new File(s"$dir$fileName.${Format.DOT.fileExtension}"))
        }.map(_ => NetGraph.logger.info(s"Successfully rendered the graph to $dir$fileName.${outputImageFormat.fileExtension}"))
        .recover { case e => NetGraph.logger.error(s"Failed to render the graph to $dir$fileName.${outputImageFormat.fileExtension} : ", e) }