package NetGraphAlgebraDefs

import NetGraphAlgebraDefs.NetModelAlgebra.{logger, outputDirectory}
import com.typesafe.config.ConfigFactory
import Utilz.NGSConstants
import guru.nidi.graphviz.attribute.Attributes.attr
import guru.nidi.graphviz.attribute.LinkAttr.weight
import guru.nidi.graphviz.attribute.{Color, Font, Label, LinkAttr, Rank, Style}

import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}
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
  def persist(dir: String = outputDirectory, fileName: String = NGSConstants.OUTPUTFILENAME()): Unit =
    val config = ConfigFactory.load()
    val outputGraphRepresentation = config.getConfig("NGSimulator").getConfig("OutputGraphRepresentation").getString("contentType")
    if (outputGraphRepresentation == "json") then
        Try {
          val nodesInGraph: String = sm.nodes().asScala.asJson.noSpaces
          val edgesInGraph: String = sm.edges().asScala.toList.map { edge =>
            sm.edgeValue(edge.source(), edge.target()).get
          }.asJson.noSpaces

          val file = new FileWriter(s"$dir$fileName")
          file.write(nodesInGraph + "\n" + edgesInGraph)
          file.close()
        }.map(_ => NetGraph.logger.info(s"Successfully persisted the graph in json to $dir$fileName"))
          .recover { case e => NetGraph.logger.error(s"Failed to persist the graph in json to $dir$fileName : ", e) }
    else {
      import java.io._
      import java.util.Base64
      import java.nio.charset.StandardCharsets.UTF_8

      val fullGraphAsList: List[NetGraphComponent] = sm.nodes().asScala.toList ::: sm.edges().asScala.toList.map { edge =>
        sm.edgeValue(edge.source(), edge.target()).get
      }
      Try(new FileOutputStream(s"$dir$fileName", false)).map(fos => new ObjectOutputStream(fos)).map { oos =>
          oos.writeObject(fullGraphAsList)
          oos.flush()
          oos.close()
        }.map(_ => NetGraph.logger.info(s"Successfully persisted the graph to $dir$fileName"))
        .recover { case e => NetGraph.logger.error(s"Failed to persist the graph to $dir$fileName : ", e) }
    }

  //  Use the following graphviz command to render the graph to an image:
  //  sfdp -x -Goverlap=scale -Tpng graph.dot > graph.png
  def toDotVizFormat(name: String, dir: String = outputDirectory, fileName: String, outputImageFormat: Format = Format.DOT): Unit =
    val nodes: List[NodeObject] = initState :: sm.nodes().asScala.toList
    if nodes.count(_.id == 0) < 1 then
      logger.error("The graph does not contain a start node with id 0")
    else
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