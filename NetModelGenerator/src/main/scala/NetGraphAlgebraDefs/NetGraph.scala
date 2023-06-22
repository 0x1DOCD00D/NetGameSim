package NetGraphAlgebraDefs

import Randomizer.SupplierOfRandomness
import Utilz.CreateLogger
import com.google.common.graph.{Graphs, MutableValueGraph, ValueGraphBuilder}
import org.slf4j.Logger
import sun.nio.cs.UTF_8

import java.io.{BufferedReader, ByteArrayInputStream, FileInputStream, FileReader, ObjectInputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardOpenOption}
import java.util.Base64
import scala.annotation.tailrec
import scala.collection.immutable.TreeSeqMap.OrderBy
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try, Using}

case class NetGraph(sm: NetStateMachine, initState: NodeObject):
  import NetGraph.logger

  def copy: NetGraph = NetGraph(Graphs.copyOf(sm), initState)

  def degrees: List[(Int, Int)] = sm.nodes().asScala.toList.map(node => (sm.inDegree(node), sm.outDegree(node)))

  def totalNodes: Int = sm.nodes().asScala.count(_ => true)

  def adjacencyMatrix: Array[Array[Float]] =
    val nodes: Array[NodeObject] = sm.nodes().asScala.toArray
    val matrix: Array[Array[Float]] = Array.ofDim[Float](nodes.length, nodes.length)
    nodes.indices.foreach(i =>
      nodes.indices.foreach(j =>
        if sm.hasEdgeConnecting(nodes(i), nodes(j)) then matrix(i)(j) = sm.edgeValue(nodes(i), nodes(j)).get.cost.toFloat
        else matrix(i)(j) = Float.PositiveInfinity
      )
    )
    matrix
  end adjacencyMatrix

  extension (m: Array[Array[Float]])
    def toCsv: String =
      val sb = new StringBuilder
      m.foreach(row =>
        sb.append("\n")
        row.foreach(cell => if cell != Float.PositiveInfinity then sb.append(f"$cell%.3f").append(",") else sb.append("-").append(",")))
      sb.toString
    end toCsv
  end extension

  def persist(dir: String, fileName: String): Unit =
    import java.io._
    import java.util.Base64
    import java.nio.charset.StandardCharsets.UTF_8
    import java.io.{FileOutputStream, ObjectOutputStream}

    val encodedGraph: Unit = Try(new ByteArrayOutputStream()).map(baos => (baos, new ObjectOutputStream(baos))).map { case (baos, oos) =>
      val lst = sm.nodes().asScala.toList.map(node =>
        baos.reset()
        oos.writeObject(node)
        oos.flush()
        s"Node ${node.id}:".concat(new String(Base64.getEncoder.encode(baos.toByteArray), UTF_8))
      ) ::: sm.edges().asScala.toList.map(edge =>
            val action = sm.edgeValue(edge.source(), edge.target()).get
            baos.reset()
            oos.writeObject(action)
            oos.flush()
            s"Edge ${edge.source().id} -> ${edge.target().id}:".concat(new String(Base64.getEncoder.encode(baos.toByteArray), UTF_8)))
      baos.close()
      oos.close()
      lst
    }.map(lstOfSerializedString =>
      import java.nio.file.{Files, Paths, StandardOpenOption}
      import java.nio.charset.StandardCharsets
      import scala.jdk.CollectionConverters.*
      Try(Files.write(
        Paths.get(s"$dir$fileName"),
        lstOfSerializedString.toSeq.asJava,
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING
      ))) match
            case Success(f) => logger.info(s"Successfully persisted the graph to ${f.get}")
            case Failure(exception) => logger.error(s"Failed to persist the graph to $dir$fileName", exception)
  end persist

  def maxOutDegree(): Int = sm.nodes().asScala.map(node => sm.outDegree(node)).max

  def getRandomConnectedNode(from: NodeObject): Option[(NodeObject, Action)] =
    val successors: Array[NodeObject] = sm.successors(from).asScala.toArray
    if successors.isEmpty then None
    else
      val randomSuccessor: NodeObject = successors(SupplierOfRandomness.onDemand(maxv = successors.length))
      val edge: Action = sm.edgeValue(from, randomSuccessor).get
      Some((randomSuccessor, edge))

  def unreachableNodes(): (Set[NodeObject], Int) =
    var loopsInGraph: Int = 0

    @tailrec
    def dfs(nodes: List[NodeObject], visited: Set[NodeObject]): Set[NodeObject] =
      nodes match
        case Nil => Set.empty[NodeObject]
        case hd :: tl =>
          if visited.contains(hd) then
            loopsInGraph += 1
            Set.empty[NodeObject]
          else
            dfs(tl ::: sm.successors(hd).asScala.toList, visited + hd) // ++ dfs(tl, visited + hd)

    end dfs

    val (reachableNodes: Set[NodeObject], loops: Int) = {
      val rns = dfs(sm.successors(initState).asScala.toList, Set())
      (rns, loopsInGraph)
    }
    val allNodes: Set[NodeObject] = sm.nodes().asScala.toSet
    logger.info(f"The reachability ration is ${reachableNodes.size.toFloat * 100 / allNodes.size.toFloat}%4.2f or there are ${reachableNodes.size} reachable nodes out of total ${allNodes.size} nodes in the graph")
    (allNodes -- reachableNodes -- Set(initState), loops)
  end unreachableNodes


  def distances(): Map[NodeObject, Double] =
    val distanceMap: scala.collection.mutable.Map[NodeObject, Double] = collection.mutable.Map() ++ sm.nodes().asScala.map(node => node -> Double.PositiveInfinity).toMap
    val zeroCost: Double = 0.0d
    val noEdgeCost: Double = Double.NaN
    distanceMap += (initState -> zeroCost)

    def relax(u: NodeObject)(v: NodeObject): Boolean =
      import scala.jdk.OptionConverters.*
      val edgeCost = if sm.hasEdgeConnecting(u, v) then
        sm.edgeValue(u, v).toScala match
          case Some(action) => action.cost
          case None => noEdgeCost
      else noEdgeCost
      if edgeCost.isNaN then false
      else if distanceMap(v) > distanceMap(u) + edgeCost then
        distanceMap(v) = distanceMap(u) + edgeCost
        true
      else false
    end relax

    def explore(node: NodeObject): Unit =
      require(node != null, "The NodeObject node must not be null")
      val successors = sm.successors(node).asScala.toList
      val relaxNode: NodeObject => Boolean = relax(node)
      successors match
        case Nil => ()
        case hd :: tl => if relaxNode(hd) then explore(hd)
          tl.foreach(cn => if relaxNode(cn) then explore(cn) else ())
    end explore

    explore(initState)
    distanceMap.toMap
  end distances

object NetGraph:
  val logger: Logger = CreateLogger(classOf[NetGraph])

  def load(dir: String, fileName: String): Option[List[Try[NodeObject]]] =
    Using(new BufferedReader(new FileReader(s"$dir$fileName"))) { reader =>
      Iterator.continually(reader.readLine()).takeWhile(_ != null).toSeq
    } match
      case Failure(exception) =>
        logger.error(s"Failed to load the NetGraph from $dir$fileName", exception)
        None
      case Success(lines) => Some(lines.map(line => {
          Try(Base64.getDecoder.decode(line.getBytes(StandardCharsets.UTF_8))).
            map(bytes => new ObjectInputStream(new ByteArrayInputStream(bytes))).
            map(ois => {
              val value = ois.readObject.asInstanceOf[NodeObject]
              ois.close()
              value
            })
        }).toList)
  end load

end NetGraph

