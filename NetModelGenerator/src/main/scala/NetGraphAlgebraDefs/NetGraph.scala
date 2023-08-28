package NetGraphAlgebraDefs

import NetGraphAlgebraDefs.NetModelAlgebra.{createAction, desiredReachabilityCoverage, edgeProbability, outputDirectory}
import Randomizer.SupplierOfRandomness
import Utilz.{CreateLogger, NGSConstants}
import com.google.common.graph.{EndpointPair, Graphs, MutableValueGraph, ValueGraphBuilder}
import org.apache.commons.io.FileUtils
import org.slf4j.Logger
import sun.nio.cs.UTF_8

import java.io.{BufferedReader, BufferedWriter, ByteArrayInputStream, File, FileInputStream, FileReader, FileWriter, ObjectInputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardOpenOption}
import java.util.Base64
import scala.annotation.tailrec
import scala.collection.immutable.TreeSeqMap.OrderBy
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try, Using}
import scala.collection.parallel.*
import scala.collection.parallel.CollectionConverters.*

case class NetGraph(sm: NetStateMachine, initState: NodeObject) extends GraphStore with Ordered[NetGraph]:
  import NetGraph.logger

  override def compare(that: NetGraph): Int =
    import scala.collection.parallel.*
    import scala.collection.parallel.CollectionConverters.*

    val thisNodes: ParSeq[NodeObject] = sm.nodes().asScala.toList.par
    val thatNodes: ParSeq[NodeObject] = that.sm.nodes().asScala.toList.par
    val thisEdges: List[EndpointPair[NodeObject]] = sm.edges().asScala.toList
    val thatEdges: List[EndpointPair[NodeObject]] = that.sm.edges().asScala.toList

    thisNodes.foldLeft(0) {
      case (acc, node) => if thatNodes.exists(_ == node) then acc else
        logger.error(s"Node ${node.id} is not in the other graph")
        acc + 1
    } + thatNodes.foldLeft(0) {
      case (acc, node) => if thisNodes.exists(_ == node) then acc else
        logger.error(s"Node ${node.id} is not in the other graph")
        acc + 1
    } + thisEdges.foldLeft(0){ case (acc, e) =>
      if e.nodeU() == thatEdges.find(n => n == e).get.nodeU() &&
        e.nodeV() == thatEdges.find(n => n == e).get.nodeV()
      then acc else
        logger.error(s"Edge ${e.nodeU().id} -> ${e.nodeV().id} is not in the other graph")
        acc+1
    } + thatEdges.foldLeft(0){ case (acc, e) =>
      if e.nodeU() == thisEdges.find(n => n == e).get.nodeU() &&
        e.nodeV() == thisEdges.find(n => n == e).get.nodeV()
      then acc else
        logger.error(s"Edge ${e.nodeU().id} -> ${e.nodeV().id} is not in the other graph")
        acc+1
    }
  end compare

  def copy: NetGraph = NetGraph(Graphs.copyOf(sm), initState)

  def degrees: List[(Int, Int)] = sm.nodes().asScala.toList.map(node => (sm.inDegree(node), sm.outDegree(node)))

  def totalNodes: Int = sm.nodes().asScala.count(_ => true)

  @tailrec final def forceReachability: Set[NodeObject] =
    import scala.jdk.CollectionConverters.*
    val orphanNodes: List[NodeObject] = unreachableNodes()._1.toList
    logger.info(s"Force reachability: there are ${orphanNodes.size} orphan nodes in the graph")
    if orphanNodes.size <= 0 then Set.empty
    else
      val reachableNodes: List[NodeObject] = (sm.nodes().asScala.toSet -- orphanNodes).toList.sortBy(node => (sm.outDegree(node), sm.inDegree(node)))(Ordering.Tuple2(Ordering.Int, Ordering.Int.reverse))
      val rnSize = reachableNodes.size
      if rnSize <= 0 then
        logger.error("There are no reachable nodes in the graph from the init node")
        Set.empty
      else
        val rn: NodeObject = reachableNodes.head
        val unr: NodeObject = orphanNodes.minBy(node => sm.inDegree(node))
        sm.putEdgeValue(rn, unr, createAction(rn, unr))

        val nodesLength = orphanNodes.size
        val totalCombinationsOfNodes = nodesLength * nodesLength
        val nodes2AddEdges: ParSeq[(Int, Int)] = SupplierOfRandomness.randProbs(totalCombinationsOfNodes)().par.map(_ < edgeProbability).zipWithIndex.filter(_._1 == true).map(v => (v._2 / nodesLength, v._2 % nodesLength))
        nodes2AddEdges.seq.foreach {
          case (from, to) =>
            if from != to then
              val nodeFrom = orphanNodes(from)
              val nodeTo = orphanNodes(to)
              if nodeTo != nodeFrom then
                if !sm.edgeValue(nodeFrom, nodeTo).isPresent then
                  logger.debug(s"Adding an edge from orphan ${nodeFrom.id} to orphan ${nodeTo.id}")
                  sm.putEdgeValue(nodeFrom, nodeTo, createAction(nodeFrom, nodeTo))
                else if !sm.edgeValue(nodeTo, nodeFrom).isPresent then
                  logger.debug(s"Adding an edge from orphan ${nodeTo.id} to orphan ${nodeFrom.id}")
                  sm.putEdgeValue(nodeTo, nodeFrom, createAction(nodeTo, nodeFrom))
                else ()
              else ()
        }
        forceReachability


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

  def maxOutDegree(): Int = sm.nodes().asScala.map(node => sm.outDegree(node)).max

  def getRandomConnectedNode(from: NodeObject): Option[(NodeObject, Action)] =
    val successors: Array[NodeObject] = sm.successors(from).asScala.toArray
    if successors.isEmpty then None
    else
      val randomSuccessor: NodeObject = successors(SupplierOfRandomness.onDemandInt(pmaxv = successors.length))
      val edge: Action = sm.edgeValue(from, randomSuccessor).get
      Some((randomSuccessor, edge))

  def unreachableNodes(): (Set[NodeObject], Int) =
    var loopsInGraph: Int = 0

    @tailrec
    def dfs(nodes: List[NodeObject], visited: Set[NodeObject]): Set[NodeObject] =
      if  visited.size.toFloat/(sm.nodes().size-1).toFloat >= desiredReachabilityCoverage then visited
      else
        nodes match
          case Nil => visited
          case hd :: tl =>
            if visited.contains(hd) then
              loopsInGraph += 1
              dfs(tl, visited)
            else
              dfs(sm.successors(hd).asScala.toList.filterNot(n=>visited.contains(n)) ::: tl, visited + hd) // ++ dfs(tl, visited + hd)
    end dfs

    val (reachableNodes: Set[NodeObject], loops: Int) = {
      val rns = dfs(sm.successors(initState).asScala.toList, Set())
      logger.info(s"DFS: reachable ${rns.size} nodes with $loopsInGraph loops in the graph")
      (rns, loopsInGraph)
    }
    val allNodes: Set[NodeObject] = sm.nodes().asScala.toSet
    logger.info(f"The reachability ratio is ${reachableNodes.size.toFloat * 100 / (allNodes.size-1).toFloat}%4.2f or there are ${reachableNodes.size} reachable nodes out of total ${allNodes.size-1} nodes in the graph")
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

  def load(fileName: String, dir: String = outputDirectory): Option[NetGraph] =
    logger.info(s"Loading the NetGraph from $dir$fileName")

    Try(new FileInputStream(s"$dir$fileName")).map ( fis => (fis, new ObjectInputStream(fis)) ).map { (fis,ois) =>
      val ng = ois.readObject.asInstanceOf[List[NetGraphComponent]]
      ois.close()
      fis.close()
      ng
    }.toOption.flatMap {
      lstOfNetComponents =>
        val nodes = lstOfNetComponents.collect { case node: NodeObject => node }
        val edges = lstOfNetComponents.collect { case edge: Action => edge }
        NetModelAlgebra(nodes, edges)
    }
  end load
end NetGraph

