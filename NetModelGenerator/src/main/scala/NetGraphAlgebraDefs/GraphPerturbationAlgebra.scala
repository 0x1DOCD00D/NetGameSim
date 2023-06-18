package NetGraphAlgebraDefs

import NetGraphAlgebraDefs.NetModelAlgebra.{dissimulationCoefficient, edgeProbability, maxBranchingFactor, maxDepth, maxProperties, perturbationCoeff, propValueRange, targetAppHighPenalty, targetAppLowPenalty}
import NetGraphAlgebraDefs.GraphPerturbationAlgebra.{ACTIONS, EdgeAdded, EdgeModified, EdgeRemoved, ModificationRecord, NodeAdded, NodeModified, OriginalNetComponent, Perturbation, logger}
import Randomizer.SupplierOfRandomness
import Utilz.CreateLogger

import scala.util.{Failure, Success, Try}
import com.google.common.graph.*
import org.slf4j.Logger

import java.util
import scala.annotation.tailrec
import scala.collection.immutable.TreeSeqMap.OrderBy
import scala.collection.immutable.Vector
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

//  six types of perturbations: node modified, node removed, edge removed, edge is modified, new node is added with edges, new edge is added to some existing node
class GraphPerturbationAlgebra(originalModel: NetGraph):
  type GraphPerturbationTuple = (NetGraph, ModificationRecord)
  import GraphPerturbationAlgebra.*
  private val newModel: NetGraph = originalModel.copy()
  private val containsNode: NodeObject => Boolean = (node: NodeObject) => newModel.sm.nodes().asScala.toList.contains(node)
  private val distances: Map[NodeObject, Double] = newModel.distances().toSeq.filter(_._2 < Double.PositiveInfinity).sortBy(_._2).toMap
  private val (minDistance, maxDistance) = (distances.minBy(_._2)._2, distances.maxBy(_._2)._2)
  private val range: Double = maxDistance - minDistance
  def perturbModel(dissimulate: Boolean = false): GraphPerturbationTuple =
    require(NetModelAlgebra.perturbationCoeff > 0 && NetModelAlgebra.perturbationCoeff <= 1, "The perturbation coefficient must be between 0 and 1")
    require(NetModelAlgebra.distanceCoeff >= 0 && NetModelAlgebra.distanceCoeff <= 1, "The distance percentile must be between 0 and 1")

    if range < NetModelAlgebra.distanceSpreadThreshold then
      logger.error(s"The range of distances, $range must be greater than the threshold ${NetModelAlgebra.distanceSpreadThreshold}")
      (newModel, Vector())
    else
      //    Suppose that the max distance is 5 and the distance coefficient is 0.2.
      //    Then the min distance to apply perturbation is 5*0.2 = 1
      val minDistance2ApplyPerturbation = maxDistance * NetModelAlgebra.distanceCoeff
      val nodesToApplyPerturbation: Seq[NodeObject] = distances.filter(_._2 >= minDistance2ApplyPerturbation).keySet.toSeq
      logger.debug(s"Out of the total ${distances.size} nodes, perturbations can be applied to ${nodesToApplyPerturbation.size} nodes that are beyond the distance threshold of $minDistance2ApplyPerturbation")
      if nodesToApplyPerturbation.isEmpty then
        logger.error(s"No nodes exist beyond the distance threshold of $minDistance2ApplyPerturbation")
        (newModel, Vector())
      else
        val probs = SupplierOfRandomness.randProbs(nodesToApplyPerturbation.length).map(_ < (if dissimulate then dissimulationCoefficient else perturbationCoeff))
        val yesOrNo: Iterator[Boolean] = probs.iterator
        logger.info(s"Out of the ${nodesToApplyPerturbation.size} nodes, perturbations will be applied to ${probs.count(_ == true)} nodes")
        (newModel,
          nodesToApplyPerturbation.toList.foldLeft(Vector[(OriginalNetComponent, Perturbation)]()) {
              (acc, node) => if yesOrNo.hasNext && yesOrNo.next() then perturbNode(node, dissimulate) ++ acc else acc
          })
    end if

  private def perturbNode(node: NodeObject, dissimulate: Boolean = false): ModificationRecord =
    val op2do: ACTIONS = ACTIONS.fromOrdinal(SupplierOfRandomness.onDemand(maxv = ACTIONS.values.map(_.ordinal).toList.max))
    if dissimulate then
      logger.debug(s"Dissimulating node $node for the target app")
      modifyNode(node)
    else
      logger.debug(s"Applying perturbation $op2do on node $node")
      op2do match
        case ACTIONS.ADDNODE => if dissimulate then modifyNode(node) else addNode(node)
        case ACTIONS.MODIFYNODE => modifyNode(node)
        case ACTIONS.REMOVENODE => if dissimulate then modifyNode(node) else removeNode(node)
        case op => operationOnEdges(node, op, dissimulate)
  end perturbNode

  /*
  * Iterate through all nodes, and for each node, iterate through all its predecessors and successors.
  * If the predecessor or successor is the node to be removed, then mark the edge removed.
  * */
  private def removeNode(node: NodeObject): ModificationRecord =
    import scala.jdk.OptionConverters.*
    val modificationRecord:ModificationRecord = Vector((OriginalNetComponent(node), NodeRemoved(node))) ++ newModel.sm.predecessors(node).asScala.toList.flatMap { pn =>
      newModel.sm.edgeValue(pn, node).toScala match
        case Some(edge) =>
          Vector((OriginalNetComponent(node), EdgeRemoved(edge)))
        case None => None
    }.toVector ++ newModel.sm.successors(node).asScala.toList.flatMap { sn =>
      newModel.sm.edgeValue(node, sn).toScala match
        case Some(edge) =>
          Vector((OriginalNetComponent(node), EdgeRemoved(edge)))
        case None => None
    }.toVector

    if newModel.sm.removeNode(node) then modificationRecord
    else
      logger.error(s"Failed to remove node $node")
      Vector()

  private def addNode(node: NodeObject): ModificationRecord =
    val newNode: NodeObject = NodeObject(newModel.sm.nodes().asScala.map(_.id).max + 1, SupplierOfRandomness.onDemand(maxv = maxBranchingFactor),
      SupplierOfRandomness.onDemand(maxv = maxProperties), propValueRange = SupplierOfRandomness.onDemand(maxv = propValueRange),
      maxDepth = SupplierOfRandomness.onDemand(maxv = maxDepth), maxBranchingFactor = SupplierOfRandomness.onDemand(maxv = maxBranchingFactor),
      maxProperties = SupplierOfRandomness.onDemand(maxv = maxProperties))
    val newEdge: Action = NetModelAlgebra.createAction(node, newNode)
    if newModel.sm.addNode(newNode) then
      Try(newModel.sm.putEdgeValue(node, newNode, newEdge)) match
        case Success(_) => Vector((OriginalNetComponent(node), NodeAdded(newNode)), (OriginalNetComponent(node), EdgeAdded(newEdge)))
        case Failure(exception) =>
          logger.error(s"Failed to add edge $newEdge for new node $newNode")
          Vector()
    else
      logger.error(s"Failed to add node $newNode")
      Vector()

  private def modifyNode(node: NodeObject): ModificationRecord =
    import scala.jdk.OptionConverters.*
    val modifiedNode: NodeObject = node.modify
    val adjacentNodes = newModel.sm.adjacentNodes(node).asScala.toList ::: List(node)
    logger.debug(s"Adjacent nodes of $node are $adjacentNodes")
    logger.debug(s"Modified version of the node $node is the node $modifiedNode")
    val inducedGraph: MutableValueGraph[NodeObject, Action] = Graphs.inducedSubgraph(newModel.sm, adjacentNodes.toSet.asJava)
    val preds = inducedGraph.predecessors(node).asScala.toList
    val succ = inducedGraph.successors(node).asScala.toList
    newModel.sm.removeNode(node)
    newModel.sm.addNode(modifiedNode)
    preds.foreach(pred => newModel.sm.putEdgeValue(pred, modifiedNode, inducedGraph.edgeValue(pred, node).get))
    succ.foreach(succ => newModel.sm.putEdgeValue(modifiedNode, succ, inducedGraph.edgeValue(node, succ).get))
    Vector((OriginalNetComponent(node), NodeModified(modifiedNode)))

  private def doTheEdge(node: NodeObject, foundNodes: Array[NodeObject], op: (NodeObject, NodeObject)=>ModificationRecord): ModificationRecord =
    if foundNodes.nonEmpty then
      if foundNodes.length == 1 then
        val chosenNode: NodeObject = foundNodes.head
        op(node, chosenNode)
      else
        val chosenNode: NodeObject = foundNodes(SupplierOfRandomness.onDemand(maxv = foundNodes.length))
        op(node, chosenNode)
    else Vector()

  private def operationOnEdges(node: NodeObject, action: ACTIONS, dissimulate: Boolean = false): ModificationRecord =
    import scala.jdk.OptionConverters.*

    val nodesLambda: NodeObject => Boolean = (otherNode: NodeObject) => otherNode != node &&
      (newModel.sm.hasEdgeConnecting(node, otherNode) ||
        newModel.sm.hasEdgeConnecting(otherNode, node))
    if containsNode(node) then
      val allNodes: List[NodeObject] = newModel.sm.nodes().asScala.toList
      if dissimulate then doTheEdge(node, allNodes.filterNot(nodesLambda).toArray[NodeObject], modifyEdge)
      else
        action match
          case ACTIONS.ADDEDGE => doTheEdge(node, allNodes.filter(nodesLambda).toArray[NodeObject], addEdge)
          case ACTIONS.REMOVEEDGE => doTheEdge(node, allNodes.filterNot(nodesLambda).toArray[NodeObject], removeEdge)
          case ACTIONS.MODIFYEDGE => doTheEdge(node, allNodes.filterNot(nodesLambda).toArray[NodeObject], modifyEdge)
          case _ =>
            logger.error(s"Invalid action $action")
            Vector()
    else
      logger.error(s"Node $node does not exist in the model")
      Vector()
  @tailrec
  private def removeEdge(node: NodeObject, chosenNode: NodeObject): ModificationRecord =
    import scala.jdk.OptionConverters.*
    if newModel.sm.hasEdgeConnecting(node, chosenNode) then
      val edge2Remove: Option[Action] = newModel.sm.edgeValue(node, chosenNode).toScala
      if edge2Remove.isDefined then
        if newModel.sm.removeEdge(node, chosenNode) == null then
          logger.error(s"Failed to remove edge from $node to $chosenNode because it does not exist")
          Vector()
        else
          logger.debug(s"Removed an edge from $node to $chosenNode successfully")
          Vector((OriginalNetComponent(node), EdgeRemoved(edge2Remove.get)))
      else
        logger.error(s"No edge exist from $node to $chosenNode but it should be there")
        Vector()
    else if newModel.sm.hasEdgeConnecting(chosenNode, node) then removeEdge(chosenNode, node)
    else Vector()

  private def addEdge(node: NodeObject, chosenNode: NodeObject): ModificationRecord =
    import scala.jdk.OptionConverters.*
    val edge2Modify: Option[Action] = newModel.sm.edgeValue(node, chosenNode).toScala
    val newEdge: Action = if edge2Modify.isEmpty then NetModelAlgebra.createAction(node, chosenNode) else edge2Modify.get.modify
    if newModel.sm.putEdgeValue(node, chosenNode, newEdge) == null then
      logger.debug(s"Added edge $newEdge from $node to $chosenNode successfully")
      Vector((OriginalNetComponent(node), EdgeAdded(newEdge)))
    else
      logger.debug(s"Former edge from $node to $chosenNode was $edge2Modify and it's replaced by the new edge $newEdge")
      Vector((OriginalNetComponent(node), EdgeModified(edge2Modify.get))) ++ removeEdge(node, chosenNode) ++ addEdge(node, chosenNode)

  @tailrec
  private def modifyEdge(node: NodeObject, chosenNode: NodeObject): ModificationRecord =
    import scala.jdk.OptionConverters.*
    if newModel.sm.hasEdgeConnecting(node, chosenNode) then
      val edge2Modify: Option[Action] = newModel.sm.edgeValue(node, chosenNode).toScala
      if edge2Modify.isEmpty then
        logger.error(s"No edge exist from $node to $chosenNode but it should be there")
        Vector()
      else
        Vector((OriginalNetComponent(node), EdgeModified(edge2Modify.get))) ++ removeEdge(node, chosenNode) ++ addEdge(node, chosenNode)
    else modifyEdge(chosenNode, node)

object GraphPerturbationAlgebra:
  trait Perturbation

  case class NodeModified(node: NodeObject) extends Perturbation
  case class NodeRemoved(node: NodeObject) extends Perturbation
  case class NodeAdded(node: NodeObject) extends Perturbation
  case class EdgeRemoved(edge: Action) extends Perturbation
  case class EdgeAdded(edge: Action) extends Perturbation
  case class EdgeModified(action: Action) extends Perturbation

  case class OriginalNetComponent(node: NetGraphComponent)

  type ModificationRecord = Vector[(OriginalNetComponent, Perturbation)]
  type ModificationRecordInverse = Map[NetGraphComponent, Double]

  enum ACTIONS:
    case REMOVENODE, ADDNODE, MODIFYNODE, REMOVEEDGE, ADDEDGE, MODIFYEDGE

  val logger: Logger = CreateLogger(classOf[GraphPerturbationAlgebra.type])

  def apply(graph: NetGraph, dissimulate: Boolean = false): (NetGraph, ModificationRecord) =
    new GraphPerturbationAlgebra(graph).perturbModel(dissimulate)

  def inverseMR(mr: ModificationRecord): ModificationRecordInverse =
    def netComponentFromPerturbation(perturbation: Perturbation): (NetGraphComponent, Double) = perturbation match
      case NodeModified(node) => (node, targetAppLowPenalty)
      case NodeRemoved(node) => (node, targetAppHighPenalty)
      case NodeAdded(node) => (node, targetAppHighPenalty)
      case EdgeRemoved(edge) => (edge, (targetAppHighPenalty+edge.cost)/2)
      case EdgeAdded(edge) => (edge, (targetAppHighPenalty+edge.cost)/2)
      case EdgeModified(edge) => (edge, (targetAppLowPenalty+edge.cost)/2)
    end netComponentFromPerturbation

    mr.foldLeft(Map[NetGraphComponent, Double]())(
      (acc, elem) => {
        val guicomp: (NetGraphComponent, Double) = netComponentFromPerturbation(elem._2)
        logger.debug(s"MR: ${guicomp._1} -> ${acc.getOrElse(guicomp._1, 0.0d) + guicomp._2}")
        acc + (guicomp._1 -> (acc.getOrElse(guicomp._1, 0.0d) + guicomp._2))
      }
    )