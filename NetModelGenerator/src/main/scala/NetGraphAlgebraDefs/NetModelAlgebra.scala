package NetGraphAlgebraDefs

import NetGraphAlgebraDefs.NetModelAlgebra.{actionRange, connectedness, createAction, edgeProbability, logger, maxBranchingFactor, maxDepth, maxProperties, propValueRange, statesTotal}
import Randomizer.{SupplierOfRandomness, UniformProbGenerator}
import Utilz.ConfigReader.getConfigEntry
import Utilz.{CreateLogger, NGSConstants}
import Utilz.NGSConstants.{ACTIONRANGE, ACTIONRANGEDEFAULT, CONNECTEDNESS, CONNECTEDNESSDEFAULT, COSTOFDETECTION, COSTOFDETECTIONDEFAULT, DEFAULTDISSIMULATIONCOEFFICIENT, DEFAULTDISTANCECOEFFICIENT, DEFAULTDISTANCESPREADTHRESHOLD, DEFAULTEDGEPROBABILITY, DEFAULTPERTURBATIONCOEFFICIENT, DISSIMULATIONCOEFFICIENT, DISTANCECOEFFICIENT, DISTANCESPREADTHRESHOLD, DOPPLEGANGERS, DOPPLEGANGERSDEFAULT, EDGEPROBABILITY, GRAPHWALKNODETERMINATIONPROBABILITY, GRAPHWALKNODETERMINATIONPROBABILITYDEFAULT, GRAPHWALKTERMINATIONPOLICY, GRAPHWALKTERMINATIONPOLICYDEFAULT, MALAPPBUDGET, MALAPPBUDGETDEFAULT, MAXBRANCHINGFACTOR, MAXBRANCHINGFACTORDEFAULT, MAXDEPTH, MAXDEPTHDEFAULT, MAXPROPERTIES, MAXPROPERTIESDEFAULT, MAXWALKPATHLENGTHCOEFF, MAXWALKPATHLENGTHCOEFFDEFAULT, NUMBEROFEXPERIMENTS, NUMBEROFEXPERIMENTSDEFAULT, PERTURBATIONCOEFFICIENT, PROPVALUERANGE, PROPVALUERANGEDEFAULT, SEED, SERVICEPENALTY, SERVICEPENALTYDEFAULT, SERVICEREWARD, SERVICEREWARDDEFAULT, SERVICEREWARDPROBABILITY, SERVICEREWARDPROBABILITYDEFAULT, STATESTOTAL, STATESTOTALDEFAULT, TARGETAPPHIGHPENALTY, TARGETAPPHIGHPENALTYDEFAULT, TARGETAPPLOWPENALTY, TARGETAPPLOWPENALTYDEFAULT, TARGETAPPSCORE, TARGETAPPSCOREDEFAULT, WALKS, WALKSDEFAULT}
import com.google.common.graph.*
import org.slf4j.Logger

import scala.collection.immutable.TreeSeqMap.OrderBy
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Random, Success, Try}

type NetStateMachine = MutableValueGraph[NodeObject, Action]
class NetModel extends NetGraphConnectednessFinalizer:
  require(statesTotal > 0, "The total number of states must be positive")
  require(maxBranchingFactor > 0, "The maximum branching factor must be greater than zero")
  require(maxDepth > 0, "The maximum depth must be greater than zero")
  require(maxProperties > 0, "The maximum number of properties must be greater than zero")
  require(propValueRange > 0, "The range of property values must be greater than zero")
  require(actionRange > 0, "The range of actions must be greater than zero")

  private [this] val stateMachine: NetStateMachine = ValueGraphBuilder.directed().build()
  val modelUUID:String = java.util.UUID.randomUUID.toString

  private def createNodes(): Unit =
    (1 to statesTotal).foreach(id=>
      if !stateMachine.addNode(NodeObject(id, SupplierOfRandomness.onDemand(maxv = maxBranchingFactor),
        SupplierOfRandomness.onDemand(maxv = maxProperties), propValueRange = SupplierOfRandomness.onDemand(maxv = propValueRange),
        maxDepth = SupplierOfRandomness.onDemand(maxv = maxDepth), maxBranchingFactor = SupplierOfRandomness.onDemand(maxv = maxBranchingFactor),
        maxProperties = SupplierOfRandomness.onDemand(maxv = maxProperties)
        )) then logger.error(s"Could not add node with id $id")
      ()
    )

  def generateModel(forceLinkOrphans: Boolean = false): NetGraph =
    createNodes()
    val allNodes: Array[NodeObject] = stateMachine.nodes().asScala.toArray
    val pvIter: Iterator[Boolean] = SupplierOfRandomness.randProbs(allNodes.length*allNodes.length).map(_ < edgeProbability).iterator
    allNodes.foreach(node=>
      allNodes.foreach(other=>
        if node != other && pvIter.nonEmpty && pvIter.next() then
          stateMachine.putEdgeValue(node, other, createAction(node, other))
        else ()
      )
    )
    logger.info(s"Generated graph with ${stateMachine.nodes().size()} nodes and ${stateMachine.edges().size()} edges")
    val initState: NodeObject = addInitState(allNodes)
    val generatedGraph: NetGraph = NetGraph(stateMachine, initState)
    if forceLinkOrphans then
      val unreachableNodes: Set[NodeObject] = generatedGraph.unreachableNodes()._1
      logger.info(s"Linking ${unreachableNodes.size} orphans to the initial state")
      val reachableNodes: Set[NodeObject] = stateMachine.nodes().asScala.toSet -- unreachableNodes
      val rnSize = reachableNodes.size
      if rnSize > 0 then
        val arrReachableNodes = reachableNodes.toArray
        unreachableNodes.foreach(urn =>
          val index = SupplierOfRandomness.onDemand(maxv = rnSize)
          stateMachine.putEdgeValue(arrReachableNodes(index), urn, createAction(arrReachableNodes(index), urn))
        )
      else unreachableNodes.foreach(unreachableNode =>stateMachine.putEdgeValue(initState, unreachableNode, createAction(initState, unreachableNode)))
    generatedGraph
  end generateModel

  def addInitState(allNodes: Array[NodeObject]): NodeObject =
    val maxOutdegree = stateMachine.nodes().asScala.map(node => stateMachine.outDegree(node)).max
    val newInitNode: NodeObject = NodeObject(0, SupplierOfRandomness.onDemand(maxv = maxBranchingFactor),
      SupplierOfRandomness.onDemand(maxv = maxProperties), propValueRange = SupplierOfRandomness.onDemand(maxv = propValueRange),
      maxDepth = SupplierOfRandomness.onDemand(maxv = maxDepth), maxBranchingFactor = SupplierOfRandomness.onDemand(maxv = maxBranchingFactor),
      maxProperties = SupplierOfRandomness.onDemand(maxv = maxProperties)
    )
    stateMachine.addNode(newInitNode)
    val orphans: Array[NodeObject] = allNodes.filter(node =>
//      NetModelAlgebra.logger.info(s"${stateMachine.inDegree(node)}")
      stateMachine.incidentEdges(node).isEmpty)
    orphans.foreach(node =>
      stateMachine.putEdgeValue(newInitNode, node, createAction(newInitNode, node)))
    val connected: Array[NodeObject] = allNodes.filter(node => stateMachine.outDegree(node) > (if maxOutdegree >= connectedness then connectedness else maxOutdegree - 1))
    connected.foreach(node =>
      stateMachine.putEdgeValue(newInitNode, node, createAction(newInitNode, node)))
    newInitNode

object NetModelAlgebra:
  val logger:Logger = CreateLogger(classOf[NetModel])

  val dopplegangers: Int = getConfigEntry(NGSConstants.configNetGameModel, DOPPLEGANGERS, DOPPLEGANGERSDEFAULT)
  val distanceSpreadThreshold: Double = getConfigEntry(NGSConstants.configNetGameModel, DISTANCESPREADTHRESHOLD, DEFAULTDISTANCESPREADTHRESHOLD)
  val dissimulationCoefficient: Double = getConfigEntry(NGSConstants.configNetGameModel, DISSIMULATIONCOEFFICIENT, DEFAULTDISSIMULATIONCOEFFICIENT)
  val perturbationCoeff: Double = getConfigEntry(NGSConstants.configNetGameModel, PERTURBATIONCOEFFICIENT, DEFAULTPERTURBATIONCOEFFICIENT)
  val distanceCoeff: Double = getConfigEntry(NGSConstants.configNetGameModel, DISTANCECOEFFICIENT, DEFAULTDISTANCECOEFFICIENT)
  val edgeProbability: Double = getConfigEntry(NGSConstants.configNetGameModel,EDGEPROBABILITY, DEFAULTEDGEPROBABILITY)
  val numberOfExperiments: Int = getConfigEntry(NGSConstants.configNetGameModel,NUMBEROFEXPERIMENTS, NUMBEROFEXPERIMENTSDEFAULT)
  val statesTotal: Int = getConfigEntry(NGSConstants.configNetGameModel,STATESTOTAL, STATESTOTALDEFAULT)
  val numberOfWalks: Int = getConfigEntry(NGSConstants.configNetGameModel,WALKS, WALKSDEFAULT)
  val maxBranchingFactor: Int = getConfigEntry(NGSConstants.configNetGameModel,MAXBRANCHINGFACTOR, MAXBRANCHINGFACTORDEFAULT)
  val maxDepth: Int = getConfigEntry(NGSConstants.configNetGameModel,MAXDEPTH, MAXDEPTHDEFAULT)
  val maxProperties: Int = getConfigEntry(NGSConstants.configNetGameModel,MAXPROPERTIES, MAXPROPERTIESDEFAULT)
  val propValueRange: Int = getConfigEntry(NGSConstants.configNetGameModel,PROPVALUERANGE, PROPVALUERANGEDEFAULT)
  val actionRange: Int = getConfigEntry(NGSConstants.configNetGameModel,ACTIONRANGE, ACTIONRANGEDEFAULT)
  val connectedness: Int = getConfigEntry(NGSConstants.configNetGameModel,CONNECTEDNESS, CONNECTEDNESSDEFAULT)
  val maxWalkPathLengthCoeff: Double = getConfigEntry(NGSConstants.configNetGameModel,MAXWALKPATHLENGTHCOEFF, MAXWALKPATHLENGTHCOEFFDEFAULT)
  val graphWalkTerminationPolicy: String = getConfigEntry(NGSConstants.configNetGameModel,GRAPHWALKTERMINATIONPOLICY, GRAPHWALKTERMINATIONPOLICYDEFAULT)
  val graphWalkNodeTerminationProbability: Double = getConfigEntry(NGSConstants.configNetGameModel,GRAPHWALKNODETERMINATIONPROBABILITY, GRAPHWALKNODETERMINATIONPROBABILITYDEFAULT)
  val MAXPATHLENGTHTC:String = "maxpathlength"
  val UNTILCYCLETC:String = "untilcycle"
  val ALLTC:String = "all"

  val mapAppBudget: Double = getConfigEntry(NGSConstants.configCostRewards,MALAPPBUDGET, MALAPPBUDGETDEFAULT)
  val costOfDetection: Double = getConfigEntry(NGSConstants.configCostRewards,COSTOFDETECTION, COSTOFDETECTIONDEFAULT)
  val serviceReward: Double = getConfigEntry(NGSConstants.configCostRewards,SERVICEREWARD, SERVICEREWARDDEFAULT)
  val serviceRewardProbability: Double = getConfigEntry(NGSConstants.configCostRewards,SERVICEREWARDPROBABILITY, SERVICEREWARDPROBABILITYDEFAULT)
  val servicePenalty: Double = getConfigEntry(NGSConstants.configCostRewards,SERVICEPENALTY, SERVICEPENALTYDEFAULT)
  val targetAppScore: Double = getConfigEntry(NGSConstants.configCostRewards,TARGETAPPSCORE, TARGETAPPSCOREDEFAULT)
  val targetAppLowPenalty: Double = getConfigEntry(NGSConstants.configCostRewards,TARGETAPPLOWPENALTY, TARGETAPPLOWPENALTYDEFAULT)
  val targetAppHighPenalty: Double = getConfigEntry(NGSConstants.configCostRewards,TARGETAPPHIGHPENALTY, TARGETAPPHIGHPENALTYDEFAULT)

  def getFields: Map[String, Double] = this.getClass.getDeclaredFields.filter(field => field.getType == classOf[Double]).map(field => field.getName -> field.get(this).asInstanceOf[Double]).toMap[String, Double] ++  this.getClass.getDeclaredFields.filter(field => field.getType == classOf[Int]).map(field => field.getName -> field.get(this).toString.toDouble).toMap[String, Double]

  def apply(forceLinkOrphans: Boolean = true): NetGraph = new NetModel().generateModel(forceLinkOrphans)

  def createAction(from: NodeObject, to: NodeObject): Action =
    val fCount = from.childrenCount
    val tCount = to.childrenCount
    val cost: Double = SupplierOfRandomness.randProbs(1).head
    require(cost >= 0 && cost <= 1)

    Action(SupplierOfRandomness.onDemand(maxv = actionRange),
      if fCount > 0 then SupplierOfRandomness.onDemand(maxv = fCount) else 0,
      if tCount > 0 then SupplierOfRandomness.onDemand(maxv = tCount) else 0,
      if SupplierOfRandomness.onDemand() % 2 == 0 then None else Some(SupplierOfRandomness.onDemand(maxv = propValueRange)),
      cost
    )

  //  each node of the graph is a NodeObject that corresponds to a GUI screen, which is a tree of GuiObjects
  @main def runNetModelAlgebra(args: String*): Unit =
    logger.info("File NetModelGenerator/src/main/scala/NetGraph/NetModelAlgebra.scala created at time 5:42 PM")
