package NetGraphAlgebraDefs

import NetGraphAlgebraDefs.NetModelAlgebra.{actionRange, connectedness, createAction, edgeProbability, logger, maxBranchingFactor, maxDepth, maxProperties, propValueRange, statesTotal}
import Randomizer.{SupplierOfRandomness, UniformProbGenerator}
import Utilz.ConfigReader.getConfigEntry
import Utilz.{CreateLogger, NGSConstants}
import Utilz.NGSConstants.{ACTIONRANGE, ACTIONRANGEDEFAULT, CONNECTEDNESS, CONNECTEDNESSDEFAULT, COSTOFDETECTION, COSTOFDETECTIONDEFAULT, DEFAULTDISSIMULATIONCOEFFICIENT, DEFAULTDISTANCECOEFFICIENT, DEFAULTDISTANCESPREADTHRESHOLD, DEFAULTEDGEPROBABILITY, DEFAULTPERTURBATIONCOEFFICIENT, DISSIMULATIONCOEFFICIENT, DISTANCECOEFFICIENT, DISTANCESPREADTHRESHOLD, DOPPLEGANGERS, DOPPLEGANGERSDEFAULT, EDGEPROBABILITY, GRAPHWALKNODETERMINATIONPROBABILITY, GRAPHWALKNODETERMINATIONPROBABILITYDEFAULT, GRAPHWALKTERMINATIONPOLICY, GRAPHWALKTERMINATIONPOLICYDEFAULT, MALAPPBUDGET, MALAPPBUDGETDEFAULT, MAXBRANCHINGFACTOR, MAXBRANCHINGFACTORDEFAULT, MAXDEPTH, MAXDEPTHDEFAULT, MAXPROPERTIES, MAXPROPERTIESDEFAULT, MAXWALKPATHLENGTHCOEFF, MAXWALKPATHLENGTHCOEFFDEFAULT, NUMBEROFEXPERIMENTS, NUMBEROFEXPERIMENTSDEFAULT, PERTURBATIONCOEFFICIENT, PROPVALUERANGE, PROPVALUERANGEDEFAULT, SEED, SERVICEPENALTY, SERVICEPENALTYDEFAULT, SERVICEREWARD, SERVICEREWARDDEFAULT, SERVICEREWARDPROBABILITY, SERVICEREWARDPROBABILITYDEFAULT, STATESTOTAL, STATESTOTALDEFAULT, TARGETAPPHIGHPENALTY, TARGETAPPHIGHPENALTYDEFAULT, TARGETAPPLOWPENALTY, TARGETAPPLOWPENALTYDEFAULT, TARGETAPPSCORE, TARGETAPPSCOREDEFAULT, WALKS, WALKSDEFAULT}
import com.google.common.graph.*
import org.slf4j.Logger

import java.io.File
import scala.collection.immutable.TreeSeqMap.OrderBy
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Random, Success, Try}

type NetStateMachine = MutableValueGraph[NodeObject, Action]
class NetModel extends NetGraphConnectednessFinalizer:
  require(statesTotal > 0, "The total number of states must be positive")
  require(connectedness < statesTotal, "The total number of edges from the init node must be less than the number of states")
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
        maxProperties = SupplierOfRandomness.onDemand(maxv = maxProperties), SupplierOfRandomness.randProbs(1).head
        )) then logger.error(s"Could not add node with id $id")
      ()
    )

  def generateModel(nodes: List[NodeObject], edges: List[Action]): Option[NetGraph] =
    val sanityCheck = edges.foldLeft(true) {
      (acc, edge) =>
        val nodeFrom = nodes.find(_.id == edge.fromNode)
        val nodeTo = nodes.find(_.id == edge.toNode)
        if nodeFrom.isEmpty then logger.error(s"Could not find node with id ${edge.fromNode}")
        if nodeTo.isEmpty then logger.error(s"Could not find node with id ${edge.toNode}")
        if nodeFrom.isEmpty || nodeTo.isEmpty then false
        else acc
    } && nodes.exists(_.id == 0) && edges.foldLeft(true) {
      (acc, edge) => acc && nodes.exists(_.id == edge.fromNode) && nodes.exists(_.id == edge.fromNode)
    }

    if sanityCheck then
      nodes.foreach { node =>
        if !stateMachine.addNode(node) then logger.error(s"Could not add node with id ${node.id}")
        ()
      }
      edges.foreach { edge =>
        val nodeFrom: NodeObject = nodes.find(_.id == edge.fromNode).get
        val nodeTo: NodeObject = nodes.find(_.id == edge.toNode).get
        Try(stateMachine.putEdgeValue(nodeFrom, nodeTo, edge)) match
          case Failure(exception) => logger.error(s"Could not add edge from ${edge.fromId} to ${edge.toId} for reason ${exception.getMessage}")
          case Success(_) => ()
      }
      Some(NetGraph(stateMachine, nodes.find(_.id == 0).get))
    else None
  end generateModel

  def generateModel(forceReachability: Boolean = false): NetGraph =
    logger.info(s"Generating graph with $statesTotal nodes and ${if forceReachability then "ensuring complete" else "random"} reachability")
    createNodes()
    logger.info(s"Created ${stateMachine.nodes().size()} nodes")
    val allNodes: Array[NodeObject] = stateMachine.nodes().asScala.toArray
    val nodesLength = allNodes.length
    val totalCombinationsOfNodes = allNodes.length*allNodes.length
    SupplierOfRandomness.randProbs(totalCombinationsOfNodes).map(_ < edgeProbability).zipWithIndex.filter(_._1).map(v=>(v._2/nodesLength, v._2%nodesLength)).foreach {
      case (from, to) =>
        val nodeFrom = allNodes(from)
        val nodeTo = allNodes(to)
        if nodeTo != nodeFrom then
          logger.info(s"Adding edge from ${nodeFrom.id} to ${nodeTo.id}")
          stateMachine.putEdgeValue(nodeFrom, nodeTo, createAction(nodeFrom, nodeTo))
    }
    logger.info(s"Generated graph with ${stateMachine.nodes().size()} nodes and ${stateMachine.edges().size()} edges")
    val initState: NodeObject = addInitState(allNodes)
    val generatedGraph: NetGraph = NetGraph(stateMachine, initState)
    logger.info(s"Added init state connected with $connectedness nodes")
    if forceReachability then generatedGraph.forceReachability
    generatedGraph
  end generateModel

  def addInitState(allNodes: Array[NodeObject]): NodeObject =
    val maxOutdegree = stateMachine.nodes().asScala.map(node => stateMachine.outDegree(node)).max
    val newInitNode: NodeObject = NodeObject(0, SupplierOfRandomness.onDemand(maxv = maxBranchingFactor),
      SupplierOfRandomness.onDemand(maxv = maxProperties), propValueRange = SupplierOfRandomness.onDemand(maxv = propValueRange),
      maxDepth = SupplierOfRandomness.onDemand(maxv = maxDepth), maxBranchingFactor = SupplierOfRandomness.onDemand(maxv = maxBranchingFactor),
      maxProperties = SupplierOfRandomness.onDemand(maxv = maxProperties), SupplierOfRandomness.randProbs(1).head
    )
    stateMachine.addNode(newInitNode)
    allNodes.sortBy(node => (stateMachine.outDegree(node), stateMachine.inDegree(node)))( Ordering.Tuple2(Ordering.Int.reverse, Ordering.Int)).take(connectedness).foreach {
      node =>
        if node != newInitNode then
          stateMachine.putEdgeValue(newInitNode, node, createAction(newInitNode, node))
          logger.info(s"Added edge from the init node to ${node.id}")
        else ()
    }
    logger.info(s"Linked the init node to $connectedness nodes")
    newInitNode
  end addInitState

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
  val outputDirectory: String = {
    val defDir = new java.io.File(".").getCanonicalPath
    logger.info(s"Default output directory: $defDir")
    val dir: String = getConfigEntry(NGSConstants.globalConfig, NGSConstants.OUTPUTDIRECTORY, defDir)
    val ref = new File(dir)
    if ref.exists() && ref.isDirectory then
      logger.info(s"Using output directory: $dir")
      if dir.endsWith("/") then dir else dir + "/"
    else
      logger.error(s"Output directory $dir does not exist or is not a directory, using current directory instead: $defDir")
      defDir
  }
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
  def apply(nodes: List[NodeObject], edges: List[Action]): Option[NetGraph] = new NetModel().generateModel(nodes, edges)

  def createAction(from: NodeObject, to: NodeObject): Action =
    val fCount = from.childrenCount
    val tCount = to.childrenCount
    val cost: Double = SupplierOfRandomness.randProbs(1).head
    require(cost >= 0 && cost <= 1)

    Action(SupplierOfRandomness.onDemand(maxv = actionRange),
      from.id,
      to.id,
      if fCount > 0 then SupplierOfRandomness.onDemand(maxv = fCount) else 0,
      if tCount > 0 then SupplierOfRandomness.onDemand(maxv = tCount) else 0,
      if SupplierOfRandomness.onDemand() % 2 == 0 then None else Some(SupplierOfRandomness.onDemand(maxv = propValueRange)),
      cost
    )

  //  each node of the graph is a NodeObject that corresponds to a GUI screen, which is a tree of GuiObjects
  @main def runNetModelAlgebra(args: String*): Unit =
    logger.info("File NetModelGenerator/src/main/scala/NetGraph/NetModelAlgebra.scala created at time 5:42 PM")
