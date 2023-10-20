package NetGraphAlgebraDefs

import NetGraphAlgebraDefs.NetModelAlgebra.{actionRange, connectedness, createAction, desiredReachabilityCoverage, edgeProbability, logger, maxBranchingFactor, maxDepth, maxProperties, propValueRange, statesTotal, valuableDataProbability}
import Randomizer.{SupplierOfRandomness, UniformProbGenerator}
import Utilz.ConfigReader.getConfigEntry
import Utilz.{CreateLogger, NGSConstants}
import Utilz.NGSConstants.{ACTIONRANGE, ACTIONRANGEDEFAULT, ACTIONTYPE, ACTIONTYPEDEFAULT, CONNECTEDNESS, CONNECTEDNESSDEFAULT, COSTOFDETECTION, COSTOFDETECTIONDEFAULT, DEFAULTDISSIMULATIONCOEFFICIENT, DEFAULTDISTANCECOEFFICIENT, DEFAULTDISTANCESPREADTHRESHOLD, DEFAULTEDGEPROBABILITY, DEFAULTPERTURBATIONCOEFFICIENT, DESIREDREACHABILITYCOVERAGE, DESIREDREACHABILITYCOVERAGEDEFAULT, DISSIMULATIONCOEFFICIENT, DISTANCECOEFFICIENT, DISTANCESPREADTHRESHOLD, EDGEPROBABILITY, GRAPHWALKNODETERMINATIONPROBABILITY, GRAPHWALKNODETERMINATIONPROBABILITYDEFAULT, GRAPHWALKTERMINATIONPOLICY, GRAPHWALKTERMINATIONPOLICYDEFAULT, MALAPPBUDGET, MALAPPBUDGETDEFAULT, MAXBRANCHINGFACTOR, MAXBRANCHINGFACTORDEFAULT, MAXDEPTH, MAXDEPTHDEFAULT, MAXPROPERTIES, MAXPROPERTIESDEFAULT, MAXWALKPATHLENGTHCOEFF, MAXWALKPATHLENGTHCOEFFDEFAULT, NUMBEROFEXPERIMENTS, NUMBEROFEXPERIMENTSDEFAULT, PERTURBATIONCOEFFICIENT, PROPVALUERANGE, PROPVALUERANGEDEFAULT, SEED, SERVICEPENALTY, SERVICEPENALTYDEFAULT, SERVICEREWARD, SERVICEREWARDDEFAULT, SERVICEREWARDPROBABILITY, SERVICEREWARDPROBABILITYDEFAULT, STATESTOTAL, STATESTOTALDEFAULT, TARGETAPPHIGHPENALTY, TARGETAPPHIGHPENALTYDEFAULT, TARGETAPPLOWPENALTY, TARGETAPPLOWPENALTYDEFAULT, TARGETAPPSCORE, TARGETAPPSCOREDEFAULT, VALUABLEDATAPROBABILITY, VALUABLEDATAPROBABILITYDEFAULT, WALKS, WALKSDEFAULT}
import com.google.common.graph.*
import org.slf4j.Logger

import java.io.File
import scala.annotation.tailrec
import scala.collection.immutable.TreeSeqMap.OrderBy
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Random, Success, Try}
import scala.collection.parallel.*
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParVector


type NetStateMachine = MutableValueGraph[NodeObject, Action]
class NetModel extends NetGraphConnectednessFinalizer:
  require(statesTotal > 0, "The total number of states must be positive")
  require(connectedness < statesTotal, "The total number of edges from the init node must be less than the number of states")
  require(maxBranchingFactor > 0, "The maximum branching factor must be greater than zero")
  require(maxDepth > 0, "The maximum depth must be greater than zero")
  require(maxProperties > 0, "The maximum number of properties must be greater than zero")
  require(propValueRange > 0, "The range of property values must be greater than zero")
  require(actionRange > 0, "The range of actions must be greater than zero")
  require(edgeProbability >= 0 && edgeProbability <= 1, "The edge probability must be between 0 and 1")
  require(connectedness >= 0 && connectedness <= statesTotal, "The connectedness must be between 0 and the total number of states")
  require(desiredReachabilityCoverage >= 0 && desiredReachabilityCoverage <= 1, "The desired reachability coverage must be between 0 and 1")

  private [this] val stateMachine: NetStateMachine = ValueGraphBuilder.directed().build()
  val modelUUID:String = java.util.UUID.randomUUID.toString

  private def createNodes(): Unit =
    val allNodes: ParSeq[NodeObject] = (1 to statesTotal).par.map { id =>
      NodeObject(
        id = id,
        children = SupplierOfRandomness.onDemandInt(pmaxv = maxBranchingFactor,repeatable = false),
        props = SupplierOfRandomness.onDemandInt(pmaxv = maxProperties, repeatable = false),
        propValueRange = SupplierOfRandomness.onDemandInt(pmaxv = propValueRange, repeatable = false),
        maxDepth = SupplierOfRandomness.onDemandInt(pmaxv = maxDepth, repeatable = false),
        maxBranchingFactor = SupplierOfRandomness.onDemandInt(pmaxv = maxBranchingFactor, repeatable = false),
        maxProperties = SupplierOfRandomness.onDemandInt(pmaxv = maxProperties, repeatable = false),
        storedValue = SupplierOfRandomness.onDemandReal(repeatable = false),
        valuableData = SupplierOfRandomness.`YesOrNo?`(valuableDataProbability)()
      )
    }
    logger.info(s"Inserting ${allNodes.size} created nodes into the graph")
    allNodes.toList.foreach( node => if !stateMachine.addNode(node) then logger.error(s"Could not add node with id ${node.id}") else ())

  def generateModel(nodes: List[NodeObject], edges: List[Action]): Option[NetGraph] =
    val sanityCheck = edges.foldLeft(true) {
      (acc, edge) =>
        val nodeFrom = nodes.find(_.id == edge.fromNode.id)
        val nodeTo = nodes.find(_.id == edge.toNode.id)
        if edge.toNode == edge.fromNode then logger.error(s"Edge from ${edge.fromId} to ${edge.toId} has the same node as source and target")
        if nodeFrom.isEmpty then logger.error(s"Could not find node with id ${edge.fromNode}")
        if nodeTo.isEmpty then logger.error(s"Could not find node with id ${edge.toNode}")
        if nodeFrom.isEmpty || nodeTo.isEmpty then false
        else acc
    } && nodes.exists(_.id == 0)

    if sanityCheck then
      nodes.foreach { node =>
        if !stateMachine.addNode(node) then logger.error(s"Could not add node with id ${node.id}")
        ()
      }
      edges.foreach { edge =>
        val nodeFrom: NodeObject = nodes.find(_.id == edge.fromNode.id).get
        val nodeTo: NodeObject = nodes.find(_.id == edge.toNode.id).get
        if nodeTo != nodeFrom then
          Try(stateMachine.putEdgeValue(nodeFrom, nodeTo, edge)) match
            case Failure(exception) => logger.error(s"Could not add edge from ${edge.fromId} to ${edge.toId} for reason ${exception.getMessage}")
            case Success(_) => ()
        else logger.error(s"Could not add edge from ${edge.fromId} to ${edge.toId} because the nodes are the same")
      }
      Some(NetGraph(stateMachine, nodes.find(_.id == 0).get))
    else None
  end generateModel

  def reportProgress(msg: String, total: Int, updateInterval: Int)(progress: Int = 0): Unit =
    if progress >= total then
      logger.info(s"$msg: 100%")
    else if progress % updateInterval == 0 then
      logger.info(s"$msg: ${progress * 100 / total}%")
    ()
  end reportProgress
  
  def generateModel(forceReachability: Boolean = false): Option[NetGraph] =
    logger.info(s"Generating a random graph with $statesTotal nodes and ${if forceReachability then s"ensuring ${desiredReachabilityCoverage*100}%" else "random"} reachability")
    createNodes()
    logger.info(s"Created ${stateMachine.nodes().size()} nodes")
    val allNodes: Vector[NodeObject] = stateMachine.nodes().asScala.toVector
    val nodesLength:Int = allNodes.length
    if nodesLength.toDouble >= math.sqrt(Long.MaxValue) then
      logger.error(s"Too many nodes to generate edges for: $nodesLength")
      None
    else
      val totalCombinationsOfNodes:Long = nodesLength * nodesLength
      val nodes4Edges: ParVector[(Int, Int)] = SupplierOfRandomness.randProbs(totalCombinationsOfNodes)().par.map(_ < edgeProbability).zipWithIndex.filter(_._1 == true).map(v => (v._2 / nodesLength, v._2 % nodesLength))
      logger.info(s"Ready to generate ${nodes4Edges.length} edge candidates")
      val edges2Add:ParVector[Action] = nodes4Edges.toVector.par.zipWithIndex.flatMap {
        case ((from, to), count) =>
          if from == to then None
          else
            //          node numbering starts from 1, since 0 is reserved for the init node
            val nodeFrom = allNodes(from)
            if nodeFrom.id - 1 != from then logger.error(s"Node from with id ${nodeFrom.id} has index $from")
            val nodeTo = allNodes(to)
            if nodeTo.id - 1 != to then logger.error(s"Node to with id ${nodeTo.id} has index $to")
            Some(createAction(nodeFrom, nodeTo))
      }
      logger.info(s"Generated ${nodes4Edges.length} edge candidates out of $totalCombinationsOfNodes possible combinations")
      edges2Add.zipWithIndex.foreach {
        (edge, index) =>
          logger.debug(s"Adding an edge from ${edge.fromNode.id} to ${edge.toNode.id}")
          stateMachine.putEdgeValue(edge.fromNode, edge.toNode, edge)
      }
      logger.info(s"Generated graph with ${stateMachine.nodes().size()} nodes and ${stateMachine.edges().size()} edges")
      val initState: NodeObject = addInitState(allNodes)
      val generatedGraph: NetGraph = NetGraph(stateMachine, initState)
      logger.info(s"Added init state connected with $connectedness nodes")
      if forceReachability then generatedGraph.forceReachability
      Some(generatedGraph)
  end generateModel

  def addInitState(allNodes: Vector[NodeObject]): NodeObject =
    val maxOutdegree = stateMachine.nodes().asScala.map(node => stateMachine.outDegree(node)).max
    val newInitNode: NodeObject = NodeObject(0, SupplierOfRandomness.onDemandInt(pmaxv = maxBranchingFactor),
      SupplierOfRandomness.onDemandInt(pmaxv = maxProperties), propValueRange = SupplierOfRandomness.onDemandInt(pmaxv = propValueRange),
      maxDepth = SupplierOfRandomness.onDemandInt(pmaxv = maxDepth), maxBranchingFactor = SupplierOfRandomness.onDemandInt(pmaxv = maxBranchingFactor),
      maxProperties = SupplierOfRandomness.onDemandInt(pmaxv = maxProperties), SupplierOfRandomness.onDemandReal(),
      valuableData = SupplierOfRandomness.`YesOrNo?`(valuableDataProbability)()
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

  val distanceSpreadThreshold: Double = getConfigEntry(NGSConstants.configNetGameModel, DISTANCESPREADTHRESHOLD, DEFAULTDISTANCESPREADTHRESHOLD)
  val dissimulationCoefficient: Double = getConfigEntry(NGSConstants.configNetGameModel, DISSIMULATIONCOEFFICIENT, DEFAULTDISSIMULATIONCOEFFICIENT)
  val perturbationCoeff: Double = getConfigEntry(NGSConstants.configNetGameModel, PERTURBATIONCOEFFICIENT, DEFAULTPERTURBATIONCOEFFICIENT)
  val distanceCoeff: Double = getConfigEntry(NGSConstants.configNetGameModel, DISTANCECOEFFICIENT, DEFAULTDISTANCECOEFFICIENT)
  val edgeProbability: Double = getConfigEntry(NGSConstants.configNetGameModel,EDGEPROBABILITY, DEFAULTEDGEPROBABILITY)
  val numberOfExperiments: Int = getConfigEntry(NGSConstants.configNetGameModel,NUMBEROFEXPERIMENTS, NUMBEROFEXPERIMENTSDEFAULT)
  val statesTotal: Int = getConfigEntry(NGSConstants.configNetGameModel,STATESTOTAL, STATESTOTALDEFAULT)
  val desiredReachabilityCoverage: Double = getConfigEntry(NGSConstants.configNetGameModel,DESIREDREACHABILITYCOVERAGE, DESIREDREACHABILITYCOVERAGEDEFAULT)
  val numberOfWalks: Int = getConfigEntry(NGSConstants.configNetGameModel,WALKS, WALKSDEFAULT)
  val maxBranchingFactor: Int = getConfigEntry(NGSConstants.configNetGameModel,MAXBRANCHINGFACTOR, MAXBRANCHINGFACTORDEFAULT)
  val maxDepth: Int = getConfigEntry(NGSConstants.configNetGameModel,MAXDEPTH, MAXDEPTHDEFAULT)
  val maxProperties: Int = getConfigEntry(NGSConstants.configNetGameModel,MAXPROPERTIES, MAXPROPERTIESDEFAULT)
  val propValueRange: Int = getConfigEntry(NGSConstants.configNetGameModel,PROPVALUERANGE, PROPVALUERANGEDEFAULT)
  val actionRange: Int = getConfigEntry(NGSConstants.configNetGameModel,ACTIONRANGE, ACTIONRANGEDEFAULT)
  val actionType: Int = getConfigEntry(NGSConstants.configNetGameModel,ACTIONTYPE, ACTIONTYPEDEFAULT)
  val connectedness: Int = getConfigEntry(NGSConstants.configNetGameModel,CONNECTEDNESS, CONNECTEDNESSDEFAULT)
  val maxWalkPathLengthCoeff: Double = getConfigEntry(NGSConstants.configNetGameModel,MAXWALKPATHLENGTHCOEFF, MAXWALKPATHLENGTHCOEFFDEFAULT)
  val graphWalkTerminationPolicy: String = getConfigEntry(NGSConstants.configNetGameModel,GRAPHWALKTERMINATIONPOLICY, GRAPHWALKTERMINATIONPOLICYDEFAULT)
  val graphWalkNodeTerminationProbability: Double = getConfigEntry(NGSConstants.configNetGameModel,GRAPHWALKNODETERMINATIONPROBABILITY, GRAPHWALKNODETERMINATIONPROBABILITYDEFAULT)
  val valuableDataProbability: Double = getConfigEntry(NGSConstants.configNetGameModel,VALUABLEDATAPROBABILITY, VALUABLEDATAPROBABILITYDEFAULT)
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

  def apply(forceLinkOrphans: Boolean = true): Option[NetGraph] = new NetModel().generateModel(forceLinkOrphans)
  def apply(nodes: List[NodeObject], edges: List[Action]): Option[NetGraph] = new NetModel().generateModel(nodes, edges)

  def createAction(from: NodeObject, to: NodeObject): Action =
    val fCount = from.childrenCount
    val tCount = to.childrenCount
    val cost: Double = SupplierOfRandomness.randProbs(1)().head
    require(cost >= 0 && cost <= 1)
    Action(SupplierOfRandomness.onDemandInt(pmaxv = actionType, repeatable = false),
      from,
      to,
      if fCount > 0 then SupplierOfRandomness.onDemandInt(pmaxv = fCount, repeatable = false) else 0,
      if tCount > 0 then SupplierOfRandomness.onDemandInt(pmaxv = tCount, repeatable = false) else 0,
      if SupplierOfRandomness.onDemandInt(repeatable = false) % 2 == 0 then None else Some(SupplierOfRandomness.onDemandInt(pmaxv = propValueRange, repeatable = false)),
      cost
    )
  //  each node of the graph is a NodeObject that corresponds to some physical entity, which is a tree of some objects
  @main def runNetModelAlgebra(args: String*): Unit =
    logger.info("File NetModelGenerator/src/main/scala/NetGraph/NetModelAlgebra.scala created at time 5:42 PM")
