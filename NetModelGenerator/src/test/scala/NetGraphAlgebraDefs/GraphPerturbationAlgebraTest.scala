package NetGraphAlgebraDefs

import NetGraphAlgebraDefs.NetModelAlgebra.{mapAppBudget, targetAppScore}
import NetGraphAlgebraDefs.GraphPerturbationAlgebra.{EdgeAdded, EdgeRemoved, ModificationRecord, NodeAdded, NodeModified, NodeRemoved, OriginalNetComponent, inverseMR}
import NetGraphAlgebraDefs.GraphPerturbationAlgebraTest.{ADDEDGEMETHOD, ADDNODEMETHOD, MODIFYEDGEMETHOD, MODIFYNODEMETHOD, REMOVEEDGEMETHOD, REMOVENODEMETHOD}
import NetModelAnalyzer.Budget.{MalAppBudget, TargetAppScore}
import NetModelAnalyzer.{CostRewardCalculator, PATHRESULT, RandomWalker}
import Randomizer.SupplierOfRandomness
import Utilz.ConfigReader.getConfigEntry
import Utilz.CreateLogger
import Utilz.NGSConstants.{DEFAULTEDGEPROBABILITY, EDGEPROBABILITY}
import com.google.common.graph.{MutableValueGraph, ValueGraphBuilder}
import org.mockito.Mockito.{mock, when}
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.scalatest.prop.TableDrivenPropertyChecks
import org.slf4j.Logger

object GraphPerturbationAlgebraTest:
  val ADDNODEMETHOD = "addNode"
  val REMOVENODEMETHOD = "removeNode"
  val MODIFYNODEMETHOD = "modifyNode"
  val ADDEDGEMETHOD = "addEdge"
  val REMOVEEDGEMETHOD = "removeEdge"
  val MODIFYEDGEMETHOD = "modifyEdge"

class GraphPerturbationAlgebraTest extends AnyFlatSpec with Matchers with MockitoSugar with TableDrivenPropertyChecks with PrivateMethodTester {
  val logger: Logger = CreateLogger(this.getClass)

  val node1: NodeObject = NodeObject(id = 1, children = 5, props = 10, propValueRange = 20, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10,1)
  val node2: NodeObject = NodeObject(id = 2, children = 5, props = 10, propValueRange = 20, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10,1)
  val node3: NodeObject = NodeObject(id = 3, children = 5, props = 10, propValueRange = 20, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10,1)
  val edge12: Action = Action(actionType = 1, node1, node2, fromId = 1, toId = 2, resultingValue = Some(12), cost = 0.12)
  val edge23: Action = Action(actionType = 2, node2, node3, fromId = 2, toId = 3, resultingValue = Some(23), cost = 0.23)

  val graphTypes = Table(
    ("graphType", "createGraphFunction"),
    ("directed", () => createTestGraph()),
    ("undirected", () => createTestGraph(isDirected = false))
  )
  def createTestGraph(isDirected: Boolean = true): NetGraph = {
    val graphBuilder = if (isDirected) ValueGraphBuilder.directed() else ValueGraphBuilder.undirected()
    val graph1: MutableValueGraph[NodeObject, Action] = graphBuilder.build()

    if !graph1.addNode(node1) then
      logger.error(s"Node $node1 already exists")
    if !graph1.addNode(node2) then
      logger.error(s"Node $node2 already exists")
    if !graph1.addNode(node3) then
      logger.error(s"Node $node3 already exists")
    graph1.putEdgeValue(node1, node2, edge12)
    graph1.putEdgeValue(node2, node3, edge23)
    NetGraph(graph1, node1)
  }
  forAll(graphTypes) { (graphType, createGraph) =>

    behavior of s"Net graph perturbation operations on $graphType graphs"

    it should s"add a new node to the $graphType graph" in {
      val graph = createGraph()
      val algebra = new GraphPerturbationAlgebra(graph)
      val theFunc = PrivateMethod[ModificationRecord](Symbol(ADDNODEMETHOD))
      val modificationRecord: ModificationRecord = algebra invokePrivate theFunc(node3)
      logger.info(modificationRecord.toString)
      logger.info(graph.sm.toString)
      modificationRecord.size shouldBe 2
      val invMR = inverseMR(modificationRecord)
      logger.info(s"Inverse MR: ${invMR.toString}")
      val res: Option[(OriginalNetComponent, GraphPerturbationAlgebra.Perturbation)] = modificationRecord.find(_._1 == OriginalNetComponent(node3))
      res.get._1.node shouldBe node3
    }

    it should s"remove a node from the $graphType graph" in {
      val graph = createGraph()
      val algebra = new GraphPerturbationAlgebra(graph)
      val theFunc = PrivateMethod[ModificationRecord](Symbol(REMOVENODEMETHOD))
      val modificationRecord: ModificationRecord = algebra invokePrivate theFunc(node1)
      logger.info(modificationRecord.toString)
      logger.info(graph.sm.toString)
      graph.sm.nodes().size shouldBe 2
      if (graphType == "directed") {
        modificationRecord shouldBe Vector((OriginalNetComponent(NodeObject(1, 5, 10, 1, 20, 5, 5, 10, 1.0)), NodeRemoved(NodeObject(1, 5, 10, 1, 20, 5, 5, 10, 1.0))), (OriginalNetComponent(NodeObject(1, 5, 10, 1, 20, 5, 5, 10, 1.0)), EdgeRemoved(Action(1, NodeObject(1, 5, 10, 1, 20, 5, 5, 10, 1.0), NodeObject(2, 5, 10, 1, 20, 5, 5, 10, 1.0), 1, 2, Some(12), 0.12))))
      } else {
        modificationRecord shouldBe Vector((OriginalNetComponent(NodeObject(1,5,10,1,20,5,5,10,1.0,false)),NodeRemoved(NodeObject(1,5,10,1,20,5,5,10,1.0,false))), (OriginalNetComponent(NodeObject(1,5,10,1,20,5,5,10,1.0,false)),EdgeRemoved(Action(1,NodeObject(1,5,10,1,20,5,5,10,1.0,false),NodeObject(2,5,10,1,20,5,5,10,1.0,false),1,2,Some(12),0.12))), (OriginalNetComponent(NodeObject(1,5,10,1,20,5,5,10,1.0,false)),EdgeRemoved(Action(1,NodeObject(1,5,10,1,20,5,5,10,1.0,false),NodeObject(2,5,10,1,20,5,5,10,1.0,false),1,2,Some(12),0.12))))
      }
      val invMR = inverseMR(modificationRecord)
      logger.info(s"Inverse MR: ${invMR.toString}")
    }

    it should s"modify a node from the $graphType graph" in {
      val graph = createGraph()
      val algebra = new GraphPerturbationAlgebra(graph)
      val theFunc = PrivateMethod[ModificationRecord](Symbol(MODIFYNODEMETHOD))
      val modificationRecord: ModificationRecord = algebra invokePrivate theFunc(node2)
      logger.info(modificationRecord.toString)
      logger.info(graph.sm.toString)
      graph.sm.nodes().size shouldBe 3

      modificationRecord(0)._1.node shouldBe node2

      val invMR = inverseMR(modificationRecord)
      logger.info(s"Inverse MR: ${invMR.toString}")
    }

    it should s"add an edge to the $graphType graph" in {
      val graph = createGraph()
      val algebra = new GraphPerturbationAlgebra(graph)
      val theFunc = PrivateMethod[ModificationRecord](Symbol(ADDEDGEMETHOD))
      graph.sm.hasEdgeConnecting(node3, node1) shouldBe false
      graph.sm.edges().size shouldBe 2
      val modificationRecord: ModificationRecord = algebra invokePrivate theFunc(node3, node1)
      logger.info(modificationRecord.toString)
      logger.info(graph.sm.toString)
      graph.sm.edges().size shouldBe 3
      graph.sm.hasEdgeConnecting(node3, node1) shouldBe true
      modificationRecord(0)._1.node shouldBe NodeObject(3, 5, 10, 1, 20, 5, 5, 10, 1)

      val invMR = inverseMR(modificationRecord)
      logger.info(s"Inverse MR: ${invMR.toString}")

    }

    it should s"remove an edge from the $graphType graph" in {
      val graph = createGraph()
      val algebra = new GraphPerturbationAlgebra(graph)
      val theFunc = PrivateMethod[ModificationRecord](Symbol(REMOVEEDGEMETHOD))
      graph.sm.hasEdgeConnecting(node2, node3) shouldBe true
      graph.sm.edges().size shouldBe 2
      val modificationRecord: ModificationRecord = algebra invokePrivate theFunc(node2, node3)
      logger.info(modificationRecord.toString)
      logger.info(graph.sm.toString)
      graph.sm.edges().size shouldBe 1
      graph.sm.hasEdgeConnecting(node2, node3) shouldBe false
      modificationRecord(0)._1.node shouldBe NodeObject(2, 5, 10, 1, 20, 5, 5, 10, 1)

      val invMR = inverseMR(modificationRecord)
      logger.info(s"Inverse MR: ${invMR.toString}")
    }

    it should s"modify an edge in the $graphType graph" in {
      val graph = createGraph()
      val algebra = new GraphPerturbationAlgebra(graph)
      val theFunc = PrivateMethod[ModificationRecord](Symbol(MODIFYEDGEMETHOD))
      graph.sm.hasEdgeConnecting(node2, node3) shouldBe true
      val oldEdge = graph.sm.edgeValue(node2, node3).get
      graph.sm.edges().size shouldBe 2
      val modificationRecord: ModificationRecord = algebra invokePrivate theFunc(node2, node3)
      logger.info(modificationRecord.toString)
      logger.info(graph.sm.toString)
      graph.sm.edges().size shouldBe 2
      graph.sm.hasEdgeConnecting(node2, node3) shouldBe true
      val newEdge = graph.sm.edgeValue(node2, node3).get
      modificationRecord(0)._1.node shouldBe NodeObject(2, 5, 10, 1, 20, 5, 5, 10, 1)
      oldEdge should not be newEdge

      val invMR = inverseMR(modificationRecord)
      logger.info(s"Inverse MR: ${invMR.toString}")
    }

    it should s"compute the costs and rewards for a walk with a modified a node in $graphType graph" in {
      val graph = createGraph()
      logger.info(s"Original graph: ${graph.sm.toString}")
      val algebra = new GraphPerturbationAlgebra(graph)
      val theFunc = PrivateMethod[ModificationRecord](Symbol(MODIFYNODEMETHOD))
      val modificationRecord: ModificationRecord = algebra invokePrivate theFunc(node2)
      logger.info(s"Modified graph: ${graph.sm.toString}")
      val invMR = inverseMR(modificationRecord)
      logger.info(s"Inverse MR: ${invMR.toString}")
      val walker = RandomWalker(graph)
      val walk: PATHRESULT = walker.walk().head
      logger.info(s"Original walk: ${walk.toString}")
      val resCosts = CostRewardCalculator(walk, invMR, List())(MalAppBudget(mapAppBudget), TargetAppScore(targetAppScore))
      resCosts._1._1.toDouble should be >= 110d
      resCosts._1._2.toDouble should be <= 200.2d
    }

    it should s"verify edge directionality in $graphType graph" in {
      val graph = createGraph()
      if (graphType == "directed") {
        graph.sm.hasEdgeConnecting(node1, node2) shouldBe true
        graph.sm.hasEdgeConnecting(node2, node1) shouldBe false
      } else {
        graph.sm.hasEdgeConnecting(node1, node2) shouldBe true
        graph.sm.hasEdgeConnecting(node2, node1) shouldBe true
      }
    }
  }
}
