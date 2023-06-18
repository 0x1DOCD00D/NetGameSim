/*
 * Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package NetModelAnalyzer

import NetGraphAlgebraDefs.GraphPerturbationAlgebra.{EdgeRemoved, ModificationRecord, NodeModified, NodeRemoved, OriginalNetComponent}
import NetGraphAlgebraDefs.GraphPerturbationAlgebraTest.{MODIFYNODEMETHOD, REMOVENODEMETHOD}
import NetGraphAlgebraDefs.{NetGraph, NetGraphComponent, NetModelAlgebra, GraphPerturbationAlgebra, NodeObject, TerminalNode}
import NetGraphAlgebraDefs.Action
import Randomizer.SupplierOfRandomness
import Utilz.ConfigReader.getConfigEntry
import Utilz.CreateLogger
import Utilz.NGSConstants.*

import scala.jdk.CollectionConverters.*
import com.google.common.graph.{MutableValueGraph, ValueGraphBuilder}
import org.mockito.Mockito.{mock, when}
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.slf4j.Logger

import scala.collection.immutable.ListMap
import scala.util.{Failure, Success, Try}

//                            +-----------------------------------------------+
//                            |                                               |
//                        .---v---.            .-------.                      |
//                       / +----+  \          / +----+  \                     |
//                 +--->(  | 2  |   ---+---->(  | 5  |   )-----------+        |
//                 |     `.+----+ ,'   |      `.+----+ ,'            |        |
//                 |       `-----'     |        `-----'              |        |
//                 |                   |           ^                 |        |
//                 |         +---------+-----------+                 |        |
//                 |         |         |                             |        |
//   .-------.     |     .-------.     |      .-------.          .---v---.    |       .-------.
//  / +----+  \    |    / +----+  \    |     / +----+  \        / +----+  \   |      / +----+  \
// (  | 1  |   ---++-->(  | 3  |   )   +--->(  | 6  |   )----->(  | 8  |   )--+---->(  | 9  |   )
//  `.+----+ ,'   |     `.+----+ ,'          `.+----+ ,'        `.+----+ ,'   |      `.+----+ ,'
//    `-----'     |       `--+--'              `-----'            `--^--'     |        `--^--'
//                |          +--------------------+                  |        |           |
//                |                               |                  |        |           |
//                |      .-------.            .---v---.              |        |           |
//                |     / +----+  \          / +----+  \             |        |           |
//                +--->(  | 4  |   )------->(  | 7  |   )------------+--------+-----------+
//                      `.+----+ ,'          `.+----+ ,'                      |
//                        `--^--'              `-----'                        |
//                           |                                                |
//                           +------------------------------------------------+
class RandomWalkerTest extends AnyFlatSpec with Matchers with MockitoSugar with PrivateMethodTester {
  val logger: Logger = CreateLogger(this.getClass)
  logger.info(NetModelAlgebra.getFields.mkString(","))
  val node1: NodeObject = NodeObject(id = 1, children = 1, props = 1, propValueRange = 1, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10)
  val node2: NodeObject = NodeObject(id = 2, children = 2, props = 2, propValueRange = 2, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10)
  val node3: NodeObject = NodeObject(id = 3, children = 3, props = 3, propValueRange = 3, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10)
  val node4: NodeObject = NodeObject(id = 4, children = 4, props = 4, propValueRange = 4, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10)
  val node5: NodeObject = NodeObject(id = 5, children = 5, props = 5, propValueRange = 5, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10)
  val node6: NodeObject = NodeObject(id = 6, children = 6, props = 6, propValueRange = 6, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10)
  val node7: NodeObject = NodeObject(id = 7, children = 7, props = 7, propValueRange = 7, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10)
  val node8: NodeObject = NodeObject(id = 8, children = 8, props = 8, propValueRange = 8, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10)
  val node9: NodeObject = NodeObject(id = 9, children = 8, props = 8, propValueRange = 9, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10)

  val edge12: Action = Action(actionType = 1, fromId = 1, toId = 2, resultingValue = Some(1), cost = 0.1)
  val edge13: Action = Action(actionType = 2, fromId = 1, toId = 3, resultingValue = Some(2), cost = 0.2)
  val edge14: Action = Action(actionType = 3, fromId = 1, toId = 4, resultingValue = Some(3), cost = 0.3)
  val edge25: Action = Action(actionType = 4, fromId = 2, toId = 5, resultingValue = Some(4), cost = 0.4)
  val edge26: Action = Action(actionType = 5, fromId = 2, toId = 6, resultingValue = Some(5), cost = 0.5)
  val edge35: Action = Action(actionType = 6, fromId = 3, toId = 5, resultingValue = Some(6), cost = 0.6)
  val edge37: Action = Action(actionType = 7, fromId = 3, toId = 7, resultingValue = Some(7), cost = 0.7)
  val edge47: Action = Action(actionType = 8, fromId = 4, toId = 7, resultingValue = Some(8), cost = 0.8)
  val edge58: Action = Action(actionType = 9, fromId = 5, toId = 8, resultingValue = Some(9), cost = 0.9)
  val edge68: Action = Action(actionType = 10, fromId = 6, toId = 8, resultingValue = Some(10), cost = 0.91)
  val edge78: Action = Action(actionType = 11, fromId = 7, toId = 8, resultingValue = Some(11), cost = 0.92)
  val edge79: Action = Action(actionType = 12, fromId = 7, toId = 9, resultingValue = Some(12), cost = 0.93)
  val edge82: Action = Action(actionType = 13, fromId = 8, toId = 2, resultingValue = Some(13), cost = 0.94)
  val edge84: Action = Action(actionType = 14, fromId = 8, toId = 4, resultingValue = Some(14), cost = 0.95)
  val edge89: Action = Action(actionType = 15, fromId = 8, toId = 9, resultingValue = Some(15), cost = 0.96)

  def createTestGraph(): NetGraph = {
    val graph: MutableValueGraph[NodeObject, Action] = ValueGraphBuilder.directed().build()

    def addnode(node: NodeObject): Unit = {
      if !graph.addNode(node) then
        logger.error(s"Node $node already exists")
      else logger.debug(s"Added node $node")
    }

    def addedge(from: NodeObject, to: NodeObject, edge: Action): Unit = {
      Try(graph.putEdgeValue(from, to, edge)) match
      case Success(_) => logger.debug(s"Added edge $edge between nodes $from -> $to")
      case Failure(e) => logger.error(s"Edge $from -> $to cannot be added because $e")
    }

    addnode(node1)
    addnode(node2)
    addnode(node3)
    addnode(node4)
    addnode(node5)
    addnode(node6)
    addnode(node7)
    addnode(node8)
    addnode(node9)

    addedge(node1, node2, edge12)
    addedge(node1, node3, edge13)
    addedge(node1, node4, edge14)
    addedge(node2, node5, edge25)
    addedge(node2, node6, edge26)
    addedge(node3, node5, edge35)
    addedge(node3, node7, edge37)
    addedge(node4, node7, edge47)
    addedge(node5, node8, edge58)
    addedge(node6, node8, edge68)
    addedge(node7, node8, edge78)
    addedge(node7, node9, edge79)
    addedge(node8, node2, edge82)
    addedge(node8, node4, edge84)
    addedge(node8, node9, edge89)
    NetGraph(graph, node1)
  }

  def test4Cycles(lst: List[Int]): Boolean =
    lst match
    case Nil => false
    case x :: xs => xs.contains(x) | test4Cycles(xs)

  behavior of "the random walk on the net graph"

  it should "create a graph for random walks" in {
    val graph = createTestGraph()
    graph shouldBe a[NetGraph]
  }

  it should "perform five random walks" in {
    val graph = createTestGraph()
    val walker = RandomWalker(graph)
    val walks = walker.walk(5)
//    walks.foreach(walk => logger.info(s"Walk: ${graph.initState :: walk}"))
    val walkNodeNumbers: List[List[Int]] = walks.map(walk => walk.map {
      case node: STEPRESULT => node._1.asInstanceOf[NodeObject].id
      case null => assert(false); -1
    }
    )
    walkNodeNumbers.foreach(walk => logger.info(s"Walk: ${graph.initState.id :: walk}"))
    walkNodeNumbers.length shouldBe 5

    walkNodeNumbers.foreach(walk => if test4Cycles(walk) then logger.info(s"Cycles found in the walk: $walk"))
    val pathLengths: List[Int] = walkNodeNumbers.map(_.length)
    pathLengths.filter(_ > walker.maxWalkPathLength+1) shouldBe Nil
  }

  it should "find cycles in fifty random walks" in {
    val graph = createTestGraph()
    val walker = RandomWalker(graph)
    val walks = walker.walk(50)
//    walks.foreach(walk => logger.info(s"Walk: ${graph.initState :: walk}"))
    val walkNodeNumbers: List[List[Int]] = walks.map(walk => walk.map {
      case node: STEPRESULT => node._1.asInstanceOf[NodeObject].id
      case null => assert(false); -1
    }
    )
    walkNodeNumbers.foreach(walk => logger.info(s"Walk: ${graph.initState.id :: walk}"))
    walkNodeNumbers.length shouldBe 50

    val noCyclesWalks = walkNodeNumbers.filter(!test4Cycles(_))
//    logger.info(s"Walks without cycles: $noCyclesWalks")
    val pathLengths: List[Int] = noCyclesWalks.map(_.length)
    pathLengths.filter(_ > 6) shouldBe Nil
  }

  it should "avoid cycles in fifty random walks" in {
    val graph = createTestGraph()
    val walker = RandomWalker(graph, Some("untilcycle"))
    val walks = walker.walk(50)
//    walks.foreach(walk => logger.info(s"Walk: ${graph.initState :: walk}"))
    val walkNodeNumbers: List[List[Int]] = walks.map(walk => walk.map {
      case node: STEPRESULT => node._1.asInstanceOf[NodeObject].id
      case null => assert(false); -1
    }
    )
    walkNodeNumbers.foreach(walk => logger.info(s"Walk: ${graph.initState.id :: walk}"))
    walkNodeNumbers.length shouldBe 50

    val cycles = walkNodeNumbers.filter(walk => test4Cycles(walk))
    cycles.foreach(walk => logger.info(s"Walk with cycles: ${graph.initState.id :: walk}"))
    cycles.length shouldBe 0
  }

  it should "avoid cycles and max path length overrun in one thousand random walks" in {
    val graph = createTestGraph()
    val walker = RandomWalker(graph, Some("all"))
    val walks = walker.walk(1000)
    //    walks.foreach(walk => logger.info(s"Walk: ${graph.initState :: walk}"))
    val walkNodeNumbers: List[List[Int]] = walks.map(walk => walk.map {
      case node: STEPRESULT => node._1.asInstanceOf[NodeObject].id
      case null => assert(false); -1
    }
    )
//    walkNodeNumbers.foreach(walk => logger.info(s"Walk: ${graph.initState.id :: walk}"))
    walkNodeNumbers.length shouldBe 1000

    val cycles = walkNodeNumbers.filter(walk => test4Cycles(walk))
    cycles.foreach(walk => logger.info(s"Walk with cycles: ${graph.initState.id :: walk}"))
    cycles.length shouldBe 0

    val pathLengths: List[Int] = walkNodeNumbers.map(_.length)
    pathLengths.filter(_ > walker.maxWalkPathLength+1) shouldBe Nil

  }

  it should "create a perturbed graph" in {
    val graph = createTestGraph()
    val perturbedGraph = GraphPerturbationAlgebra(graph)
    logger.info(s"Perturbed graph: ${perturbedGraph._2}")
    perturbedGraph._1.sm.nodes().size() shouldBe graph.sm.nodes().size()
    perturbedGraph._2.length should be >= 1
  }

  it should "create a perturbed graph with modified node 8" in {
    val graph = createTestGraph()
    val algebra = new GraphPerturbationAlgebra(graph)
    val theFunc = PrivateMethod[ModificationRecord](Symbol(MODIFYNODEMETHOD))
    val modificationRecord: ModificationRecord = algebra invokePrivate theFunc(node8)
    graph.sm.nodes().size shouldBe 9

    logger.info(modificationRecord(0)._2.toString)
    modificationRecord(0)._1.node shouldBe node8
    modificationRecord(0)._2.asInstanceOf[NodeModified].node should not equal node8
  }

  it should "create a perturbed graph with node 8 removed and all of its connected edges" in {
    val graph = createTestGraph()
    val algebra = new GraphPerturbationAlgebra(graph)
    val theFunc = PrivateMethod[ModificationRecord](Symbol(REMOVENODEMETHOD))
    val modificationRecord: ModificationRecord = algebra invokePrivate theFunc(node8)
    logger.info(modificationRecord.toString)
    logger.info(graph.sm.toString)
    graph.sm.nodes().size shouldBe 8

    logger.info(modificationRecord.toString())
    modificationRecord(0)._2.asInstanceOf[NodeRemoved].node shouldBe node8
  }

  it should "compute stats for different numbers of random walks" in {
    val graph = createTestGraph()
    val walker = RandomWalker(graph)
    val walks_1 = walker.walk()
    val walks_10 = walker.walk(10)

    walks_1.foreach(walk => logger.info(s"Walk 1: ${graph.initState.id :: walk}"))
    val stats_1 = new WalkingStats(graph, walks_1)
    logger.info(s"Stats for 1 walks: ${stats_1.graphCoverage()}")
    logger.info(s"Stats for 1 walks: ${stats_1.coveragePercentages}")

    walks_10.foreach(walk => logger.info(s"Walk 10: ${graph.initState.id :: walk}"))
    val stats_10 = new WalkingStats(graph, walks_10)
    logger.info(s"Stats for 10 walks: ${stats_10.graphCoverage()}")
    logger.info(s"Stats for 10 walks: ${stats_10.coveragePercentages}")
    stats_10.coveragePercentages(0) should be > stats_1.coveragePercentages(0)
  }

  it should "determine the path estimate for the remaining graph coverage" in {
    val graph = createTestGraph()
    val walker = RandomWalker(graph)
    val walks_5 = walker.walk(5)
    walks_5.foreach(walk => logger.info(s"Walk 5: ${graph.initState.id :: walk}"))
    val stats = new WalkingStats(graph, walks_5)
    val sorted = stats.graphCoverage().toSeq.filter(e => e._1.isInstanceOf[NodeObject] && e._2 > 0).sortBy(_._2).map(_._1.asInstanceOf[NodeObject].id).toSet
    val all = graph.sm.nodes().asScala.map(_.id).toSet
    logger.info(s"Sorted walk 5: $sorted")
    logger.info(s"Uncovered nodes: ${all -- sorted}")
    val pe = PathsEstimator(graph)
    val estimate: List[SLICEOFCOMPONENTPIE] = pe.exploreThesePaths(walks_5, 3)
    estimate.flatten.filter(e=> e.isInstanceOf[NodeObject]).map(e=> e.asInstanceOf[NodeObject].id).toSet shouldBe (all -- sorted)
    logger.info(s"Estimate: $estimate")
  }
}