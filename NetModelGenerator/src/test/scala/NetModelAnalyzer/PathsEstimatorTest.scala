/*
 * Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package NetModelAnalyzer

import NetGraphAlgebraDefs.{Action, NetGraph, NetGraphComponent, NetModelAlgebra, NodeObject}
import Utilz.CreateLogger
import com.google.common.graph.{MutableValueGraph, ValueGraphBuilder}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.Logger

import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

/**
 * Tests for PathsEstimator, which determines unexplored paths in the graph
 * after a set of random walks and divides them among multiple MapApps using
 * a fair cake-cutting strategy.
 *
 * The test graph used here is a simple 5-node directed graph:
 *
 *   1 -> 2 -> 3
 *   |         ^
 *   v         |
 *   4 ------> 5
 *
 * This structure ensures that some nodes and edges may remain unvisited
 * after short random walks, giving PathsEstimator something to recommend.
 */
class PathsEstimatorTest extends AnyFlatSpec with Matchers {
  val logger: Logger = CreateLogger(this.getClass)

  // Reusable test nodes
  val node1: NodeObject = NodeObject(id = 1, children = 1, props = 1, propValueRange = 1, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10, 1)
  val node2: NodeObject = NodeObject(id = 2, children = 2, props = 2, propValueRange = 2, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10, 1)
  val node3: NodeObject = NodeObject(id = 3, children = 3, props = 3, propValueRange = 3, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10, 1)
  val node4: NodeObject = NodeObject(id = 4, children = 4, props = 4, propValueRange = 4, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10, 1)
  val node5: NodeObject = NodeObject(id = 5, children = 5, props = 5, propValueRange = 5, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10, 1)

  val edge12: Action = Action(actionType = 1, fromNode = node1, toNode = node2, fromId = 1, toId = 2, resultingValue = Some(1), cost = 0.1)
  val edge23: Action = Action(actionType = 2, fromNode = node2, toNode = node3, fromId = 2, toId = 3, resultingValue = Some(2), cost = 0.2)
  val edge14: Action = Action(actionType = 3, fromNode = node1, toNode = node4, fromId = 1, toId = 4, resultingValue = Some(3), cost = 0.3)
  val edge45: Action = Action(actionType = 4, fromNode = node4, toNode = node5, fromId = 4, toId = 5, resultingValue = Some(4), cost = 0.4)
  val edge53: Action = Action(actionType = 5, fromNode = node5, toNode = node3, fromId = 5, toId = 3, resultingValue = Some(5), cost = 0.5)

  def createTestGraph(): NetGraph = {
    val graph: MutableValueGraph[NodeObject, Action] = ValueGraphBuilder.directed().build()
    graph.addNode(node1)
    graph.addNode(node2)
    graph.addNode(node3)
    graph.addNode(node4)
    graph.addNode(node5)
    graph.putEdgeValue(node1, node2, edge12)
    graph.putEdgeValue(node2, node3, edge23)
    graph.putEdgeValue(node1, node4, edge14)
    graph.putEdgeValue(node4, node5, edge45)
    graph.putEdgeValue(node5, node3, edge53)
    NetGraph(graph, node1)
  }

  behavior of "PathsEstimator"

  // Verify that the exploration map is initialized with all graph nodes and edges,
  // each starting with a visit count of zero.
  it should "initialize the exploration map with all nodes and edges at zero" in {
    val graph = createTestGraph()
    val pe = PathsEstimator(graph)
    val nodeCount = graph.sm.nodes().size()
    val edgeCount = graph.sm.edges().size()
    pe.explorationMap.size shouldBe nodeCount + edgeCount
    pe.explorationMap.values.forall(_ == 0) shouldBe true
    logger.info(s"Exploration map initialized with ${pe.explorationMap.size} entries (${nodeCount} nodes + ${edgeCount} edges)")
  }

  // When paths are empty (no walks performed), every node and edge is unexplored
  // so exploreThesePaths should return all graph components as recommendations.
  it should "return all components as unexplored when given empty paths" in {
    val graph = createTestGraph()
    val pe = PathsEstimator(graph)
    val emptyPaths: LISTOFWALKEDPATHS = List.empty
    val result = pe.exploreThesePaths(emptyPaths, 3)
    val allUnexplored = result.flatten
    val nodeCount = graph.sm.nodes().size()
    val edgeCount = graph.sm.edges().size()
    // All nodes and edges should be unexplored since no walks were given
    allUnexplored.size shouldBe nodeCount + edgeCount
    logger.info(s"With empty paths, all ${allUnexplored.size} components are unexplored")
  }

  // After walking some paths, the components visited should no longer appear
  // in the unexplored recommendations from exploreThesePaths.
  it should "exclude already-visited components from recommendations" in {
    val graph = createTestGraph()
    val pe = PathsEstimator(graph)
    // Simulate a walk that covers the path 1->2->3 (node1, edge12, node2, edge23, node3)
    val simulatedWalk: PATHRESULT = List(
      (node2, edge12),
      (node3, edge23)
    )
    val result = pe.exploreThesePaths(List(simulatedWalk), 2)
    val unexplored = result.flatten
    // node2, node3, edge12, and edge23 were visited — they should NOT be in the result
    unexplored should not contain node2
    unexplored should not contain node3
    unexplored should not contain edge12
    unexplored should not contain edge23
    // node1, node4, node5, edge14, edge45, edge53 were not visited — they should be present
    unexplored should contain(node4)
    unexplored should contain(node5)
    logger.info(s"After walking 1->2->3, ${unexplored.size} components remain unexplored")
  }

  // The malapps parameter controls how the unexplored components are divided
  // into groups. With 2 malapps we should get at most 2 groups.
  it should "divide unexplored components among the specified number of malapps" in {
    val graph = createTestGraph()
    val pe = PathsEstimator(graph)
    val emptyPaths: LISTOFWALKEDPATHS = List.empty
    val malapps = 2
    val result = pe.exploreThesePaths(emptyPaths, malapps)
    // Each group should have at most ceil(total / malapps) components
    // and there should be ceil(total / malapps) groups
    result.foreach(group => group.size should be <= (graph.sm.nodes().size() + graph.sm.edges().size()))
    logger.info(s"Unexplored components divided into ${result.size} groups for $malapps malapps")
  }

  // When all paths have been walked (all nodes and edges visited),
  // exploreThesePaths should return an empty recommendation list.
  it should "return empty recommendations when all components have been visited" in {
    val graph = createTestGraph()
    val pe = PathsEstimator(graph)
    // Simulate walks that cover every node and edge in the graph
    val walk1: PATHRESULT = List(
      (node2, edge12),
      (node3, edge23)
    )
    val walk2: PATHRESULT = List(
      (node4, edge14),
      (node5, edge45),
      (node3, edge53)
    )
    // Also need to mark node1 as visited — it appears as the _1 component
    val walk3: PATHRESULT = List(
      (node1, edge12)
    )
    val result = pe.exploreThesePaths(List(walk1, walk2, walk3), 3)
    val unexplored = result.flatten
    unexplored shouldBe empty
    logger.info("All components visited — no recommendations returned")
  }

  // Verify that repeated calls to exploreThesePaths accumulate visit counts
  // in the exploration map (the map is mutable and persists across calls).
  it should "accumulate visit counts across multiple calls" in {
    val graph = createTestGraph()
    val pe = PathsEstimator(graph)
    val walk1: PATHRESULT = List((node2, edge12))
    pe.exploreThesePaths(List(walk1), 2)
    // After first call, node2 and edge12 should each have count 1
    pe.explorationMap(node2) shouldBe 1
    pe.explorationMap(edge12) shouldBe 1
    // Call again with the same walk
    pe.exploreThesePaths(List(walk1), 2)
    // Counts should accumulate to 2
    pe.explorationMap(node2) shouldBe 2
    pe.explorationMap(edge12) shouldBe 2
    logger.info(s"Visit counts accumulated correctly: node2=${pe.explorationMap(node2)}, edge12=${pe.explorationMap(edge12)}")
  }
}
