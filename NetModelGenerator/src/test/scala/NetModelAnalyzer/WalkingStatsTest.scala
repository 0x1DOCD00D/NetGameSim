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

/**
 * Tests for WalkingStats, which computes graph coverage statistics
 * based on random walk results. Coverage is measured by how many
 * distinct nodes and edges were visited across all walks.
 *
 * These tests verify:
 *  - graphCoverage() correctly counts visits per node/edge
 *  - coveragePercentages computes node and edge coverage independently
 *  - Edge coverage uses the correct numerator (coveredEdges, not coveredNodes)
 *  - Empty walks produce zero coverage
 *  - Full walks produce 100% coverage
 *
 * The test graph is a 4-node directed chain:
 *   1 -> 2 -> 3 -> 4
 */
class WalkingStatsTest extends AnyFlatSpec with Matchers {
  val logger: Logger = CreateLogger(this.getClass)

  val node1: NodeObject = NodeObject(id = 1, children = 1, props = 1, propValueRange = 1, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10, 1)
  val node2: NodeObject = NodeObject(id = 2, children = 2, props = 2, propValueRange = 2, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10, 1)
  val node3: NodeObject = NodeObject(id = 3, children = 3, props = 3, propValueRange = 3, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10, 1)
  val node4: NodeObject = NodeObject(id = 4, children = 4, props = 4, propValueRange = 4, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10, 1)

  val edge12: Action = Action(actionType = 1, fromNode = node1, toNode = node2, fromId = 1, toId = 2, resultingValue = Some(1), cost = 0.1)
  val edge23: Action = Action(actionType = 2, fromNode = node2, toNode = node3, fromId = 2, toId = 3, resultingValue = Some(2), cost = 0.2)
  val edge34: Action = Action(actionType = 3, fromNode = node3, toNode = node4, fromId = 3, toId = 4, resultingValue = Some(3), cost = 0.3)

  def createChainGraph(): NetGraph = {
    val graph: MutableValueGraph[NodeObject, Action] = ValueGraphBuilder.directed().build()
    graph.addNode(node1)
    graph.addNode(node2)
    graph.addNode(node3)
    graph.addNode(node4)
    graph.putEdgeValue(node1, node2, edge12)
    graph.putEdgeValue(node2, node3, edge23)
    graph.putEdgeValue(node3, node4, edge34)
    NetGraph(graph, node1)
  }

  behavior of "WalkingStats"

  // When no walks are provided, every node and edge should have a visit count
  // of zero, meaning nothing was covered.
  it should "report zero coverage when no walks are provided" in {
    val graph = createChainGraph()
    val stats = new WalkingStats(graph, List.empty)
    val coverage = stats.graphCoverage()
    // All entries should be zero since no walks happened
    coverage.values.forall(_ == 0) shouldBe true
    val (nodeCov, edgeCov) = stats.coveragePercentages
    nodeCov shouldBe 0.0f
    edgeCov shouldBe 0.0f
    logger.info(s"Zero walks: node coverage=$nodeCov%, edge coverage=$edgeCov%")
  }

  // A walk that visits only part of the graph should produce partial coverage.
  // Walking 1->2 covers 2 of 4 nodes and 1 of 3 edges.
  it should "compute partial coverage for a walk that visits a subset of the graph" in {
    val graph = createChainGraph()
    // Walk visits node2 via edge12 — covers node2 and edge12
    val walk: PATHRESULT = List((node2, edge12))
    val stats = new WalkingStats(graph, List(walk))
    val coverage = stats.graphCoverage()
    // node2 and edge12 should have count 1
    coverage(node2) shouldBe 1
    coverage(edge12) shouldBe 1
    // node1, node3, node4, edge23, edge34 should have count 0
    coverage(node1) shouldBe 0
    coverage(node3) shouldBe 0
    coverage(node4) shouldBe 0
    coverage(edge23) shouldBe 0
    coverage(edge34) shouldBe 0
    logger.info(s"Partial walk coverage map: $coverage")
  }

  // Regression test for the bug where edge coverage was computed using
  // coveredNodes instead of coveredEdges. To catch this, we need a walk
  // where coveredNodes != coveredEdges. Visiting the same node (node2)
  // via two different edges (edge12, edge23) gives 1 unique covered node
  // but 2 unique covered edges. With the bug, both percentages would use
  // coveredNodes=1 as numerator; with the fix, edge coverage correctly
  // uses coveredEdges=2.
  it should "compute node and edge coverage percentages independently" in {
    val graph = createChainGraph()
    // Walk visits node2 twice (via edge12 and edge23), so coveredNodes=1
    // but coveredEdges=2 — these must produce different percentages.
    val walk: PATHRESULT = List(
      (node2, edge12),
      (node2, edge23)
    )
    val stats = new WalkingStats(graph, List(walk))
    val (nodeCov, edgeCov) = stats.coveragePercentages
    // 1 of 4 nodes covered = 25%
    nodeCov shouldBe 25.0f
    // 2 of 3 edges covered = 66.67%
    // With the old bug, this would wrongly be 100*1/3 = 33.33%
    edgeCov shouldBe 66.67f +- 0.01f
    // Sanity check: node and edge coverage MUST differ for this test to
    // catch the bug where edge coverage accidentally used coveredNodes.
    nodeCov should not be edgeCov
    logger.info(s"Independent coverage: nodes=$nodeCov%, edges=$edgeCov%")
  }

  // When a walk covers every node and edge, both coverages should be 100%.
  it should "report 100% coverage when all nodes and edges are visited" in {
    val graph = createChainGraph()
    val walk: PATHRESULT = List(
      (node2, edge12),
      (node3, edge23),
      (node4, edge34)
    )
    // node1 is not visited by the walk steps above — we need a second walk
    // that includes node1. In the walk format, the first element of each
    // tuple is the destination node. So we add node1 as a visited component.
    val walk2: PATHRESULT = List(
      (node1, edge12)
    )
    val stats = new WalkingStats(graph, List(walk, walk2))
    val (nodeCov, edgeCov) = stats.coveragePercentages
    nodeCov shouldBe 100.0f
    edgeCov shouldBe 100.0f
    logger.info(s"Full coverage: nodes=$nodeCov%, edges=$edgeCov%")
  }

  // Multiple walks should accumulate visit counts. If node2 is visited
  // in two separate walks, its count should be 2.
  it should "accumulate visit counts from multiple walks" in {
    val graph = createChainGraph()
    val walk1: PATHRESULT = List((node2, edge12))
    val walk2: PATHRESULT = List((node2, edge12), (node3, edge23))
    val stats = new WalkingStats(graph, List(walk1, walk2))
    val coverage = stats.graphCoverage()
    // node2 appears in both walks, so its count should be 2
    coverage(node2) shouldBe 2
    // edge12 also appears in both walks
    coverage(edge12) shouldBe 2
    // node3 and edge23 appear only in walk2
    coverage(node3) shouldBe 1
    coverage(edge23) shouldBe 1
    logger.info(s"Accumulated coverage: node2=${coverage(node2)}, edge12=${coverage(edge12)}")
  }

  // The graphCoverage map should contain an entry for every node and edge
  // in the graph, even those not visited.
  it should "include all graph components in the coverage map" in {
    val graph = createChainGraph()
    val stats = new WalkingStats(graph, List.empty)
    val coverage = stats.graphCoverage()
    val nodeCount = graph.sm.nodes().size()
    val edgeCount = graph.sm.edges().size()
    // The map should have entries for all 4 nodes and all 3 edges
    coverage.size shouldBe nodeCount + edgeCount
    coverage.keys.count(_.isInstanceOf[NodeObject]) shouldBe nodeCount
    coverage.keys.count(_.isInstanceOf[Action]) shouldBe edgeCount
    logger.info(s"Coverage map has ${coverage.size} entries ($nodeCount nodes + $edgeCount edges)")
  }
}
