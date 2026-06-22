/*
 * Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package NetModelAnalyzer

import NetGraphAlgebraDefs.{Action, NetGraph, NodeObject}
import Utilz.CreateLogger
import com.google.common.graph.{MutableValueGraph, ValueGraphBuilder}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.Logger

/**
 * Tests for Analyzer focusing on its defensive behavior.
 *
 * The principal test here is the regression for Bug 4 in the round-3 PR:
 * the isomorphism-diagnostic block in Analyzer.apply used to call
 * `.find(_.id == 3).get`, which threw NoSuchElementException on any graph
 * that did not contain a node with id 3. After the fix, Analyzer must
 * handle such graphs gracefully and still return its computed list of
 * strongly-connected components.
 */
class AnalyzerTest extends AnyFlatSpec with Matchers {
  val logger: Logger = CreateLogger(this.getClass)

  val node0: NodeObject = NodeObject(id = 0, children = 1, props = 1, propValueRange = 1, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10, 1)
  val node1: NodeObject = NodeObject(id = 1, children = 1, props = 1, propValueRange = 1, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10, 1)
  val node2: NodeObject = NodeObject(id = 2, children = 1, props = 1, propValueRange = 1, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10, 1)
  val edge01: Action = Action(actionType = 1, fromNode = node0, toNode = node1, fromId = 0, toId = 1, resultingValue = Some(1), cost = 0.1)
  val edge12: Action = Action(actionType = 2, fromNode = node1, toNode = node2, fromId = 1, toId = 2, resultingValue = Some(2), cost = 0.2)

  behavior of "Analyzer.apply"

  // Regression test for Bug 4. Graph intentionally has node ids {0, 1, 2} —
  // no node 3 — so the previous `.find(_.id == 3).get` would fail with
  // NoSuchElementException. With the fix, Analyzer falls back to any
  // removable non-init node (here, node1 or node2) and completes normally.
  it should "not crash when the graph has no node with id 3" in {
    val graph: MutableValueGraph[NodeObject, Action] = ValueGraphBuilder.directed().build()
    graph.addNode(node0)
    graph.addNode(node1)
    graph.addNode(node2)
    graph.putEdgeValue(node0, node1, edge01)
    graph.putEdgeValue(node1, node2, edge12)
    val netGraph = NetGraph(graph, node0)

    // Under the old code this line threw NoSuchElementException. The new
    // code must return the cycle list (possibly empty) without throwing.
    val components = Analyzer(netGraph)
    components should not be null
    logger.info(s"Analyzer returned ${components.size} strongly-connected components on a graph without node 3")
  }

  // Sanity test: Analyzer still works when a node with id 3 is present. The
  // fix only adds a fallback for the missing case; the happy path must still
  // exercise the id==3 branch so existing behavior is preserved.
  it should "still work when the graph contains node id 3" in {
    val node3: NodeObject = NodeObject(id = 3, children = 1, props = 1, propValueRange = 1, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10, 1)
    val edge23: Action = Action(actionType = 3, fromNode = node2, toNode = node3, fromId = 2, toId = 3, resultingValue = Some(3), cost = 0.3)
    val graph: MutableValueGraph[NodeObject, Action] = ValueGraphBuilder.directed().build()
    graph.addNode(node0)
    graph.addNode(node1)
    graph.addNode(node2)
    graph.addNode(node3)
    graph.putEdgeValue(node0, node1, edge01)
    graph.putEdgeValue(node1, node2, edge12)
    graph.putEdgeValue(node2, node3, edge23)
    val netGraph = NetGraph(graph, node0)

    val components = Analyzer(netGraph)
    components should not be null
    logger.info(s"Analyzer with node 3 present returned ${components.size} strongly-connected components")
  }
}
