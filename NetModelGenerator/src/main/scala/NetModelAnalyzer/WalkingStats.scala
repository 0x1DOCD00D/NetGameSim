/*
 * Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package NetModelAnalyzer

import NetGraphAlgebraDefs.{Action, NetGraph, NetGraphComponent, NodeObject}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Try
import cats.instances.map._
import cats.Monoid
import cats.instances.int._
import cats.syntax.semigroup._

class WalkingStats(graph: NetGraph, paths: LISTOFWALKEDPATHS):
  private val graphNodes = graph.sm.nodes.asScala
  private val graphEdges = graph.sm.edges.asScala.map(connPoint => graph.sm.edgeValue(connPoint).get())
  private val graphNodesVisits: Map[NetGraphComponent, Int] =
    graphNodes.collect { case a => (a, 0) }.toMap[NetGraphComponent, Int]
    ++
    graphEdges.collect { case a => (a, 0) }.toMap[NetGraphComponent, Int]

  def graphCoverage(): Map[NetGraphComponent, Int] =
    graphNodesVisits |+| paths.foldLeft(Map[NetGraphComponent, Int]()) { (maps, path) =>
      maps |+| path.foldLeft(Map[NetGraphComponent, Int]()) { (map, component) =>
        val node = component._1.asInstanceOf[NodeObject]
        val edge = component._2.asInstanceOf[Action]
        map |+| Map(node -> 1, edge -> 1)
      }
    }
  end graphCoverage

  def coveragePercentages: (Float, Float) =
    val graphCov = graphCoverage()
    val coveredNodes = graphCov.count((k, v) => v > 0 && k.isInstanceOf[NodeObject])
    val coveredEdges = graphCov.count((k, v) => v > 0 && k.isInstanceOf[Action])
    val nodeCoverage = f"${100f*coveredNodes.toFloat/graphNodes.size}%4.2f".toFloat
    val edgeCoverage = f"${100f*coveredNodes.toFloat/graphEdges.size}%4.2f".toFloat
    (nodeCoverage, edgeCoverage)
  end coveragePercentages



end WalkingStats


