/*
 * Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package NetModelAnalyzer

import NetGraphAlgebraDefs.{NetGraph, NetGraphComponent, NodeObject}

import scala.jdk.CollectionConverters.*

/*
* The purpose of this class is to determine the desired paths in the graph
* that are not covered by the random walks. This is a cumulative process whereas
* new paths are suggested using the Dijkstra algorithm based on the remaining unexplored
* node and edges from the graph of the original application. The key parameter is how many
* paths to recommend to explore, since sending a path incurs a cost for the receiving MapApp.
* Receiving a large number of recommended paths to explore results in both the increased
* cost of the MapApp and the increased possibility of all MapApp exploring all recommended paths.
* Therefore we use the fair cake cutting algorithm to determine a small number of paths to recommend
* to each MapApp. The problem is described here: https://en.wikipedia.org/wiki/Fair_cake-cutting
* A MapApp cannot simply explore the recommended paths, but it can only explore the recommended paths
* that are not already covered by the random walk that represents a user working with the financial app.
* Of course, once MalApp runs out of the recommended paths it reverts to the random walk.
* The frequency of the path estimator recommendations is determined by the configuration parameter.
* */
class PathsEstimator(private val graph: NetGraph):
  val explorationMap: scala.collection.mutable.Map[NetGraphComponent, Int] = collection.mutable.Map()
    ++ graph.sm.nodes().asScala.map(node => node -> 0).toMap
    ++ graph.sm.edges().asScala.map(edge => graph.sm.edgeValue(edge.nodeU(), edge.nodeV()).get() -> 0).toMap
  def exploreThesePaths(paths: LISTOFWALKEDPATHS, malapps: Int): List[SLICEOFCOMPONENTPIE] =
    paths.foreach(path => path.foreach(comp =>
      explorationMap(comp._1) += 1
      explorationMap(comp._2) += 1
    )
    )
    explorationMap.filterNot(_._2 > 0).keys.toList.grouped(malapps).toList

object PathsEstimator:
  def apply(graph: NetGraph) = new PathsEstimator(graph)