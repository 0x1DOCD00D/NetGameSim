/*
 * Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package NetModelAnalyzer

import NetGraphAlgebraDefs.NetModelAlgebra.{createAction, mapAppBudget, numberOfExperiments, targetAppScore}
import NetGraphAlgebraDefs.{Action, GraphPerturbationAlgebra, NetGraph, NetGraphComponent, NetModelAlgebra, NodeObject}
import NetGraphAlgebraDefs.GraphPerturbationAlgebra.{ModificationRecord, ModificationRecordInverse, inverseMR}
import NetModelAnalyzer.Budget.{MalAppBudget, TargetAppScore}
import com.google.common.graph.{EndpointPair, MutableValueGraph}
import org.jgrapht.alg.connectivity.KosarajuStrongConnectivityInspector
import org.jgrapht.alg.interfaces.VertexCoverAlgorithm
import org.jgrapht.alg.isomorphism.{VF2GraphIsomorphismInspector, VF2SubgraphIsomorphismInspector}
import org.jgrapht.alg.vertexcover.RecursiveExactVCImpl

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

object Analyzer:
  def apply(g: NetGraph): List[Set[NodeObject]] =
    import org.jgrapht.*
    import org.jgrapht.graph.*
    import org.jgrapht.graph.guava.*

    val gc = g.copy
    val jg: Graph[NodeObject, Action] = new MutableValueGraphAdapter(gc.sm, createAction(gc.initState,gc.initState), (x: Action) => x.cost).asInstanceOf[Graph[NodeObject, Action]]
    val inspector = new KosarajuStrongConnectivityInspector(jg)
    val components = inspector.stronglyConnectedSets().asScala.toList.filter(_.size > 1)
    logger.info(s"Detected ${components.map(_.size)} cycles")

    val newg = g.copy
    newg.sm.removeNode(newg.sm.nodes().asScala.find(_.id == 3).get)
    val jg1: Graph[NodeObject, Action] = new MutableValueGraphAdapter(newg.sm, createAction(newg.initState,newg.initState), (x: Action) => x.cost).asInstanceOf[Graph[NodeObject, Action]]
    val vf2: VF2SubgraphIsomorphismInspector[NodeObject, Action] = new VF2SubgraphIsomorphismInspector(jg, jg1)
    val isomorphisms = vf2.getMappings.asScala.toList
    logger.info(s"Detected ${isomorphisms.size} isomorphic mappings")
    isomorphisms.foreach(m => logger.info(s"Isomorphism: $m"))
    components.map(s =>s.asScala.toSet)


  def apply(graph: NetGraph, mr: ModificationRecordInverse, numberOfWalks:Int, appName: String): (COSTTUPLE, DetectedModifiedComponents) =
    @tailrec def computeTotalCostsRewards(allWalks: List[PATHRESULT], cr: COSTTUPLE, dc: DetectedModifiedComponents): (COSTTUPLE, DetectedModifiedComponents) =
      allWalks match
        case Nil => (cr,dc)
        case head :: tail =>
          val ucr:(COSTTUPLE, DetectedModifiedComponents) = CostRewardCalculator(head, mr, dc)(cr)
          computeTotalCostsRewards(tail, ucr._1, ucr._2)
    end computeTotalCostsRewards

    logger.info(s"Analyzing the model of the $appName app: ${mr.size} modifications out of which ${mr.keySet.count(_.isInstanceOf[NodeObject])} are GUI objects")
    val walker = RandomWalker(graph)
    val walks: List[PATHRESULT] = walker.walk(numberOfWalks)
    computeWalksStats(graph, walks, 10)
    computeTotalCostsRewards(walks, (MalAppBudget(0.0d), TargetAppScore(0.0d)), List())
  end apply

  def computeWalksStats(graph: NetGraph, walks: List[PATHRESULT], bins: Int): Unit =
    require(bins > 0, "The number of bins must be positive")
    require(walks.length > bins, "The number of walks must be greater than the number of bins")
    walks.foreach(walk => logger.debug(s"Walks: ${graph.initState.id :: walk}"))
    val stats = new WalkingStats(graph, walks)
    val (nodeCov, edgeCov) = stats.coveragePercentages
    val walkPathLengths = walks.map(_.size)//.sortBy(identity)

    val (max,min) = (walkPathLengths.max,walkPathLengths.min)
    logger.info(s"Max walk length is $max and min walk length is $min")
    val binWidth:Double = (max.toDouble - min.toDouble) / bins.toDouble
    val bounds: List[Double] = (1 to bins).map { x => min + binWidth * x }.toList

    def histogramBuilder(w: List[Int], b:List[Double]): List[List[Int]] =
      b match {
        case Nil => Nil
        case h :: Nil => List(w)
        case h :: t   =>
          val (l,r) = w.partition(_ <= h)
          l :: histogramBuilder(r,t)
      }

    val histogram = histogramBuilder(walkPathLengths, bounds)
    val histQuant = histogram.map(_.length).zip(bounds)
    logger.info(f"Node coverage: $nodeCov and edge coverage is $edgeCov")
//    histQuant.foreach(v => logger.info(s"There are ${v._1} walks with length less than ${v._2.toInt}"))
/*
    val sorted = stats.graphCoverage().toSeq.filter(e => e._1.isInstanceOf[NodeObject] && e._2 > 0).sortBy(_._2).map(_._1.asInstanceOf[NodeObject].id).toSet
    val all = graph.sm.nodes().asScala.map(_.id).toSet
    logger.info(s"Sorted walks: $sorted")
    logger.info(s"Uncovered nodes: ${all -- sorted}")
    val pe = PathsEstimator(graph)
    val estimate: List[SLICEOFCOMPONENTPIE] = pe.exploreThesePaths(walks, 3)
    estimate.flatten.filter(e => e.isInstanceOf[NodeObject]).map(e => e.asInstanceOf[NodeObject].id).toSet
    val remaining =  all -- sorted
    logger.info(s"Estimate: $estimate")
*/

  @main def runAnalyzer(args: String*): Unit =
    import com.google.common.graph.{Graphs, MutableValueGraph, ValueGraphBuilder}
    def debug(g: NetGraph, g1:NetGraph):Unit =
      val distances = g.distances().toSeq.filter(_._2 < Double.PositiveInfinity).sortBy(_._2).toMap
      val distances1 = g1.distances().toSeq.filter(_._2 < Double.PositiveInfinity).sortBy(_._2).toMap
      if distances != distances1 then logger.error(s"Distances are different: ${distances.toSet -- distances1.toSet}")
      val (minDistance, maxDistance) = (distances.minBy(_._2)._2, distances.maxBy(_._2)._2)
      val (minDistance1, maxDistance1) = (distances.minBy(_._2)._2, distances.maxBy(_._2)._2)
      if minDistance != minDistance1 then logger.error(s"Min distances are different: $minDistance and $minDistance1")
      if maxDistance != maxDistance1 then logger.error(s"Max distances are different: $maxDistance and $maxDistance1")
      if maxDistance * NetModelAlgebra.distanceCoeff != maxDistance1 * NetModelAlgebra.distanceCoeff then logger.error(s"perturb distances are different")
    end debug

    logger.info(NetModelAlgebra.getFields.mkString(", ") )
    val winners: ListBuffer[Int] = ListBuffer()
    NetModelAlgebra() match
      case None => logger.error("Could not create the model")
      case Some(graph) =>
        (1 to numberOfExperiments).foreach {
          exp =>
            logger.info(s"=============> Experiment $exp <====================")
            logger.info(s"Creating target app model for experiment $exp")
            logger.info("Perturbing target app model for experiment $exp")
            val tappModel: GraphPerturbationAlgebra#GraphPerturbationTuple = GraphPerturbationAlgebra(graph.copy, true)

            val pms: List[GraphPerturbationAlgebra#GraphPerturbationTuple] = (1 to 10).toList.map(num =>
              logger.info(s"Perturbing doppleganger app model $num")
              GraphPerturbationAlgebra(graph.copy)
            )
            if pms.exists(_._2.isEmpty) then logger.error("Perturbed models are empty")
            else
              logger.info("Successfully created perturbed models")
              val tappCosts: (COSTTUPLE, DetectedModifiedComponents) = Analyzer(tappModel._1, inverseMR(tappModel._2), NetModelAlgebra.numberOfWalks, "Target")
              val allCosts: List[(COSTTUPLE, DetectedModifiedComponents)] = pms.zipWithIndex.map {
                case (pm, index) => Analyzer(pm._1, inverseMR(pm._2), NetModelAlgebra.numberOfWalks, s"Doppleganger_$index")
              }
              logger.info(s"Target app costs: ${tappCosts._1}")
              val listOfCosts: List[COSTTUPLE] = tappCosts._1 :: allCosts.map(_._1)
              val minCost: Double = listOfCosts.map(_._1.toDouble).min
              val maxCost: Double = if minCost < 0 then listOfCosts.map(_._1.toDouble).max - minCost else listOfCosts.map(_._1.toDouble).max
              val removedNegListOfCosts = listOfCosts.map(c => if minCost < 0.0 then (c._1.toDouble - minCost, c._2.toDouble) else (c._1.toDouble, c._2.toDouble))
              val maxScore: Double = removedNegListOfCosts.map(_._2.toDouble).max
              val normalizedCosts: List[(Double, Double, Double, Double)] = removedNegListOfCosts.map(c => (c._1.toDouble / maxCost, c._2.toDouble / maxScore, c._1.toDouble, c._2.toDouble))

              normalizedCosts.zipWithIndex.foreach(
                (c, index) =>
                  if index == 0 then logger.info(f"Target app costs: ${c._1}%1.3f with the score ${c._2}%1.3f, abs values (${c._3}%1.3f, ${c._4}%1.3f) and its harmonic score ${2 * c._2 * c._1 / (c._2 + c._1)}%1.3f and its geometric score ${scala.math.sqrt(c._2 * c._1)}%1.3f")
                  else logger.info(f"Doppleganger app $index costs: ${c._1}%1.3f with the score ${c._2}%1.3f, abs values (${c._3}%1.3f, ${c._4}%1.3f) and its harmonic score ${2 * c._2 * c._1 / (c._2 + c._1)}%1.3f and its geometric score ${scala.math.sqrt(c._2 * c._1)}%1.3f")
              )

              val appScores: List[(Double, Int)] = normalizedCosts.zipWithIndex.map((c, index) => (scala.math.sqrt(c._2 * c._1), index)).sortBy(_._1)
              winners += appScores.head._2
              appScores.foreach(
                (c, index) =>
                  if index == 0 then logger.info(f"Target app scores $c%1.3f")
                  else logger.info(f"Doppleganger app ${index + 1} scores $c%1.3f")
              )
              logger.info(f"The winner is app ${appScores.head._2} with the score ${appScores.head._1}%1.3f")
        }
        logger.info(s"Winners: ${winners.mkString(", ")}")
