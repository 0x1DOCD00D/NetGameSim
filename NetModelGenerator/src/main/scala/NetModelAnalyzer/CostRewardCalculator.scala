/*
 * Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package NetModelAnalyzer

import NetGraphAlgebraDefs.{Action, NetGraphComponent, NodeObject}
import NetGraphAlgebraDefs.GraphPerturbationAlgebra.{ModificationRecord, ModificationRecordInverse}
import NetModelAnalyzer.Budget.*
import Randomizer.SupplierOfRandomness
import Utilz.{CreateLogger, NGSConstants}
import cats.data.State

import scala.annotation.tailrec

/*
* The simulator computes random walks through the net graph thus simulating the behavior of the smartphone user.
* Every time a random walk is performed the simulator produces an instance of PATHRESULT that contains the
* list of nodes visited during the random walk and the list of edges traversed during the random walk.
* In addition, ModificationRecordInverse contains the list of modifications that were performed on the net graph
* so that it is possible for each node/edge in PATHRESULT to determine whether it was modified or not.
* While detecting some delta two key variables are updated: the target app score and the budget of the malapp.
* Each detected deviation from the "correct" net graph results in the decrease of target app score,
* and at the same time each step decreases the budget of the malapp. However, the PATHRESULT comes with the
* service reward that designates the usefulness of the malapp to the user. If the malapp is a true helpful app
* then it's useful fort the majority of the random walks earning high service award that compensates for the
* cost of detection.
* Service award should be proportional to the weights of the edges in a random walk. The higher the weight
* the more complexity there is to navigate the app and the more service should be provided to the user. Therefore,
* we computer the service award as the average of the weights of the edges in the random walk multiplied by the service award constant.
* This computed service award will be applied using the accept/reject mechanism of the service award probability.
* */
type COSTTUPLE = (MalAppBudget, TargetAppScore)
type CostRewardFunction = (PATHRESULT, ModificationRecordInverse, DetectedModifiedComponents) => COSTTUPLE => (COSTTUPLE, DetectedModifiedComponents)
type DetectedModifiedComponents = List[NetGraphComponent]

object CostRewardCalculator extends CostRewardFunction:
  override def apply(v1: PATHRESULT, v2: ModificationRecordInverse, v3: DetectedModifiedComponents): COSTTUPLE => (COSTTUPLE, DetectedModifiedComponents) =
    def detectModification(guiComp: NetGraphComponent, costs: COSTTUPLE, dc: DetectedModifiedComponents): (COSTTUPLE, DetectedModifiedComponents) =
      val newMalScore = costs._1.cost()
      if v2.contains(guiComp) && !dc.contains(guiComp) then
        val newTapScore = costs._2.penalty(v2(guiComp))
        logger.debug(s"Modification detected for $guiComp and applied penalty resulting in the app score $newTapScore")
        ((newMalScore, newTapScore), guiComp :: dc)
      else ((newMalScore, costs._2), dc)
    end detectModification

    @tailrec
    def computeCosts4Walk(path: PATHRESULT, costs: COSTTUPLE, dc: DetectedModifiedComponents): (COSTTUPLE, DetectedModifiedComponents) =
      path match
        case Nil => (costs, dc)
        case hd::tl =>
          val resultNode = detectModification(hd._1, costs, dc)
          val result = detectModification(hd._2, resultNode._1, resultNode._2)
          computeCosts4Walk(tl, result._1, result._2)
    end computeCosts4Walk

    (costs:COSTTUPLE) => {
      import NetGraphAlgebraDefs.NetModelAlgebra.*
      val pathLength = v1.size.toDouble
      val avgWeight: Double = v1.map(_._2.asInstanceOf[Action].cost).sum / pathLength

      val (newCost:COSTTUPLE, dc:DetectedModifiedComponents) = computeCosts4Walk(v1, costs, v3)
      if math.abs(newCost._2.toDouble - costs._2.toDouble) > NGSConstants.EPSILON then
        logger.debug(s"Malappbudget: ${newCost._1} changed from ${costs._1}, tapp score: ${newCost._2} changed from ${costs._2}, detected ${dc.size} changed components")
      ((if SupplierOfRandomness.`YesOrNo?`(serviceRewardProbability) then newCost._1.reward(avgWeight) else newCost._1.penalty(avgWeight), newCost._2), dc)
    }
  end apply

  @main def runCostRewardCalculator(args: String*): Unit =
    import NetGraphAlgebraDefs.NetModelAlgebra.*
    val logger = CreateLogger(classOf[CostRewardCalculator.type])
    logger.info(s"malAppBudget: $mapAppBudget, costOfDetection: $costOfDetection, serviceReward: $serviceReward, servicePenalty: $servicePenalty, targetAppScore: $targetAppScore, " +
      s"targetAppPenalty: $targetAppLowPenalty, targetAppHighPenalty: $targetAppHighPenalty, serviceRewardProbability: $serviceRewardProbability")
