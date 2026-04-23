/*
 * Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package NetModelAnalyzer

import NetGraphAlgebraDefs.{Action, NodeObject}
import NetModelAnalyzer.Budget.{MalAppBudget, TargetAppScore}
import Utilz.CreateLogger
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.Logger

/**
 * Tests for CostRewardCalculator focused on defensive behavior around empty
 * walk paths. The module previously contained no dedicated test file — it
 * was only exercised indirectly through GraphPerturbationAlgebraTest.
 *
 * The principal test here is the regression for the empty-path
 * division-by-zero bug: when RandomWalker.trimPath produces an empty
 * PATHRESULT (a walk that terminated immediately at a TerminalNode),
 * avgWeight was computed as `sum / 0.0`, yielding NaN that then
 * propagated through every subsequent budget/score update.
 */
class CostRewardCalculatorTest extends AnyFlatSpec with Matchers {
  val logger: Logger = CreateLogger(this.getClass)

  val nodeA: NodeObject = NodeObject(id = 1, children = 1, props = 1, propValueRange = 1, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10, 1)
  val nodeB: NodeObject = NodeObject(id = 2, children = 1, props = 1, propValueRange = 1, maxDepth = 5, maxBranchingFactor = 5, maxProperties = 10, 1)
  val edgeAB: Action = Action(actionType = 1, fromNode = nodeA, toNode = nodeB, fromId = 1, toId = 2, resultingValue = Some(1), cost = 0.5)

  behavior of "CostRewardCalculator"

  // Regression test for the empty-path division-by-zero bug. Before the fix,
  // the calculator computed avgWeight as `sum / pathLength` where pathLength
  // was 0.0 for an empty path, yielding NaN. That NaN was then fed into
  // MalAppBudget.reward(avgWeight) or MalAppBudget.penalty(avgWeight),
  // poisoning every downstream budget update for the remainder of the run.
  // After the fix, an empty path produces avgWeight = 0.0 and the returned
  // costs are finite numbers that can safely feed later calculations.
  it should "not produce NaN when the walk path is empty" in {
    val emptyPath: PATHRESULT = List.empty
    val modRecord = Map.empty[NetGraphAlgebraDefs.NetGraphComponent, Double]
    val initialCosts = (MalAppBudget(10.0), TargetAppScore(5.0))

    val ((newBudget, newScore), detected) =
      CostRewardCalculator(emptyPath, modRecord, List())(initialCosts)

    newBudget.toDouble.isNaN shouldBe false
    newBudget.toDouble.isInfinite shouldBe false
    newScore.toDouble.isNaN shouldBe false
    newScore.toDouble.isInfinite shouldBe false
    // An empty walk detects no modifications, so the detected list is empty.
    detected shouldBe empty
    logger.info(s"Empty path safely handled: budget=${newBudget.toDouble}, score=${newScore.toDouble}")
  }

  // Sanity check: a non-empty path should still compute finite values after
  // the fix. The guard only short-circuits when pathLength is zero, so
  // normal paths must continue to produce the average of edge costs.
  it should "process a non-empty path and return finite values" in {
    val path: PATHRESULT = List((nodeB, edgeAB))
    val modRecord = Map.empty[NetGraphAlgebraDefs.NetGraphComponent, Double]
    val initialCosts = (MalAppBudget(10.0), TargetAppScore(5.0))

    val ((newBudget, newScore), _) =
      CostRewardCalculator(path, modRecord, List())(initialCosts)

    newBudget.toDouble.isNaN shouldBe false
    newBudget.toDouble.isInfinite shouldBe false
    newScore.toDouble.isNaN shouldBe false
    newScore.toDouble.isInfinite shouldBe false
    logger.info(s"Non-empty path produced finite values: budget=${newBudget.toDouble}, score=${newScore.toDouble}")
  }
}
