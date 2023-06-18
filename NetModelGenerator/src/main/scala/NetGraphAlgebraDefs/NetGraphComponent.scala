package NetGraphAlgebraDefs

import Randomizer.{SupplierOfRandomness, UniformProbGenerator}
import Randomizer.UniformProbGenerator.*
trait NetGraphComponent

case class NodeObject(id: Int, children: Int, props: Int, currentDepth: Int = 1, propValueRange:Int, maxDepth:Int, maxBranchingFactor:Int, maxProperties:Int) extends NetGraphComponent:
  val properties: List[Int] = List.fill(props)(SupplierOfRandomness.onDemand(maxv = propValueRange))
  val childrenObjects: List[NodeObject] =
    if currentDepth <= maxDepth then
      List.tabulate(children)(cid => NodeObject(cid+id+1,
        SupplierOfRandomness.onDemand(maxv=maxBranchingFactor),
        SupplierOfRandomness.onDemand(maxv=maxProperties), currentDepth + 1,
        propValueRange, maxDepth, maxBranchingFactor, maxProperties))
    else List.empty
  def childrenCount: Int = children + childrenObjects.map(_.childrenCount).sum
  def modify: NodeObject =
    NodeObject(id,
      children,
      props,
      currentDepth,
      SupplierOfRandomness.onDemand(maxv = propValueRange),
      maxDepth,
      SupplierOfRandomness.onDemand(maxv = propValueRange),
      maxProperties
    )

case class Action(actionType: Int, fromId: Int, toId: Int, resultingValue: Option[Int], cost: Double) extends NetGraphComponent:
  def modify: Action = Action(SupplierOfRandomness.onDemand(maxv = NetModelAlgebra.actionRange), fromId, toId, resultingValue, SupplierOfRandomness.randProbs(1).head)

case object TerminalNode extends NetGraphComponent
case object TerminalAction extends NetGraphComponent