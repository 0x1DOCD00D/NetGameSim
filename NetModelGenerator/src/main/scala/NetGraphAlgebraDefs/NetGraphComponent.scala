package NetGraphAlgebraDefs

import Randomizer.{SupplierOfRandomness, UniformProbGenerator}
import Randomizer.UniformProbGenerator.*
trait NetGraphComponent

case class NodeObject(id: Int, children: Int, props: Int, currentDepth: Int = 1, propValueRange:Int, maxDepth:Int, maxBranchingFactor:Int, maxProperties:Int, storedValue: Double) extends NetGraphComponent:
  val properties: List[Int] = List.fill(props)(SupplierOfRandomness.onDemandInt(pmaxv = propValueRange, repeatable = false))
  val childrenObjects: List[NodeObject] =
    if currentDepth <= maxDepth then
      List.tabulate(children)(cid => NodeObject(cid+id+1,
        SupplierOfRandomness.onDemandInt(pmaxv=maxBranchingFactor, repeatable = false),
        SupplierOfRandomness.onDemandInt(pmaxv=maxProperties, repeatable = false), currentDepth + 1,
        propValueRange, maxDepth, maxBranchingFactor, maxProperties, SupplierOfRandomness.onDemandReal(repeatable = false)))
    else List.empty
  def childrenCount: Int = children + childrenObjects.map(_.childrenCount).sum
  def modify: NodeObject =
    NodeObject(id,
      children,
      props,
      currentDepth,
      SupplierOfRandomness.onDemandInt(pmaxv = propValueRange, repeatable = false),
      maxDepth,
      SupplierOfRandomness.onDemandInt(pmaxv = propValueRange, repeatable = false),
      maxProperties,
      SupplierOfRandomness.onDemandReal(repeatable = false)
    )

case class Action(actionType: Int, fromNode: NodeObject, toNode: NodeObject, fromId: Int, toId: Int, resultingValue: Option[Int], cost: Double) extends NetGraphComponent:
  def modify: Action = Action(SupplierOfRandomness.onDemandInt(pmaxv = NetModelAlgebra.actionRange), fromNode, toNode, fromId, toId, resultingValue, SupplierOfRandomness.randProbs(1)().head)

case object TerminalNode extends NetGraphComponent
case object TerminalAction extends NetGraphComponent