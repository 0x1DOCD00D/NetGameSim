package NetGraphAlgebraDefs

import NetGraphAlgebraDefs.NetModelAlgebra.valuableDataProbability
import Randomizer.{SupplierOfRandomness, UniformProbGenerator}
import Randomizer.UniformProbGenerator.*

import scala.collection.parallel.*
import scala.collection.parallel.CollectionConverters.*

trait NetGraphComponent

case class NodeObject(id: Int, children: Int, props: Int, currentDepth: Int = 1, propValueRange:Int, maxDepth:Int, maxBranchingFactor:Int, maxProperties:Int, storedValue: Double, valuableData: Boolean = false) extends NetGraphComponent:
  val properties: List[Int] = 1.to(props).par.map(_=>SupplierOfRandomness.onDemandInt(pmaxv = NetGraphAlgebraDefs.NetModelAlgebra.propValueRange, repeatable = false)).toList
  val childrenObjects: List[NodeObject] =
    if currentDepth <= maxDepth then
      List.tabulate(children)(cid => NodeObject(cid+id+1,
        currentDepth = currentDepth + 1,
        children = SupplierOfRandomness.onDemandInt(pmaxv = NetGraphAlgebraDefs.NetModelAlgebra.maxBranchingFactor, repeatable = false),
        props = SupplierOfRandomness.onDemandInt(pmaxv = NetGraphAlgebraDefs.NetModelAlgebra.maxProperties, repeatable = false),
        propValueRange = SupplierOfRandomness.onDemandInt(pmaxv = NetGraphAlgebraDefs.NetModelAlgebra.propValueRange, repeatable = false),
        maxDepth = SupplierOfRandomness.onDemandInt(pmaxv = NetGraphAlgebraDefs.NetModelAlgebra.maxDepth, repeatable = false),
        maxBranchingFactor = SupplierOfRandomness.onDemandInt(pmaxv = NetGraphAlgebraDefs.NetModelAlgebra.maxBranchingFactor, repeatable = false),
        maxProperties = SupplierOfRandomness.onDemandInt(pmaxv = NetGraphAlgebraDefs.NetModelAlgebra.maxProperties, repeatable = false),
        storedValue = SupplierOfRandomness.onDemandReal(repeatable = false),
        valuableData = SupplierOfRandomness.`YesOrNo?`(valuableDataProbability)()
      ))
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
      SupplierOfRandomness.onDemandReal(repeatable = false),
      valuableData
    )

case class Action(actionType: Int, fromNode: NodeObject, toNode: NodeObject, fromId: Int, toId: Int, resultingValue: Option[Int], cost: Double) extends NetGraphComponent:
  def modify: Action = Action(SupplierOfRandomness.onDemandInt(pmaxv = NetModelAlgebra.actionRange), fromNode, toNode, fromId, toId, resultingValue, SupplierOfRandomness.randProbs(1)().head)

case object TerminalNode extends NetGraphComponent
case object TerminalAction extends NetGraphComponent