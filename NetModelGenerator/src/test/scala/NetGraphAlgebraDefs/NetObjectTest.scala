package NetGraphAlgebraDefs

import Randomizer.SupplierOfRandomness
import Utilz.ConfigReader.getConfigEntry
import Utilz.CreateLogger
import Utilz.NGSConstants.{DEFAULTEDGEPROBABILITY, EDGEPROBABILITY}
import org.mockito.Mockito.{mock, when}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.slf4j.Logger

class NetObjectTest extends AnyFlatSpec with Matchers with MockitoSugar {
  val logger: Logger = CreateLogger(this.getClass)
  behavior of "Net graph generation"

  it should "test a mock" in {
    val mockRandomizer = mock[SupplierOfRandomness.type ]
    when(mockRandomizer.onDemandInt()).thenReturn(1)
    mockRandomizer.onDemandInt() shouldBe 1
  }

  behavior of "NodeObject.modify"

  // Regression test: NodeObject.modify previously used propValueRange as the
  // randomization bound for maxBranchingFactor (position 7 in the constructor).
  // That silently skewed every perturbed node's branching factor. This test
  // exercises modify() many times and confirms maxBranchingFactor stays within
  // the original node's maxBranchingFactor bound, not propValueRange.
  it should "respect each field's own bound when generating a modified node" in {
    // Use a deliberately skewed configuration so the old bug would be visible:
    // if maxBranchingFactor is 3 but propValueRange is 99, the old code could
    // produce maxBranchingFactor values up to 99 — the fix keeps it in [0, 3].
    val node = NodeObject(
      id = 1,
      children = 2,
      props = 4,
      currentDepth = 1,
      propValueRange = 99,
      maxDepth = 5,
      maxBranchingFactor = 3,
      maxProperties = 10,
      storedValue = 1.0,
      valuableData = false
    )
    val trials = 200
    val results = (1 to trials).map(_ => node.modify)
    // The modified maxBranchingFactor must never exceed the original node's
    // maxBranchingFactor. If the old bug regressed, we would see values above 3.
    val violating = results.filter(_.maxBranchingFactor > node.maxBranchingFactor)
    violating shouldBe empty
    // Fields we never randomize in modify() must be preserved byte-for-byte.
    results.foreach { m =>
      m.id shouldBe node.id
      m.children shouldBe node.children
      m.props shouldBe node.props
      m.currentDepth shouldBe node.currentDepth
      m.maxDepth shouldBe node.maxDepth
      m.maxProperties shouldBe node.maxProperties
      m.valuableData shouldBe node.valuableData
    }
    logger.info(s"modify() produced $trials nodes, max observed maxBranchingFactor=${results.map(_.maxBranchingFactor).max} (bound: ${node.maxBranchingFactor})")
  }
}