package Randomizer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SupplierOfRandomnessTest extends AnyFlatSpec with Matchers {
  behavior of "supplier of the random numbers"

  it should "obtain ten random probs" in {
    val lst = SupplierOfRandomness.randProbs(10)
    lst.length shouldBe 10
    lst.head shouldBe 0.7304302967434272
    lst.sum shouldBe 5.255743899997139
  }

  it should "obtain one random integer within a given range" in {
    val intval = SupplierOfRandomness.onDemand(10,20)
    intval should be <= 20
    intval should be >= 10
  }
}
