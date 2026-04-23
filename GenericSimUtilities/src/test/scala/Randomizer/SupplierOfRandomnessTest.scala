package Randomizer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SupplierOfRandomnessTest extends AnyFlatSpec with Matchers {
  given repeatable:Boolean = true
  behavior of "supplier of the random numbers"

  it should "obtain ten random probs" in {
    val lst:Vector[Double] = SupplierOfRandomness.randProbs(10)()
    lst.length shouldBe 10
  }

  it should "obtain one random integer within a given range" in {
    val intval = SupplierOfRandomness.onDemandInt(pminv = 10,pmaxv = 20)
    intval should be <= 20
    intval should be >= 10
  }

  // Regression test for Bug 3 in the round-3 PR: randInts previously hard-coded
  // howMany = 1 on every internal call to generateRandom, so it returned a
  // single-element vector regardless of the howManyNumbers argument. After the
  // fix, requesting 100 integers must actually return 100 integers.
  it should "return exactly howManyNumbers values from randInts" in {
    val result: Vector[Int] = SupplierOfRandomness.randInts(100, 0, 10)
    result.length shouldBe 100
    // Every value must also respect the declared range.
    result.foreach { v =>
      v should be >= 0
      v should be < 10
    }
  }
}
