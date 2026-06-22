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

  // Regression: randInts previously hardcoded howMany=1 regardless of the
  // caller's requested count, so randInts(N) always returned length 1.
  it should "return a vector whose length matches howManyNumbers" in {
    val ints = SupplierOfRandomness.randInts(10, pminv = 0, pmaxv = 100)
    ints.length shouldBe 10
    all(ints) should be >= 0
    all(ints) should be <= 100
  }
}
