package Randomizer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UniformProbGeneratorTest extends AnyFlatSpec with Matchers {
  behavior of "random number generator"

  it should "create a uniform distribution of double precision values, take ten random values check the first value" in {
    val values = UniformProbGenerator.generateRandom(10, false)().asInstanceOf[List[Double]]
    values.length shouldBe 10
    values.head should be >= 0.0
    values.head should be <= 1.0
  }

  it should "create a uniform integer distribution, take ten random values check the first value" in {
    val values = UniformProbGenerator.generateRandom(10, true)(1,5).asInstanceOf[List[Int]]
    values.length shouldBe 10
    values.head should be >= 1
    values.head should be <= 5
  }

  it should "create a uniform distribution, take two random values and check them" in {
    val values = UniformProbGenerator.generateRandom(2, false)().asInstanceOf[List[Double]]
    values.length shouldBe 2
    values.head should not equal values.tail.head
  }

}
