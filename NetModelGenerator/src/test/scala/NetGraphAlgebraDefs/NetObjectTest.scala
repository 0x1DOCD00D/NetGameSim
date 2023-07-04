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
}