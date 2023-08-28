package NetGraphAlgebraDefs

import NetGraphAlgebraDefs.{NetGraph, NetModel, NetModelAlgebra}
import Randomizer.SupplierOfRandomness
import Utilz.ConfigReader.getConfigEntry
import Utilz.CreateLogger
import Utilz.NGSConstants.{DEFAULTEDGEPROBABILITY, EDGEPROBABILITY}
import org.mockito.Mockito.{mock, when}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.slf4j.Logger

class NetModelAlgebraTest extends AnyFlatSpec with Matchers with MockitoSugar {
  val logger: Logger = CreateLogger(this.getClass)
  behavior of "Net graph generation"

  it should "test a mock" in {
    import Utilz.ConfigReader
    val crMock = mock[NetModel]
    when(crMock.generateModel()).thenReturn(Option(NetGraph(null,null)))
    val graph = crMock.generateModel()
    graph shouldBe Option(NetGraph(null,null))
  }

  it should "create a small net graph" in {
    val graph: NetGraph = NetModelAlgebra().get

    val am = graph.adjacencyMatrix
    graph.degrees.length should be >= 1
    am.flatMap(nodeRow => List(nodeRow.count(_ < Float.PositiveInfinity))).toList.length should be >= 10

    graph.totalNodes should be <= NetModelAlgebra.statesTotal+1
  }

}
