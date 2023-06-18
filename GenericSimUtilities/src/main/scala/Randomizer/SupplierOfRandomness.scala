package Randomizer

import Utilz.{CreateLogger, NGSConstants}
import Utilz.NGSConstants.SEED
import com.typesafe.config.ConfigFactory
import org.slf4j.Logger

import scala.util.Try

trait MutableBookeeping4Efficiency:
  protected var initInts: Boolean = false
  protected var initDbls: Boolean = false
  protected var currOffsetInt:Int = 0
  protected var currGenInt: UniformProbGenerator = _
  protected var currOffsetDbl:Int = 0
  protected var currGenDbl: UniformProbGenerator = _


object SupplierOfRandomness extends MutableBookeeping4Efficiency:
  val logger: Logger = CreateLogger(this.getClass)
  infix def `YesOrNo?`(thresholdProb: Double = 0.5d): Boolean =
    require(thresholdProb >= 0.0d && thresholdProb <= 1.0d, s"thresholdProb must be between 0.0 and 1.0, but was $thresholdProb")
    randProbs(1).head < thresholdProb
  def onDemand(minv:Int = 0, maxv:Int = Int.MaxValue): Int =
    if !initInts || currGenInt == null then
      initInts = true
      UniformProbGenerator(UniformProbGenerator.createGenerator(seed), szOfValues = 0, ints = true) match {
        case (gen, offset, lstOfInts) => currGenInt = gen; currOffsetInt = offset; lstOfInts.asInstanceOf[List[Int]]
      }
    if minv >= maxv then minv else currGenInt.generator.between(minv,maxv)

  def randInts(howManyNumbers: Int): List[Int] =
    if !initInts then
      initInts = true
      UniformProbGenerator(UniformProbGenerator.createGenerator(seed), szOfValues = howManyNumbers, ints = true) match {
        case (gen, offset, lstOfInts) => currGenInt = gen; currOffsetInt = offset; lstOfInts.asInstanceOf[List[Int]]
      }
    else UniformProbGenerator(currGenInt, offset = currOffsetInt, szOfValues = howManyNumbers, ints = true) match {
      case (gen, offset, lstOfInts) => currGenInt = gen; currOffsetInt += offset; lstOfInts.asInstanceOf[List[Int]]
    }

  def randProbs(howManyNumbers: Int): List[Double] =
    if !initDbls then
      initDbls = true
      UniformProbGenerator(UniformProbGenerator.createGenerator(seed), szOfValues = howManyNumbers) match {
        case (gen, offset, lst) => currGenDbl = gen; currOffsetDbl = offset; lst.asInstanceOf[List[Double]]
      }
    else UniformProbGenerator(currGenInt, offset = currOffsetInt, szOfValues = howManyNumbers) match {
      case (gen, offset, lst) => currGenInt = gen; currOffsetInt = offset; lst.asInstanceOf[List[Double]]
    }

  private val seed: Option[Long] = Try(NGSConstants.globalConfig.getLong(SEED)) match {
    case scala.util.Success(value) =>
      Try(value) match {
        case scala.util.Success(value) => Some(value)
        case scala.util.Failure(_) => None
      }
    case scala.util.Failure(_) => None
  }

