package Randomizer

import Utilz.{CreateLogger, NGSConstants}
import Utilz.NGSConstants.SEED
import com.typesafe.config.ConfigFactory
import org.slf4j.Logger

import java.lang.annotation.Repeatable
import scala.annotation.internal.Repeated
import scala.util.Try

object SupplierOfRandomness:
  val logger: Logger = CreateLogger(this.getClass)

  def `YesOrNo?`(thresholdProb: Double = 0.5d)(repeatable: Boolean = false): Boolean =
    require(thresholdProb >= 0.0d && thresholdProb <= 1.0d, s"thresholdProb must be between 0.0 and 1.0, but was $thresholdProb")
    UniformProbGenerator.generateRandom(1, false, repeatable)().asInstanceOf[Vector[Double]].head < thresholdProb

  def onDemandInt(repeatable:Boolean = true, pminv:Int = 0, pmaxv:Int = Int.MaxValue): Int =
    if pminv < 0 || pmaxv < 0 || pminv >= pmaxv then 
      logger.error(s"ondemand is called with incorrect parameters: pminv=$pminv, pmaxv=$pmaxv")
    val minv = if pminv < 0 then math.abs(pminv) else pminv
    val maxv = if pmaxv < 0 then math.abs(pmaxv) else pmaxv
    val genValue: Option[Int] = if minv < maxv then UniformProbGenerator.generateRandom(1, true, repeatable)(minv, maxv).asInstanceOf[Vector[Int]].headOption
      else if minv > maxv then UniformProbGenerator.generateRandom(1, true, repeatable)(maxv, minv).asInstanceOf[Vector[Int]].headOption
      else if minv == maxv && maxv < Int.MaxValue-1 then UniformProbGenerator.generateRandom(1, true, repeatable)(minv, maxv+1).asInstanceOf[Vector[Int]].headOption
      else if minv == maxv && minv > 0 then UniformProbGenerator.generateRandom(1, true, repeatable)(minv - 1, maxv).asInstanceOf[Vector[Int]].headOption
      else if minv == maxv then UniformProbGenerator.generateRandom(1, true, repeatable)(0,maxv).asInstanceOf[Vector[Int]].headOption
      else UniformProbGenerator.generateRandom(1, true, repeatable)(minv,maxv).asInstanceOf[Vector[Int]].headOption

    genValue match
      case Some(value) => value
      case None => logger.error(s"ondemand failed to create a random integer within the limits $minv and $maxv"); 0
  end onDemandInt
  def onDemandReal(repeatable: Boolean = true): Double =
    val v = UniformProbGenerator.generateRandom(1, false, repeatable)().asInstanceOf[Vector[Double]].headOption match
      case Some(value) => value
      case None => logger.error(s"ondemand failed to create a random real number"); 0.0d
//    logger.info(s"ondemand real generated $v")
    v
  end onDemandReal
  def randInts(howManyNumbers: Long, pminv:Int = 0, pmaxv:Int = Int.MaxValue)(using repeatable: Boolean = true): Vector[Int] =
    if pminv < 0 || pmaxv < 0 || pminv >= pmaxv then logger.error(s"randInts is called with incorrect parameters: pminv=$pminv, pmaxv=$pmaxv")
    val minv = if pminv < 0 then math.abs(pminv) else pminv
    val maxv = if pmaxv < 0 then math.abs(pmaxv) else pmaxv
    if minv < maxv then UniformProbGenerator.generateRandom(1, true, repeatable)(minv, maxv).asInstanceOf[Vector[Int]]
      else if minv > maxv then UniformProbGenerator.generateRandom(1, true, repeatable)(maxv, minv).asInstanceOf[Vector[Int]]
      else if minv == maxv && maxv < Int.MaxValue then UniformProbGenerator.generateRandom(1, true, repeatable)(minv, maxv + 1).asInstanceOf[Vector[Int]]
      else if minv == maxv && minv > 0 then UniformProbGenerator.generateRandom(1, true, repeatable)(minv - 1, maxv).asInstanceOf[Vector[Int]]
      else UniformProbGenerator.generateRandom(1, true, repeatable).asInstanceOf[Vector[Int]]
  end randInts
  def randProbs(howManyNumbers: Long)(repeatable: Boolean = true): Vector[Double] = UniformProbGenerator.generateRandom(howManyNumbers, false, repeatable)().asInstanceOf[Vector[Double]]
