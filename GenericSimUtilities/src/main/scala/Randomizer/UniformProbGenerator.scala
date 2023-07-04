package Randomizer

import Randomizer.SupplierOfRandomness.logger
import Utilz.NGSConstants.SEED
import Utilz.{CreateLogger, NGSConstants}
import org.slf4j.Logger

import java.lang.annotation.Repeatable
import java.util.concurrent.ThreadLocalRandom
import scala.util.{Random, Try}
import scala.collection.parallel.*
import scala.collection.parallel.CollectionConverters.*

trait Randomizer(seed: Option[Long]):
  protected val generator: Random = seed match
    case Some(s) => Random(s)
    case None => Random()


class UniformProbGenerator(val seed: Option[Long] = None) extends Randomizer(seed):
  val logger: Logger = CreateLogger(this.getClass)
  type GeneratedValues = Double | Int

  private def genFunc[T <: AnyVal](quantity: Int, gf: T=>T=>T, minv:Option[T] = None, maxv:Option[T]=None): List[T] =
    if quantity == 1 then List(if minv.isEmpty || maxv.isEmpty then gf(0d.asInstanceOf[T])(0d.asInstanceOf[T]) else gf(minv.get)(maxv.get))
    else (1 to quantity).toList.par.map(_ => if minv.isEmpty || maxv.isEmpty then gf(0d.asInstanceOf[T])(0d.asInstanceOf[T]) else gf(minv.get)(maxv.get)).toList

  private def generateUniformProbabilities(howMany:Int, repeatable: Boolean = true): List[Double] =
    if repeatable then
      synchronized {
        genFunc[Double](howMany, (i:Double)=>(j:Double)=>generator.nextDouble(), None, None)
      }
    else genFunc[Double](howMany, (i:Double)=>(j:Double)=>ThreadLocalRandom.current().nextDouble(), None, None)
  end generateUniformProbabilities

  private def generateInts(howMany:Int, repeatable: Boolean = true, minv:Int = 0, maxv:Int = Int.MaxValue): List[Int] =
    if repeatable then
      synchronized {
        genFunc[Int](howMany,
          (i:Int)=>(j:Int)=>generator.between(i,j),
          Some(minv), Some(maxv))
      }
    else
      genFunc[Int](howMany,
        (i:Int)=>(j:Int)=>ThreadLocalRandom.current().nextInt(i,j),
        Some(minv), Some(maxv))
  end generateInts

  private def uniformOrInts(howMany:Int, intsOrDouble: Boolean, repeatable: Boolean = true)(minv:Int = 0, maxv:Int = Int.MaxValue): List[GeneratedValues] =
    if howMany <= 0 then
      logger.error(s"Cannot generate $howMany values")
      List()
    else
      if intsOrDouble then generateInts(howMany,repeatable,minv,maxv) else generateUniformProbabilities(howMany,repeatable)

object UniformProbGenerator:
  private val seed: Option[Long] = Try(NGSConstants.globalConfig.getLong(SEED)) match {
    case scala.util.Success(value) => Some(value)
    case scala.util.Failure(fail) =>
      logger.error(s"Error while reading seed from config file: ${fail.getMessage}")
      None
  }
  private [this] val gen:Option[UniformProbGenerator] = Try(new UniformProbGenerator(seed)) match {
    case scala.util.Success(value) =>
      logger.info(s"Random value generator created with ${if seed.isEmpty then "no seed" else s"the seed $seed"}")
      Some(value)
    case scala.util.Failure(fail) =>
      logger.error(s"Failed to create a random value generator: ${fail.getMessage}")
      None
  }
  def generateRandom(howMany: Int, generateIntsYesOrNo: Boolean, repeatable: Boolean = true)(minv:Int = 0, maxv:Int = Int.MaxValue): List[Double|Int] =
    gen match
      case Some(g) =>
        val res = g.uniformOrInts(howMany, generateIntsYesOrNo, repeatable)(minv, maxv)
        logger.debug(s"Generated $howMany ${if generateIntsYesOrNo then "integers" else "doubles"}: $res repeatable = $repeatable with minv = $minv and maxv = $maxv")
        if generateIntsYesOrNo then res.asInstanceOf[List[Int]] else res.asInstanceOf[List[Double]]
      case None =>
        logger.error("Random generator not initialized: creating a new one with default values")
        List()
