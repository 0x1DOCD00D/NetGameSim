package Utilz

import Utilz.NGSConstants.MAXWALKPATHLENGTHCOEFFDEFAULT
import com.typesafe.config.{Config, ConfigFactory}

import java.text.SimpleDateFormat
import java.util.Date
import scala.util.Failure

object NGSConstants:
  private val config: Config = ConfigFactory.load()
  case class EnumeratedLoopParameters(ps: List[Double])
  case class FromToWithStepParameters(from: Double, to: Double, step: Double)

  val SEED: String = "seed"
  val CONFIGENTRYNAME: String = "NGSimulator"
  val MODELCONFIGENTRYNAME: String = "NetModel"
  val COSTREWARDSCONFIGENTRYNAME: String = "CostRewards"

  val EPSILON: Double = 1E-3d
  val DOPPLEGANGERS: String = "dopplegangers"
  val DOPPLEGANGERSDEFAULT: Int = 5
  val EDGEPROBABILITY: String = "edgeProbability"
  val DEFAULTEDGEPROBABILITY: Double = 0.3d
  val DISTANCESPREADTHRESHOLD: String = "distanceSpreadThreshold"
  val DEFAULTDISTANCESPREADTHRESHOLD: Double = 0.05d
  val PERTURBATIONCOEFFICIENT: String = "perturbationCoefficient"
  val DEFAULTPERTURBATIONCOEFFICIENT: Double = 0.2d
  val DISSIMULATIONCOEFFICIENT: String = "dissimulationCoefficient"
  val DEFAULTDISSIMULATIONCOEFFICIENT: Double = 0.1d
  val DISTANCECOEFFICIENT: String = "distanceCoefficient"
  val DEFAULTDISTANCECOEFFICIENT: Double = 0.1d
  val NUMBEROFEXPERIMENTS: String = "numberOfExperiments"
  val NUMBEROFEXPERIMENTSDEFAULT: Int = 10
  val STATESTOTAL: String = "statesTotal"
  val STATESTOTALDEFAULT: Int = 30
  val WALKS: String = "numberOfWalks"
  val WALKSDEFAULT: Int = 1500
  val MAXBRANCHINGFACTOR = "maxBranchingFactor"
  val MAXBRANCHINGFACTORDEFAULT = 7
  val MAXDEPTH = "maxDepth"
  val MAXDEPTHDEFAULT = 5
  val MAXPROPERTIES = "maxProperties"
  val MAXPROPERTIESDEFAULT = 20
  val PROPVALUERANGE = "propValueRange"
  val PROPVALUERANGEDEFAULT = 100
  val ACTIONRANGE = "actionRange"
  val ACTIONRANGEDEFAULT = 10
  val CONNECTEDNESS = "connectedness"
  val CONNECTEDNESSDEFAULT = 28
  val MAXWALKPATHLENGTHCOEFF = "maxWalkPathLengthCoeff"
  val MAXWALKPATHLENGTHCOEFFDEFAULT = 1.2d
  val GRAPHWALKTERMINATIONPOLICYDEFAULT = "maxpathlength"
  val GRAPHWALKTERMINATIONPOLICY = "graphWalkTerminationPolicy"
  val GRAPHWALKNODETERMINATIONPROBABILITY = "graphWalkNodeTerminationProbability"
  val GRAPHWALKNODETERMINATIONPROBABILITYDEFAULT = 0.05d
  val OUTPUTDIRECTORY = "outputDirectory"
  def OUTPUTFILENAME: String =
    val currentDate = new Date(System.currentTimeMillis())
    val df = new SimpleDateFormat("dd-MM-yy-HH-mm-ss")
    "NetGraph_" + df.format(currentDate) + ".txt"

  val MALAPPBUDGET = "malAppBudget"
  val MALAPPBUDGETDEFAULT = 100.0d
  val COSTOFDETECTION = "costOfDetection"
  val COSTOFDETECTIONDEFAULT = 0.5d
  val SERVICEREWARD = "serviceReward"
  val SERVICEREWARDDEFAULT = 1.3d
  val SERVICEPENALTY = "servicePenalty"
  val SERVICEPENALTYDEFAULT = 2.3d
  val TARGETAPPSCORE = "targetAppScore"
  val TARGETAPPSCOREDEFAULT = 100.0d
  val TARGETAPPLOWPENALTY = "targetAppLowPenalty"
  val TARGETAPPLOWPENALTYDEFAULT = 0.2d
  val TARGETAPPHIGHPENALTY = "targetAppHighPenalty"
  val TARGETAPPHIGHPENALTYDEFAULT = 0.5d
  val SERVICEREWARDPROBABILITY = "serviceRewardProbability"
  val SERVICEREWARDPROBABILITYDEFAULT = 0.5d

  val globalConfig: Config = obtainConfigModule(config, CONFIGENTRYNAME)

  val configNetGameModel: Config = obtainConfigModule(globalConfig, MODELCONFIGENTRYNAME)

  val configCostRewards: Config = obtainConfigModule(globalConfig, COSTREWARDSCONFIGENTRYNAME)

  def obtainConfigModule(cf: Config, moduleName: String): Config = scala.util.Try(cf.getConfig(moduleName)) match {
    case scala.util.Success(cfg) => cfg
    case Failure(exception) => throw new Exception(s"No config entry found for $moduleName: ${exception.getMessage}")
  }