package tryp
package mi
package rbf

import simulacrum._

import scalaz.std.vector.{vectorInstance => zVectorInstance}
import scalaz.syntax.zip._

import monocle.macros.Lenses
import monocle.function._
import monocle.syntax.apply._

import spire.math.exp
import spire.algebra._
import spire.implicits._
import spire.random._

import breeze.linalg.{sum, squaredDistance, pinv}
import breeze.linalg.functions.euclideanDistance
import breeze.numerics.abs
import breeze.generic.UFunc

import LearnConf._

object Linalg
{
  def randWeightMat(a: Int, b: Int): Mat = {
    Mat.rand(b, a) :* (1.0 / a)
  }

  def randWeightCol(a: Int): Col = {
    Col.rand(a) :* (1.0 / a)
  }
}

@typeclass trait BasisFunction[P]
{
  def center(p: P): Col
  def output(p: P)(input: Col): Double
}

import BasisFunction.ops._

case class Params[P: BasisFunction](weights: Col, bf: Nev[P])
{
  def centers = bf map(_.center)
}

@typeclass abstract class Initializer[P: BasisFunction]
extends AnyRef
{
  def init[A: Sample](features: Int, centroids: Int): Params[P]
}

abstract class Initialization[P: Initializer: BasisFunction]
{
  def create(features: Int, centroids: Int): Params[P]
}

class RandomInitialization[P: Initializer: BasisFunction, A: Sample]
extends Initialization[P]
{
  def create(features: Int, centroids: Int) =
    Initializer[P].init[A](features, centroids)
}

class ManualInitialization[P: Initializer: BasisFunction]
(manual: Params[P])
extends Initialization[P]
{
  def create(features: Int, centroids: Int) = manual
}

@typeclass abstract class UpdateParams[P: BasisFunction]
extends AnyRef
{
  def updateCenter(index: Int, diff: Col): Nev[P] => Nev[P]

  def updateParams(a: P)(all: Nev[P], lambda: Double): P
}

@Lenses
case class GaussBF(center: Col, sigma: Double)
extends UFunc
{
  lazy val sigmaSquared = sigma * sigma

  implicit val colImpl = new Impl[Col, Double] {
    def apply(a: Col) = GaussBF.main(a, center, sigmaSquared)
  }
}

trait GaussBFInstances
{
  implicit lazy val instance_Initializer_GaussBF
  : Initializer[GaussBF] =
    new Initializer[GaussBF] {
      def init[A: Sample](features: Int, centroids: Int) = {
        Params(Linalg.randWeightCol(centroids),
          GaussBF.rand[A](features, centroids))
      }
    }

  implicit lazy val instance_UpdateParams_GaussBF
  : UpdateParams[GaussBF] =
    new UpdateParams[GaussBF] {
      def update(params: Params[GaussBF], pred: Params[GaussBF],
        eta: Double) = {
          params
      }

      def updateCenter(index: Int, diff: Col) =
        (nevIndex[GaussBF].index(index) ^|-> GaussBF.center).modify(_ + diff)

      def updateParams(bf: GaussBF)(all: Nev[GaussBF], lambda: Double)
      : GaussBF = {
        val dist = all.map {
          case a if a != bf => euclideanDistance(bf.center, a.center)
          case _ => Double.MaxValue
        }
        val newSigma = dist.unwrap.min * lambda
        (bf &|-> GaussBF.sigma).set(newSigma)
      }
    }

  implicit lazy val instance_BasisFunction_GaussBF
  : BasisFunction[GaussBF] =
    new BasisFunction[GaussBF] {
      def center(a: GaussBF) = a.center

      def output(a: GaussBF)(input: Col): Double = {
        import a._
        a(input)
      }
    }
}

object GaussBF
extends GaussBFInstances
{
  def rand[A: Sample](features: Int, centroids: Int): Nev[GaussBF] = {
    def inst = GaussBF(Col.rand(features) * Sample[A].range,
      tryp.Random.double())
    centroids gen inst match {
      case head :: tail => Nev(head, tail: _*)
      case a => Nev(inst)
    }
  }

  def main(sample: Col, center: Col, variance: Double) = {
    exp(-squaredDistance(sample, center) / variance)
  }
}

case class RBFLearnConf[P: BasisFunction](steps: Int, centroids: Int,
  eta: Double, lambda: Double, mode: LearnConf.LearnMode,
  initialization: Initialization[P])
  {
    lazy val bf = BasisFunction[P]

    def initialParams(featureCount: Int) =
      initialization.create(featureCount, centroids)
  }

object RBFLearnConf
{
  def default[P: BasisFunction: Initializer, A: Sample](
    steps: Int = 10,
    centroids: Int = 3,
    eta: Double = 0.3,
    lambda: Double = 2.0,
    mode: LearnMode = Batch,
    initialization: Option[Initialization[P]] = None
  ) =
    RBFLearnConf[P](steps, centroids, eta, lambda, mode,
      initialization | new RandomInitialization[P, A])
}

case class RState(output: Double)

object RState
{
  def init(output: Double) = RState(output)
}

case class RBFPredictor[P: BasisFunction](config: RBFLearnConf[P])
extends Predictor[Params[P], RState]
{
  def rbfOut[A: Sample](data: Col, bf: Nev[P]): Nev[Double] = {
    bf.map(_.output(data))
  }

  def apply[A: Sample](sample: A, params: Params[P])
  : Prediction[A, Params[P], RState] = {
    val r = Col(rbfOut(sample.feature, params.bf).unwrap.toArray)
    val z = RState.init(params.weights.t * r)
    Prediction(sample, params, z)
  }
}

case class TwoStep[P: BasisFunction]
()
extends Optimizer[Params[P], RState]
{
  def apply[A: Sample](pred: Prediction[A, Params[P], RState]): Params[P] = {
    pred.param
  }
}

abstract class RBFStep[A: Sample, P: BasisFunction: UpdateParams]
extends EstimationStep[Params[P]]
{
  import UpdateParams.ops._

  val config: RBFLearnConf[P]

  val data: Nel[A]

  lazy val features = data map(_.feature)

  lazy val targets = Col(data map(_.value) unwrap: _*)

  val eta = config.eta / data.length

  lazy val predict = RBFPredictor[P](config)

  lazy val optimize = TwoStep[P]()

  /* @return the Col in `t` and its index in `t` that is closest to `z`
   * by euclidean distance metrics
   */
  def closest(z: Col, t: Nev[Col]): (Col, Int) = {
    val c = t
      .map(euclideanDistance(z, _))
      .fzip(t)
      .fzip(Nev(0, (1 until t.length): _*))
      .reduceLeft((a, b) => if(a._1._1 < b._1._1) a else b)
    (c._1._2, c._2)
  }

  def updateCenter(z: Nev[P], sample: Col) = {
    val (tq, index) = closest(sample, z.map(_.center))
    val diff = (sample - tq) * eta
    UpdateParams[P].updateCenter(index, diff)(z)
  }

  def moveCenters(params: Nev[P]): Nev[P] = {
    features.foldLeft(params)(updateCenter)
  }

  def updateBf(params: Nev[P]): Nev[P] = {
    val moved = moveCenters(params)
    moved.map(_.updateParams(moved, config.lambda))
  }

  def updateWeights(bf: Nev[P]): Params[P] = {
    val out = features.map(predict.rbfOut(_, bf).unwrap.toArray).unwrap
    val phi = Mat(out: _*)
    val trans = phi.t
    val leftCoeff = trans * phi
    val rightCoeff = trans * targets
    val weights = pinv(leftCoeff) * rightCoeff
    Params(weights, bf)
  }
}

case class BatchStep[A: Sample, P: BasisFunction: UpdateParams](data: Nel[A],
  config: RBFLearnConf[P])
extends RBFStep[A, P]
{
  def apply(params: Params[P]): Params[P] = {
    updateWeights(updateBf(params.bf))
  }
}

case class OnlineStep[A: Sample, P: BasisFunction: UpdateParams]
(data: Nel[A], config: RBFLearnConf[P])
extends RBFStep[A, P]
{
  def apply(params: Params[P]): Params[P] = {
    params
  }
}

case class RBFEstimator[A: Sample, P: BasisFunction: Initializer: UpdateParams]
(data: Nel[A], config: RBFLearnConf[P])
extends Estimator[Params[P]]
{
  val featureCount = data.head.feature.length

  lazy val initialParams = config.initialParams(featureCount)

  lazy val step: RBFStep[A, P] = {
    if (config.mode == LearnConf.Batch) BatchStep(data, config)
    else OnlineStep(data, config)
  }
}

case class RBFValidation[A](data: A, pred: RState)(implicit sample: Sample[A])
extends SampleValidation[A, RState]
{
  lazy val predictedValue = pred.output

  lazy val predictedClass = sample.predictedClass(predictedValue)

  def actualClass = data.cls

  def success = actualClass == predictedClass

  def successInfo = {
    if (success) s"correct class"
    else s"wrong class: $predictedClass ($predictedValue)"
  }

  def info =
    s"${actualClass} (${data.feature.data.mkString(", ")}): $successInfo"

  def error(cost: Func2) = {
    cost.f(data.value, pred.output)
  }
}

case class RBFValidator[A: Sample, P: BasisFunction]
(data: Nel[A], config: RBFLearnConf[P])
extends Validator[A, Params[P], RState]
{
  lazy val predict = RBFPredictor[P](config)

  def verify(params: Params[P])(sample: A): SampleValidation[A, RState] = {
    val pred = predict(sample, params)
    RBFValidation(sample, pred.pred)
  }

  def run(params: Params[P]) = {
    val pred = data map(verify(params))
    Validation(pred)
  }
}

case class RBFConvergenceStopCriterion[P: BasisFunction]
(count: Long, epsilon: Double)
extends ConvergenceStopCriterion[Params[P]]
{
  def diff(params: Params[P], prev: Params[P]) = {
    val a = params.weights
    sum(abs(a :- prev.weights)) / a.size
  }
}
