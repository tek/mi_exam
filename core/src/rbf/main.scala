package tryp
package mi
package rbf

import simulacrum._

import breeze.linalg.sum
import breeze.numerics.abs

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

sealed trait BFParams

case class Params[P <: BFParams](weights: Col, bf: P)

@typeclass trait Initializer[P <: BFParams]
{
  def init(features: Int, centroids: Int): Params[P]
}

@typeclass trait UpdateParams[P <: BFParams]
{
  def update(params: Params[P], pred: Params[P], eta: Double): Params[P]
}

case class GaussParams(t: Mat, sigma: Col)
extends BFParams

object GaussParams
{
  def rand(features: Int, centroids: Int) = {
    GaussParams(Mat.rand(features, centroids), Col.rand(centroids))
  }

  implicit lazy val instance_Initializer_GaussParams
  : Initializer[GaussParams] =
    new Initializer[GaussParams] {
      def init(features: Int, centroids: Int) = {
        Params(Linalg.randWeightCol(centroids),
          GaussParams.rand(features, centroids))
      }
    }

  implicit lazy val instance_UpdateParams_GaussParams
  : UpdateParams[GaussParams] =
    new UpdateParams[GaussParams] {
      def update(params: Params[GaussParams], pred: Params[GaussParams],
        eta: Double) = {
          params
      }
    }
}

case class RBFLearnConf(centroids: Int, eta: Double, mode: LearnConf.LearnMode)

object RBFLearnConf
{
  def default(
    centroids: Int = 5,
    eta: Double = 0.3,
    mode: LearnMode = Batch
  ) = RBFLearnConf(centroids, eta, mode)
}

case class RState(output: Double)

object RState
{
  def init() = {
    RState(1.0)
  }
}

case class RBFPredictor[P <: BFParams](config: RBFLearnConf)
extends Predictor[Params[P], RState]
{
  def apply[A: Sample](sample: A, params: Params[P])
  : Prediction[A, Params[P], RState] = {
    val z = RState.init()
    Prediction(sample, params, z)
  }
}

case class TwoStep[P <: BFParams]
()
extends Optimizer[Params[P], RState]
{
  def apply[A: Sample](pred: Prediction[A, Params[P], RState]): Params[P] = {
    pred.param
  }
}

abstract class RBFStep[A: Sample, P <: BFParams]
extends EstimationStep[Params[P]]
{
  val config: RBFLearnConf

  val data: Nel[A]

  val eta = config.eta / data.length

  lazy val predict = RBFPredictor[P](config)

  lazy val optimize = TwoStep[P]()
}

case class BatchStep[A: Sample, P <: BFParams: UpdateParams](data: Nel[A],
  config: RBFLearnConf)
extends RBFStep[A, P]
{
  def apply(params: Params[P]): Params[P] = {
    val optimized = data map(a => optimize(predict(a, params)))
    optimized.foldLeft(params) { (z, pred) =>
      UpdateParams[P].update(z, pred, eta)
    }
  }
}

case class OnlineStep[A: Sample, P <: BFParams: UpdateParams]
(data: Nel[A], config: RBFLearnConf)
extends RBFStep[A, P]
{
  def apply(params: Params[P]): Params[P] = {
    params
  }
}

case class RBFEstimator[A: Sample, P <: BFParams: Initializer: UpdateParams]
(data: Nel[A], config: RBFLearnConf)
extends Estimator[Params[P]]
{
  val featureCount = data.head.feature.length

  lazy val initialParams = Initializer[P].init(featureCount, config.centroids)

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

case class RBFValidator[A: Sample, P <: BFParams]
(data: Nel[A], config: RBFLearnConf)
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

case class RBFConvergenceStopCriterion[P <: BFParams]
(count: Long, epsilon: Double)
extends ConvergenceStopCriterion[Params[P]]
{
  def diff(params: Params[P], prev: Params[P]) = {
    val a = params.weights
    sum(abs(a :- prev.weights)) / a.size
  }
}
