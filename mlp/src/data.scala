package tryp
package mi
package mlp

import breeze._
import linalg._
import numerics._

case class LayerState(in: Col, out: Col)

case class Layers(hidden: Nel[LayerState])
{
  def addLayer(in: Col, out: Col) =
    copy(hidden.combine(Nel(LayerState(in, out))))

  def in = hidden.map(_.in)

  def out = hidden.map(_.out)

  def output = hidden.last.out(0)

  def value = output

  def last = hidden.last
}

case class MLP(weights: Weights, hidden: Layers)
{
  def addLayer(in: Col, out: Col) =
    copy(hidden = hidden.addLayer(in, out))

  def in = hidden.in

  def out = hidden.out

  def output = hidden.output
}

object MLP
{
  def init[T <: Func](out: Col, tfDeriv: T, weights: Weights) = {
    MLP(weights, Layers(Nel(LayerState(tfDeriv.f(out), out))))
  }

  implicit def instance_ModelState_MLP: ModelState[MLP] =
    new ModelState[MLP] {
      def output(a: MLP) = a.output
    }

  implicit def instance_ParamDiff_Weights: ParamDiff[Weights] =
    new ParamDiff[Weights] {
      def diff(a: Weights, b: Weights) =
        a.tail.zip(b.tail)
          .map { case (a, b) => sum(abs(a :- b)) / a.size }
          .sum
    }

  implicit def instance_CreateEstimator_MLP[S: Sample: MC]
  : CreateEstimator[S, Weights, MLPLearnConf] =
    new CreateEstimator[S, Weights, MLPLearnConf] {
      def apply(data: Nel[S])
      (implicit conf: MLPLearnConf, sconf: MSConf)
      : Estimator[Weights] = {
        val stop = ParamDiffStopCriterion[Weights](sconf.steps, sconf.epsilon)
        MLPEstimator(data, conf, stop)
      }
    }

  implicit def instance_CreateModelCreator_MLP[S] =
    new CreateModelCreator[S, Weights, MLP, MLPLearnConf] {
      def apply(data: Nel[S])
      (implicit conf: MLPLearnConf, sconf: MSConf)
      =
        MLPModelCreator(conf)
    }

  implicit def instance_CreateValidator_MLP[S: Sample: MC]
  : CreateValidator[S, MLP, MLPLearnConf] =
    new CreateValidator[S, MLP, MLPLearnConf] {
      def apply(data: Nel[S])
      (implicit conf: MLPLearnConf, sconf: MSConf)
      : Validator[MLP] = MLPValidator(data, conf)
    }
}

object Weights
{
  sealed trait InitMode
  case object Random extends InitMode
  case class Manual(w: Weights) extends InitMode
  case object Annealing extends InitMode
}

trait WeightInitializer
{
  def apply(layers: Nel[Int]): Weights
}

object RandomWeights
extends WeightInitializer
{
  def apply(layers: Nel[Int]): Weights =
    layers
      .tail
      .sliding(2)
      .collect {
        case List(a, b) =>
          Mat.rand(b, a) :* (1.0 / a)
      }
      .toList
      .nelOption | Nel(Mat.fill(1, 1)(1.0))
}

case class ManualWeights(weights: Weights)
extends WeightInitializer
{
  def apply(layers: Nel[Int]): Weights = weights
}

case class AnnealedWeights[S: Sample]
(estimator: MLPEstimator[S])
(implicit mc: MC[S])
extends WeightInitializer
{
  val k1 = 4

  val k2 = 4

  val k3 = 3

  val a0 = 0

  val a = 0.5

  def apply(layers: Nel[Int]): Weights = {
    val w: Weights = RandomWeights(layers)
    val wm = Weights.Manual(w)
    val est = estimator.copy(config = estimator.config.copy(initMode = wm))
    w
  }
}

case class MLPLearnConf (
  rank: Int,
  transfer: DFunc[_ <: Func],
  eta: Double,
  hidden: Nel[Int],
  cost: DFunc2[_ <: Func2],
  bias: Boolean,
  initMode: Weights.InitMode,
  learnMode: LearnConf.LearnMode,
  gradientMode: MLPLearnConf.GradientMode,
  output: Int
)
{
  def in = rank + (if (bias) 1 else 0)

  def inLayers = Nel(in).combine(hidden)

  def layers = inLayers.combine(Nel(output))
}

object MLPLearnConf
{
  import LearnConf._

  def default(
    rank: Int,
    transfer: DFunc[_ <: Func] = Logistic(0.5),
    eta: Double = 0.8,
    hidden: Nel[Int] = Nel(2),
    cost: DFunc2[_ <: Func2] = QuadraticError,
    bias: Boolean = true,
    initMode: Weights.InitMode = Weights.Random,
    learnMode: LearnMode = Batch,
    gradientMode: GradientMode = TrivialGradient,
    output: Int = 1
  ) =
    MLPLearnConf(rank, transfer, eta, hidden, cost, bias, initMode,
      learnMode, gradientMode, output)

  sealed trait GradientMode

  case object TrivialGradient
  extends GradientMode

  case object ConjugateGradient
  extends GradientMode
}
