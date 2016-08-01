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
        a.unwrap.zip(b.unwrap)
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

trait WeightInitializer
{
  def apply(layers: Nel[Int]): Weights
}

object RandomWeights
extends WeightInitializer
{
  def apply(layers: Nel[Int]): Weights =
    layers
      .unwrap
      .sliding(2)
      .collect {
        case List(a, b) =>
          Mat.rand(b, a) :* (1.0 / a)
      }
      .toList
      .nelOption | Nel(Mat.fill(1, 1)(1.0))
}

class ManualWeights(weights: Weights)
extends WeightInitializer
{
  def apply(layers: Nel[Int]): Weights = weights
}

case class MLPLearnConf (
  transfer: DFunc[_ <: Func],
  eta: Double,
  hidden: Nel[Int],
  initializer: WeightInitializer,
  cost: DFunc2[_ <: Func2],
  bias: Boolean,
  learnMode: LearnConf.LearnMode,
  gradientMode: MLPLearnConf.GradientMode,
  output: Int
)
{
  def in(rank: Int) = rank + (if (bias) 1 else 0)

  def inLayers(rank: Int) = Nel(in(rank)).combine(hidden)

  def layers(rank: Int) =
    inLayers(rank).combine(Nel(output))

  def initialParams(rank: Int) = initializer(layers(rank))
}

object MLPLearnConf
{
  import LearnConf._

  def default(
    transfer: DFunc[_ <: Func] = Logistic(0.5),
    eta: Double = 0.8,
    hidden: Nel[Int] = Nel(2),
    initializer: WeightInitializer = RandomWeights,
    cost: DFunc2[_ <: Func2] = QuadraticError,
    bias: Boolean = true,
    learnMode: LearnMode = Batch,
    gradientMode: GradientMode = TrivialGradient,
    output: Int = 1
  ) =
    MLPLearnConf(transfer, eta, hidden, initializer, cost, bias, learnMode,
      gradientMode, output)

  sealed trait GradientMode

  case object TrivialGradient
  extends GradientMode

  case object ConjugateGradient
  extends GradientMode
}
