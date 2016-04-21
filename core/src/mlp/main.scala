package tryp
package mi
package mlp

import scalaz.std.list.{listInstance => zListInstance}
import scalaz.syntax.zip._

import cats._, data.{Func => _, _}

import spire.math._
import spire.algebra._
import spire.implicits._
import spire.random._

import breeze.linalg.Transpose
import breeze.generic.{UFunc, MappingUFunc}
import breeze.linalg.sum
import breeze.numerics.abs
import UFunc.UImpl

object Logistic
{
  def main(a: Double, beta: Double) = 1.0 / (1.0 + exp(-beta * a))
}

case class Logistic(beta: Double)
extends DFunc[LogisticDeriv]
{
  implicit val doubleImpl = new DI {
    def apply(a: Double) = Logistic.main(a, beta)
  }

  lazy val deriv = new LogisticDeriv(beta)
}

class LogisticDeriv(beta: Double)
extends DFunc[NullFuncBase]
{
  implicit val doubleImpl = new DI {
    def apply(a: Double) = {
      val fx = Logistic.main(a, beta)
      fx * (1 - fx)
    }
  }

  def deriv = NullFunc
}

case class LayerState(in: Col, out: Col)

case class PState(layers: Nel[LayerState])
{
  def addLayer(in: Col, out: Col) =
    copy(layers.combine(Nel(LayerState(in, out))))

  def in = layers.map(_.in)

  def out = layers.map(_.out)

  def output = layers.last.out(0)

  def value = output
}

object PState
{
  def init[T <: Func](out: Col, tfDeriv: T) = {
    PState(Nel(LayerState(tfDeriv.f(out), out)))
  }
}

trait WeightInitializer
{
  def apply(features: Int, layers: Nel[Int], output: Int): Weights
}

object RandomWeights
extends WeightInitializer
{
  def apply(features: Int, layers: Nel[Int], output: Int): Weights = {
    ((features :: layers.unwrap) :+ output)
      .sliding(2)
      .collect {
        case List(a, b) =>
          Mat.rand(b, a) :* (1.0 / a)
      }
      .toList match {
        case List(a, b @ _*) => Nel(a, b: _*)
        case _ => Nel(Mat.fill(1, 1)(1.0))
      }
  }
}

class ManualWeights(weights: Weights)
extends WeightInitializer
{
  def apply(features: Int, layers: Nel[Int], output: Int): Weights = {
    weights
  }
}

case class MLPLearnConf
(transfer: DFunc[_ <: Func], eta: Double, layers: Nel[Int], steps: Int,
  initializer: WeightInitializer, cost: DFunc2[_ <: Func2], bias: Boolean,
  mode: LearnConf.LearnMode)

object MLPLearnConf
{
  import LearnConf._

  def default(
    transfer: DFunc[_ <: Func] = Logistic(0.5),
    eta: Double = 0.8,
    layers: Nel[Int] = Nel(4, 3),
    steps: Int = 1000,
    initializer: WeightInitializer = RandomWeights,
    cost: DFunc2[_ <: Func2] = QuadraticError,
    bias: Boolean = true,
    mode: LearnMode = Batch
  ) =
      MLPLearnConf(transfer, eta, layers, steps, initializer, cost, bias, mode)
}

case class MLPPredictor(config: MLPLearnConf)
extends Predictor[Weights, PState]
{
  def transfer = config.transfer

  def input[A: Sample](sample: A) = {
    if (config.bias) Col.vertcat(Col(1d), sample.feature)
    else sample.feature
  }

  def layer(state: PState, w: Mat) = {
    val in = w * state.layers.last.out
    val out = transfer.f(in)
    state.addLayer(in, out)
  }

  def apply[A: Sample](sample: A, weights: Weights)
  : Prediction[A, Weights, PState] = {
    val z = PState.init(input(sample), transfer.deriv)
    Prediction(sample, weights, weights.foldLeft(z)(layer))
  }
}

case class BackProp
(transfer: DFunc[_ <: Func], cost: DFunc2[_ <: Func2])
extends Optimizer[Weights, PState]
{
  lazy val deriv = transfer.deriv

  def backprop(state: PState, weights: Weights): Layers = {
    val hPrime = state.in.map(deriv.f(_)).reverse
    weights
      .toList
      .reverse
      .zip(hPrime.tail)
      .init
      .foldLeft(Nel(hPrime.head)) {
        case (z, (w, h)) =>
          val d = h :* (w.t * z.head)
          OneAnd(d, z.unwrap)
      }
  }

  def modelGradient(state: PState, deltas: Layers): Weights = {
    deltas
      .fzip(state.out)
      .map { case (a, b) => a * b.t }
  }

  def apply[A: Sample](pred: Prediction[A, Weights, PState]): Weights = {
    val state = pred.pred
    val back = backprop(state, pred.param)
    val mg = modelGradient(state, back)
    val ce = cost.deriv.f(pred.sample.value, state.output)
    mg.map(_ * ce)
  }
}

abstract class MLPStep[A: Sample]
extends EstimationStep[Weights]
{
  val config: MLPLearnConf

  val data: Nel[A]

  val eta = config.eta / data.length

  lazy val predict = MLPPredictor(config)

  lazy val optimize = BackProp(config.transfer, config.cost)
}

// TODO parallel computing
case class BatchStep[A: Sample](data: Nel[A], config: MLPLearnConf)
extends MLPStep
{
  def apply(weights: Weights): Weights = {
    val optimized = data map(a => optimize(predict(a, weights)))
    optimized.foldLeft(weights) { (z, pred) =>
      z.fzipWith(pred) { (a, b) => a :- (b * eta) }
    }
  }
}

case class OnlineStep[A: Sample](data: Nel[A], config: MLPLearnConf)
extends MLPStep
{
  def apply(weights: Weights): Weights = {
    data.foldLeft(weights) { (z, sample) =>
      val o = optimize(predict(sample, z))
      z.fzipWith(o) { (a, b) => a :- (b * eta) }
    }
  }
}

case class MLPEstimator[A: Sample]
(data: Nel[A], config: MLPLearnConf)
extends Estimator[A, Weights]
{
  lazy val initialParams = config.initializer(featureCount, config.layers, 1)

  lazy val step: MLPStep[A] = {
    if (config.mode == LearnConf.Batch) BatchStep(data, config)
    else OnlineStep(data, config)
  }
}

case class MLPValidation[A](data: A, pred: PState)(implicit sample: Sample[A])
extends SampleValidation[A, PState]
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

case class MLPValidator[A: Sample]
(data: Nel[A], config: MLPLearnConf)
extends Validator[A, Weights, PState]
{
  lazy val predict = MLPPredictor(config)

  def verify(weights: Weights)(sample: A): SampleValidation[A, PState] = {
    val pred = predict(sample, weights)
    MLPValidation(sample, pred.pred)
  }

  def run(weights: Weights) = {
    val pred = data map(verify(weights))
    Validation(pred)
  }
}

case class MLPConvergenceStopCriterion(count: Long, epsilon: Double)
extends StopCriterion[Nel[Mat]]
{
  val steps = StepCountStopCriterion[Nel[Mat]](count)

  def apply(iteration: Long, params: Nel[Mat], prev: Option[Nel[Mat]]) = {
    steps(iteration, params, prev) ||
      prev.exists(a => diff(params, a) < epsilon)
  }

  def diff(params: Nel[Mat], prev: Nel[Mat]) = {
      params.unwrap.zip(prev.unwrap)
        .map { case (a, b) => sum(abs(a :- b)) / a.size }
        .sum
  }
}
