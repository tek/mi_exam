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

import imp.imp

import breeze.linalg.{DenseVector, DenseMatrix, Transpose}
import breeze.generic.{UFunc, MappingUFunc}
import UFunc.UImpl

import Sample.ops._

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
          DenseMatrix.rand(b, a) :* (1.0 / a)
      }
      .toList match {
        case List(a, b @ _*) => Nel(a, b: _*)
        case _ => Nel(DenseMatrix.fill(1, 1)(1.0))
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

case class TrainConf
(transfer: DFunc[_ <: Func], eta: Double, layers: Nel[Int], steps: Int,
  initializer: WeightInitializer, cost: DFunc2[_ <: Func2], bias: Boolean)

case class MLPPredictor(config: TrainConf)
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

case class MLPTrainer[A: Sample]
(data: Nel[A], config: TrainConf)
extends Trainer[Weights]
{
  lazy val predict = MLPPredictor(config)

  lazy val optimize = BackProp(config.transfer, config.cost)

  val featureCount = {
    data.head.feature.length + (if (config.bias) 1 else 0)
  }

  lazy val initialParams = config.initializer(featureCount, config.layers, 1)

  val eta = config.eta / data.length

  // TODO parallel computing
  def step(weights: Weights): Weights = {
    val optimized = data map(a => optimize(predict(a, weights)))
    optimized.foldLeft(weights) { (z, pred) =>
      z.fzipWith(pred) { (a, b) => a :- (b * eta) }
    }
  }
}

case class MLPValidation[A](data: A, pred: PState)(implicit sample: Sample[A])
extends SampleValidation[A, PState]
{
  lazy val predictedValue = pred.output

  lazy val predictedClass = sample.predictedClass(predictedValue)

  def success = data.cls == predictedClass

  def successInfo = {
    if (success) s"correct class"
    else s"wrong class: $predictedClass ($predictedValue)"
  }

  def info = s"${data.cls} (${data.feature.data.mkString(", ")}): $successInfo"
}

case class MLPValidator[A: Sample]
(data: Nel[A], config: TrainConf)
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
