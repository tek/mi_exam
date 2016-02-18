package tryp
package mi
package mlp

import scalaz.std.list.{listInstance => zListInstance}
import scalaz.syntax.zip._

import cats._, data._, syntax.all._, std.all._

import spire.math._
import spire.algebra._
import spire.implicits._
import spire.random._

import imp.imp

import breeze.linalg.{DenseVector, DenseMatrix, Transpose}
import breeze.generic.{UFunc, MappingUFunc}

trait Transfer
extends UFunc
with MappingUFunc
{
  implicit val doubleImpl: UFunc.UImpl[this.type, Double, Double]
}

trait TransferMain
extends Transfer
{
  def deriv: Transfer
}

object Logistic
{
  def main(a: Double, beta: Double) = 1.0 / (1.0 + exp(-beta * a))
}

class Logistic(beta: Double)
extends TransferMain
{
  implicit val doubleImpl = new Impl[Double, Double] {
    def apply(a: Double) = Logistic.main(a, beta)
  }

  lazy val deriv = new LogisticDeriv(beta)
}

object Identity
extends TransferMain
{
  implicit val doubleImpl = new Impl[Double, Double] {
    def apply(a: Double) = a
  }

  lazy val deriv = Identity
}

class LogisticDeriv(beta: Double)
extends Transfer
{
  implicit val doubleImpl = new Impl[Double, Double] {
    def apply(a: Double) = {
      val fx = Logistic.main(a, beta)
      fx * (1 - fx)
    }
  }
}

trait TrainData
{
  def feature: DenseVector[Double]
  def target: Double
}

case class LayerState(in: Col, out: Col)

case class PState(layers: Nel[LayerState])
{
  def addLayer(in: Col, out: Col) =
    copy(layers.combine(Nel(LayerState(in, out))))

  def in = layers.map(_.in)

  def out = layers.map(_.out)

  def output = layers.last.out(0)
}

object PState
{
  def init(out: Col, tfDeriv: Transfer) = {
    import tfDeriv._
    PState(Nel(LayerState(tfDeriv(out), out)))
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

case class TrainConf(transfer: TransferMain, beta: Double, eta: Double,
  layers: Nel[Int], steps: Int, initializer: WeightInitializer)

case class PointTrain[A <: TrainData](data: A)(implicit config: TrainConf)
{
  val transfer = config.transfer

  val deriv = transfer.deriv

  def forwardprop(weights: Weights): PState = {
    import transfer._
    weights.foldLeft(PState.init(data.feature, transfer.deriv)) {
      case (state, w) =>
        val in = w * state.layers.last.out
        val out = transfer(in)
        state.addLayer(in, out)
    }
  }

  def backprop(state: PState, weights: Weights): Layers = {
    import deriv._
    val hPrime = state.in.map(deriv(_)).reverse
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
      .fzip(state.in)
      .map { case (a, b) => a * b.t }
  }

  def costError(state: PState) = {
    data.target - state.output
  }

  def gradient(weights: Weights): Weights = {
    val state = forwardprop(weights)
    val deltas = backprop(state, weights)
    val mg = modelGradient(state, deltas)
    val ce = costError(state)
    mg.map(_ * ce)
  }
}

case class Train[A <: TrainData](data: Nel[A])
(implicit config: TrainConf)
{
  val featureCount = data.head.feature.length

  lazy val points = data map(PointTrain(_))

  def initialWeights = config.initializer(featureCount, config.layers, 1)

  // TODO parallel computing
  def step(weights: Weights): Weights = {
    points.foldLeft(weights) { (z, point) =>
      z.fzipWith(point.gradient(weights)) { (a, b) => a + (b * config.eta) }
    }
  }

  def run = {
    0.until(config.steps)
      .foldLeft(initialWeights) { case (w, _) => step(w) }
  }
}
