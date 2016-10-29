package tryp
package mi
package mlp

import cats.data.NonEmptyList

import breeze._
import linalg._

case class BackProp
(transfer: DFunc[_ <: Func], cost: DFunc2[_ <: Func2])
extends Optimizer[Weights, MLP, Double]
{
  lazy val deriv = transfer.deriv

  def backprop(state: MLP): Nel[Col] = {
    val hPrime = state.hidden.in.map(deriv.f(_)).reverse
    state.weights
      .toList
      .reverse
      .zip(hPrime.tail)
      .init
      .foldLeft(Nel(hPrime.head)) {
        case (z, (w, h)) =>
          (h :* (w.t * z.head)) :: z
      }
  }

  def modelGradient(state: MLP, deltas: Nel[Col]): Weights = {
    deltas
      .fzip(state.hidden.out)
      .map { case (a, b) => a * b.t }
  }

  def apply[S: Sample](pred: Prediction[S, MLP, Double])
  (implicit mc: MC[S]): Weights = {
    val state = pred.model
    val weights = state.weights
    val back = backprop(state)
    val mg = modelGradient(state, back)
    val ce = cost.deriv.f(pred.sample.valueOrNaN, state.output)
    mg.map(_ * ce)
  }
}

abstract class MLPStep[S: Sample]
(implicit mc: MC[S])
extends EstimationStep[Weights]
{
  val config: MLPLearnConf

  val data: Nel[S]

  val eta = config.eta / data.length

  lazy val predict = MLPPredictor(config)

  lazy val optimize = BackProp(config.transfer, config.cost)

  lazy val validator = MLPValidator(data, config)

  def error(weights: Col): Double =
    validator.error(reshapeToWeights(weights)) getOrElse Double.NaN

  def gradient(weights: Weights): Weights = {
    val r: Nel[Weights] = data map (a => optimize(predict(a, weights)))
    r.reduceLeft { (a, b) => a fzip b map { case (a, b) => a + b } }
  }

  def colGradient(weights: Col): Col =
    reshapeWeights(gradient(reshapeToWeights(weights)))

  def reshapeWeights(w: Weights): Col = {
    w map (_.toDenseVector) reduceLeft (Col.vertcat(_, _))
  }

  def reshapeToWeights(w: Col): Weights = {
    def go(ww: Col, index: (Int, Int)) = {
      val (i, j) = index
      val num = i * j
      ww(num to -1) -> Mat.create(j, i, ww(0 until num).toArray)
    }
    def init(index: (Int, Int)) = {
      val (ww, mat) = go(w, index)
      ww -> Nel(mat)
    }
    val data = counts.reduceLeftTo(init) {
      case ((rest, mats), a) =>
        val (ww, mat) = go(rest, a)
        ww -> NonEmptyList(mat, mats.tail)
    }
    data._2.reverse
  }

  lazy val counts =
    config.inLayers.reverse.reduceLeftTo(a => Nel((a, 1))) {
      case (z, a) =>
        NonEmptyList((a, z.head._1), z.tail)
    }
}

// TODO parallel computing
case class BatchStep[S: Sample](data: Nel[S], config: MLPLearnConf)
(implicit mc: MC[S])
extends MLPStep
{
  def apply(weights: Weights): I =
    gradientDescent(reshapeWeights(weights)) map reshapeToWeights

  lazy val gradientDescent =
    config.gradientMode match {
      case MLPLearnConf.TrivialGradient =>
        trivialGradient
      case MLPLearnConf.ConjugateGradient =>
        conjugateGradient
    }

  def trivialGradient =
    TrivialGradientDescent(config.eta, colGradient)

  def conjugateGradient =
    ConjugateGradientDescent(error, colGradient)
}

case class OnlineStep[S: Sample](data: Nel[S], config: MLPLearnConf)
(implicit mc: MC[S])
extends MLPStep
{
  def apply(weights: Weights): I = {
    data
      .foldLeft(weights) { (z, sample) =>
        val o = optimize(predict(sample, z))
        z.fzipWith(o) { (a, b) => a :- (b * eta) }
      }
      .validNel[String]
  }
}

case class MLPEstimator[S: Sample]
(data: Nel[S], config: MLPLearnConf, stop: StopCriterion[Weights])
(implicit mc: MC[S])
extends UniformIterativeEstimator[Weights]
{
  lazy val initializer: WeightInitializer =
    config.initMode match {
      case Weights.Random => RandomWeights
      case Weights.Manual(w) => ManualWeights(w)
      case Weights.Annealing => AnnealedWeights(this)
    }

  lazy val initialParams = initializer(config.layers)

  lazy val step: MLPStep[S] =
    config.learnMode match {
      case LearnConf.Batch => BatchStep(data, config)
      case LearnConf.Online => OnlineStep(data, config)
    }
}
