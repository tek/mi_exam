package tryp
package mi
package mlp

import annotation.tailrec

import cats.data.OneAnd

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

abstract class GradientDescent[S: Sample]
(implicit mc: MC[S])
{
  def data: Nel[S]

  def optimize: BackProp

  def predict: MLPPredictor

  def config: MLPLearnConf

  def gradient(weights: Weights): Weights = {
    val r: Nel[Weights] = data map (a => optimize(predict(a, weights)))
    r.reduceLeft { (a, b) => a fzip b map { case (a, b) => a + b } }
  }

  def apply(weights: Weights): Vali[Weights]
}

case class TrivialGradientDescent[S: Sample](data: Nel[S], optimize: BackProp,
  predict: MLPPredictor, config: MLPLearnConf)
(implicit mc: MC[S])
extends GradientDescent[S]
{
  def apply(weights: Weights) = {
    weights.apzip(gradient)
      .map { case (a, b) => a :- (b * config.eta) }
      .validNel[String]
  }
}

case class CGParams(
  λ: Double, λcon: Double, σ: Double, r: Col, p: Col, δ: Double, µ: Double,
  α: Double, success: Boolean, done: Boolean
)
{
  lazy val pSquared = p dot p
}

object CGParams
{
  def init(grad0: Col) =
    CGParams(1e-4d, 0d, 1e-2d, grad0, grad0, 0d, 0d, 0d, true, false)
}

case class CGPass(k: Int, w: Col, error: Col => Double, grad: Col => Col)
extends Logging
{
  def n = w.size

  def secondOrder(params: CGParams) = {
    if (params.success) {
      val σk = params.σ / norm(params.p)
      val s = grad(w + (params.σ * params.p)) - grad(w)
      val δ = params.p dot s
      params.copy(δ = δ)
    }
    else params
  }

  def updateDelta(params: CGParams) = {
    val δ = params.δ + ((params.λ - params.λcon) * params.pSquared)
    if (δ <= 0d) {
      val λcon = 2 * (params.λ - (δ / params.pSquared))
      val δ1 = -δ + (params.λ * params.pSquared)
      val λ = params.λcon
      params.copy(δ = δ1, λ = λ, λcon = λcon)
    }
    else params.copy(δ = δ)
  }

  def stepSize(params: CGParams) = {
    val µ = params.p dot params.r
    val α = µ / params.δ
    params.copy(µ = µ, α = α)
  }

  def errorDiff(params: CGParams, next: Col) = {
    2 * params.δ * (error(w) - error(next)) / params.µ
  }

  def descend(params: CGParams, next: Col, d: Double) = {
    val r = -grad(next)
    val done = (norm(r) < 1e-6) || (norm(w - next) < 1e-6)
    val λcon = 0d
    val beta =
      if (k % n == 0) ((r dot r) - (params.r dot r)) / params.µ
      else 1
    val p = r + beta * params.p
    val λ = if(d >= .75d) params.λ / 4 else params.λ
    next -> params.copy(r = r, p = p, λ = λ, done = done, λcon = λcon,
      success = true)
  }

  def reduce(params: CGParams) = {
    val next = w + (params.α * params.p)
    val d = errorDiff(params, next)
    val res: (Col, CGParams) =
      if (d >= 0) descend(params, next, d)
      else w -> params.copy(λcon = params.λ, success = false)
    val (wNext, parNext) = res
    val λ =
      if (d < .25d) parNext.λ + (parNext.δ * (1d - d)) / params.pSquared
      else parNext.λ
    wNext -> parNext.copy(λ = λ)
  }

  val run2 =
    secondOrder _ andThen updateDelta _ andThen stepSize _ andThen reduce _

  def run(params: CGParams): (Col, CGParams) = {
    val step1 = secondOrder(params)
    val step2 = updateDelta(step1)
    val step3 = stepSize(step2)
    reduce(step3)
  }
}

case class ConjugateGradientDescent[S: Sample](data: Nel[S],
  optimize: BackProp, predict: MLPPredictor, config: MLPLearnConf)
(implicit mc: MC[S])
extends GradientDescent[S]
{
  lazy val validator = MLPValidator(data, config)

  def apply(weights: Weights) = {
    val w0 = reshapeWeights(weights)
    val init = CGParams.init(-colGradient(w0))
    @tailrec
    def go(n: Int, k: Int, w: Col, par: CGParams): Col = {
      if (k > 1000 || n > 10000 || par.done) {
        p(k)
        p(n)
        w
      }
      else {
        val pass = CGPass(k, w, error, colGradient)
        val (wn, parn) = pass.run(par)
        val k1 = if (parn.success) k + 1 else k
        go(n + 1, k1, wn, parn)
      }
    }
    reshapeToWeights(go(0, 0, w0, init)).validNel[String]
  }

  def error(weights: Col): Double =
    validator.error(reshapeToWeights(weights)) getOrElse Double.NaN

  def colGradient(weights: Col) =
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
        ww -> OneAnd(mat, mats.unwrap)
    }
    data._2.reverse
  }

  def rank = data.head.rank

  lazy val counts =
    config.inLayers(rank).reverse.reduceLeftTo(a => Nel((a, 1))) {
      case (z, a) =>
        OneAnd((a, z.head._1), z.unwrap)
    }
}

abstract class MLPStep[S: Sample]
extends EstimationStep[Weights]
{
  val config: MLPLearnConf

  val data: Nel[S]

  val eta = config.eta / data.length

  lazy val predict = MLPPredictor(config)

  lazy val optimize = BackProp(config.transfer, config.cost)
}

// TODO parallel computing
case class BatchStep[S: Sample](data: Nel[S], config: MLPLearnConf)
(implicit mc: MC[S])
extends MLPStep
{
  def apply(weights: Weights): I =
    gradientDescent(weights)

  lazy val gradientDescent =
    config.gradientMode match {
      case MLPLearnConf.TrivialGradient =>
        trivialGradient
      case MLPLearnConf.ConjugateGradient =>
        conjugateGradient
    }

  def trivialGradient =
    TrivialGradientDescent(data, optimize, predict, config)

  def conjugateGradient =
    ConjugateGradientDescent(data, optimize, predict, config)
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
  lazy val initialParams = config.initialParams(Sample[S].featureCount)

  lazy val step: MLPStep[S] = {
    config.learnMode match {
      case LearnConf.Batch => BatchStep(data, config)
      case LearnConf.Online => OnlineStep(data, config)
    }
  }
}
