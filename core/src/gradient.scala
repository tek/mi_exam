package tryp
package mi

import annotation.tailrec

import breeze._
import linalg._
import functions._

trait GradientDescent
{
  def gradient: Col => Col

  def apply(weights: Col): Vali[Col]
}

case class TrivialGradientDescent(eta: Double, gradient: Col => Col)
extends GradientDescent
{
  def apply(w: Col) = {
    (w + eta * gradient(w))
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

case class CGPass(k: Int, w: Col, error: Col => Double, gradient: Col => Col)
extends Logging
{
  def n = w.size

  def secondOrder(params: CGParams) = {
    if (params.success) {
      val σk = params.σ / norm(params.p)
      val s = gradient(w + (params.σ * params.p)) - gradient(w)
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
    val r = -gradient(next)
    val done = (norm(r) < 1e-6) || (euclideanDistance(w, next) < 1e-6)
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

case class ConjugateGradientDescent(error: Col => Double, gradient: Col => Col)
extends GradientDescent
{
  def apply(w: Col) = {
    val init = CGParams.init(-gradient(w))
    @tailrec
    def go(n: Int, k: Int, w: Col, par: CGParams): Col = {
      if (k > 1000 || n > 10000 || par.done) w
      else {
        val pass = CGPass(k, w, error, gradient)
        val (wn, parn) = pass.run(par)
        val k1 = if (parn.success) k + 1 else k
        go(n + 1, k1, wn, parn)
      }
    }
    go(0, 0, w, init).validNel[String]
  }
}
