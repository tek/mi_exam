package tryp
package mi

import fs2._
import fs2.util._
import Step._

import annotation.tailrec

case class Est[M](iterations: Long, params: M)

trait EstimationStep[M]
{
  protected type I = String ValidatedNel M

  def apply(params: M): I
}

trait Estimator[M]
{
  protected type I = Vali[M]
  protected type R = Vali[Est[M]]

  def stream: Stream[Task, R]
}

trait SimpleEstimator[M]
extends Estimator[M]
{
  def go: I

  def stream: Stream[Task, R] =
    Stream.suspend(Stream.emit(go map (Est(1, _))))
}

trait IterativeEstimator[M]
extends Estimator[M]
{
  def initialParams: M

  def stop: StopCriterion[M]

  def steps: Nel[EstimationStep[M]]

  def result(iteration: Long, par: M) = Est(iteration, par)

  implicit def strat = Strategy.sequential

  // def run: Task[R] = {
  //   @tailrec
  //   def go(iteration: Long, par: M, prev: Option[M]): Est[M] = {
  //     if (stop(iteration, par, prev)) result(iteration, par)
  //     else go(iteration + 1, step(par), Some(par))
  //   }
  //   Task(go(0, initialParams, None).valid)
  // }

  type Ret = Stream[Task, R]

  private[this] lazy val empty = Stream.empty[Task, R]

  private[this] def streamRecurse(iteration: Long, param: M, prev: Option[M]) =
  {
    if (stop(iteration, param, prev)) empty
    else Stream.suspend(streamImpl(
      iteration + 1, computeOne(param.validNel[String]), Some(param))
    )
  }

  private[this] def streamImpl(iteration: Long, param: I, prev: Option[M])
  : Ret = {
    Stream.emit(param map (result(iteration, _))) ++
    param.fold(a => Stream.emit(a.invalid), streamRecurse(iteration, _, prev))
  }

  override def stream: Ret =
    streamImpl(0, initialParams.validNel[String], None)

  private[this] def computeOne(param: I) = {
    steps.foldLeft(param) { (z, s) =>
      z flatMap (s(_))
    }
  }
}

trait UniformIterativeEstimator[M]
extends IterativeEstimator[M]
{
  def step: EstimationStep[M]

  def steps = Nel(step)
}
