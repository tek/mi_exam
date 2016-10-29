package tryp
package mi

import fs2._
import fs2.util._

import annotation.tailrec

trait Estimation
{
  def iterations: Long
}

case class Est[P](iterations: Long, params: P)
extends Estimation

trait EstimationStep[P]
{
  protected type I = Vali[P]

  def apply(params: P): I
}

trait Estimator[P]
{
  protected type I = Vali[P]
  protected type R = Vali[Est[P]]

  def stream: Stream[Task, R]
}

trait SimpleEstimator[P]
extends Estimator[P]
{
  def go: I

  def stream: Stream[Task, R] =
    Stream.suspend(Stream.emit(go map (Est(1, _))))
}

trait IterativeEstimator[P]
extends Estimator[P]
{
  def initialParams: P

  def stop: StopCriterion[P]

  def steps: Nel[EstimationStep[P]]

  def result(iteration: Long, par: P) = Est(iteration, par)

  implicit def strat = Strategy.sequential

  // def run: Task[R] = {
  //   @tailrec
  //   def go(iteration: Long, par: P, prev: Option[P]): Est[P] = {
  //     if (stop(iteration, par, prev)) result(iteration, par)
  //     else go(iteration + 1, step(par), Some(par))
  //   }
  //   Task(go(0, initialParams, None).valid)
  // }

  type Ret = Stream[Task, R]

  private[this] lazy val empty = Stream.empty[Task, R]

  private[this] def streamRecurse(iteration: Long, param: P, prev: Option[P]) =
  {
    if (stop(iteration, param, prev)) empty
    else Stream.suspend(streamImpl(
      iteration + 1, computeOne(param.validNel[String]), Some(param))
    )
  }

  private[this] def streamImpl(iteration: Long, param: I, prev: Option[P])
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

trait UniformIterativeEstimator[P]
extends IterativeEstimator[P]
{
  def step: EstimationStep[P]

  def steps = Nel(step)
}
