package tryp
package mi

import fs2._
import fs2.util._
import Step._

import annotation.tailrec

case class Est[M](iterations: Long, params: M)

trait EstimationStep[M]
{
  def apply(params: M): M
}

trait Estimator[M]
{
  def stream: Stream[Task, Est[M]]
}

trait SimpleEstimator[M]
extends Estimator[M]
{
  def go: String Xor M

  def stream: Stream[Task, Est[M]] =
    Stream.suspend(go map (e => Stream.emit(Est(1, e))) getOrElse Stream.empty)
}

trait IterativeEstimator[M]
extends Estimator[M]
{
  def initialParams: M

  def stop: StopCriterion[M]

  val step: EstimationStep[M]

  def result(iteration: Long, par: M) = Est(iteration, par)

  implicit def strat = Strategy.sequential

  def run: Task[Est[M]] = {
    @tailrec
    def go(iteration: Long, par: M, prev: Option[M]): Est[M] = {
      if (stop(iteration, par, prev)) result(iteration, par)
      else go(iteration + 1, step(par), Some(par))
    }
    Task(go(0, initialParams, None))
  }

  type Ret = Stream[Task, Est[M]]

  override def stream: Ret = {
    def go(iteration: Long, par: M, prev: Option[M]): Ret = {
      Stream.emit(result(iteration, par)) ++ {
        if (stop(iteration, par, prev)) Stream.empty
        else Stream.suspend(go(iteration + 1, step(par), Some(par)))
      }
    }
    go(0, initialParams, None)
  }
}
