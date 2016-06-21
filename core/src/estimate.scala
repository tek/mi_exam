package tryp
package mi

import fs2._
import fs2.util._
import Step._

import annotation.tailrec

case class Est[M](iterations: Long, params: M)

trait Estimator[M]
{
  def apply(params: M): M
}

trait SimpleEstimator[M]
{
  def initialParams: M

  val step: Estimator[M]

  def stream: Stream[Task, Est[M]] = 
    Stream.suspend(Stream.emit(Est(1, step(initialParams))))
}

trait IterativeEstimator[M]
extends SimpleEstimator[M]
{
  def stop: StopCriterion[M]

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
