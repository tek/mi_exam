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
  def stream: Stream[Task, String ValidatedNel Est[M]]
}

trait SimpleEstimator[M]
extends Estimator[M]
{
  def go: String ValidatedNel M

  def stream: Stream[Task, String ValidatedNel Est[M]] =
    Stream.suspend(Stream.emit(go map (Est(1, _))))
}

trait IterativeEstimator[M]
extends Estimator[M]
{
  private[this] type R = String ValidatedNel Est[M]

  def initialParams: M

  def stop: StopCriterion[M]

  val step: EstimationStep[M]

  def result(iteration: Long, par: M) = Est(iteration, par)

  implicit def strat = Strategy.sequential

  def run: Task[R] = {
    @tailrec
    def go(iteration: Long, par: M, prev: Option[M]): Est[M] = {
      if (stop(iteration, par, prev)) result(iteration, par)
      else go(iteration + 1, step(par), Some(par))
    }
    Task(go(0, initialParams, None).valid)
  }

  type Ret = Stream[Task, R]

  override def stream: Ret = {
    def go(iteration: Long, par: M, prev: Option[M]): Ret = {
      Stream.emit(result(iteration, par).validNel[String]) ++ {
        if (stop(iteration, par, prev)) Stream.empty[Task, R]
        else Stream.suspend(go(iteration + 1, step(par), Some(par)))
      }
    }
    go(0, initialParams, None)
  }
}
