package tryp
package mi

import fs2._
import fs2.util._
import Step._

import annotation.tailrec

case class Estimation[M](iterations: Long, params: M)

trait EstimationStep[M]
{
  def apply(params: M): M
}

abstract class Estimator[S: Sample, M]
{
  def data: Nel[S]

  def initialParams: M

  val step: EstimationStep[M]


  def result(iteration: Long, par: M) = Estimation(iteration, par)

  def run(stop: StopCriterion[M]): Estimation[M] = {
    @tailrec
    def go(iteration: Long, par: M, prev: Option[M]): Estimation[M] = {
      if (stop(iteration, par, prev)) result(iteration, par)
      else go(iteration + 1, step(par), Some(par))
    }
    go(0, initialParams, None)
  }

  type Ret = Stream[Task, Estimation[M]]

  def stream(stop: StopCriterion[M]): Ret = {
    def go(iteration: Long, par: M, prev: Option[M]): Ret = {
      Stream.emit(result(iteration, par)) ++ {
        if (stop(iteration, par, prev)) Stream.empty
        else Stream.suspend(go(iteration + 1, step(par), Some(par)))
      }
    }
    go(0, initialParams, None)
  }

  def runSteps(count: Long) = {
    run(StepCountStopCriterion[M](count))
  }
}
