package tryp
package mi

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

  val featureCount = data.head.feature.length

  def result(iteration: Long, par: M) = {
    Estimation(iteration, par)
  }

  def run(stop: StopCriterion[M]): Estimation[M] = {
    @tailrec
    def go(iteration: Long, par: M, prev: Option[M]): Estimation[M] = {
      if (stop(iteration, par, prev)) result(iteration, par)
      else go(iteration + 1, step(par), Some(par))
    }
    go(0, initialParams, None)
  }

  def runSteps(count: Long) = {
    run(StepCountStopCriterion[M](count))
  }
}
