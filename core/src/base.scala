package tryp
package mi

import annotation.tailrec

import simulacrum._

import breeze.generic.{UFunc, MappingUFunc}

@typeclass trait Sample[A]
{
  def feature(a: A): Col
  def value(a: A): Double
}

trait StopCriterion[P]
{
  def apply(iteration: Long, params: P): Boolean
}

case class StepCountStopCriterion[P](count: Long)
extends StopCriterion[P]
{
  def apply(iteration: Long, params: P) = iteration >= count
}

case class TrainResult[P](iterations: Long, params: P)

trait Train[P]
{
  def initialParams: P

  def step(par: P): P

  def result(iteration: Long, par: P) = {
    TrainResult(iteration, par)
  }

  def run(stop: StopCriterion[P]): TrainResult[P] = {
    @tailrec
    def go(iteration: Long, par: P): TrainResult[P] = {
      if (stop(iteration, par)) result(iteration, par)
      else go(iteration + 1, step(par))
    }
    go(0, initialParams)
  }

  def runSteps(count: Long) = {
    run(StepCountStopCriterion[P](count))
  }
}

case class Prediction[A, P, O](sample: A, param: P, pred: O)

trait Predictor[P, O]
{
  def apply[A: Sample](sample: A, param: P): Prediction[A, P, O]
}

trait Optimizer[P, O]
{
  def apply[A: Sample](a: Prediction[A, P, O]): P
}
