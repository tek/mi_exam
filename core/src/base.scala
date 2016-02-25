package tryp
package mi

import annotation.tailrec

import simulacrum._

import breeze.generic.{UFunc, MappingUFunc}
import breeze.linalg.sum
import breeze.numerics.abs

trait ModelClass

case class LabeledClass(name: String)
extends ModelClass
{
  override def toString = name
}

@typeclass trait Sample[A]
{
  def classes: Map[Double, ModelClass]
  def cls(a: A): ModelClass
  def feature(a: A): Col
  def value(a: A): Double

  def predictedClass(pred: Double): ModelClass = {
    classes
      .minBy { case (v, cls) => (v - pred).abs }
      ._2
  }
}

trait StopCriterion[P]
{
  def apply(iteration: Long, params: P, prev: Option[P]): Boolean
}

case class StepCountStopCriterion[P](count: Long)
extends StopCriterion[P]
{
  def apply(iteration: Long, params: P, prev: Option[P]) = iteration >= count
}

case class ConvergenceStopCriterion(count: Long, epsilon: Double)
extends StopCriterion[Nel[Mat]]
{
  val steps = StepCountStopCriterion[Nel[Mat]](count)

  def diff(params: Nel[Mat], prev: Nel[Mat]) = {
      params.unwrap.zip(prev.unwrap)
        .map { case (a, b) => sum(abs(a :- b)) / a.size }
        .sum
  }

  def apply(iteration: Long, params: Nel[Mat], prev: Option[Nel[Mat]]) = {
    steps(iteration, params, prev) ||
      prev.exists(a => diff(params, a) < epsilon)
  }
}

case class TrainResult[P](iterations: Long, params: P)

trait Trainer[P]
{
  def initialParams: P

  def step(par: P): P

  def result(iteration: Long, par: P) = {
    TrainResult(iteration, par)
  }

  def run(stop: StopCriterion[P]): TrainResult[P] = {
    @tailrec
    def go(iteration: Long, par: P, prev: Option[P]): TrainResult[P] = {
      if (stop(iteration, par, prev)) result(iteration, par)
      else go(iteration + 1, step(par), Some(par))
    }
    go(0, initialParams, None)
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
