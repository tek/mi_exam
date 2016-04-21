package tryp
package mi

import annotation.tailrec

import simulacrum._

import breeze.generic.{UFunc, MappingUFunc}

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
  def range: Double = 1d

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

@typeclass trait ParamDiff[P]
{
  def diff(a: P, b: P): Double
}

case class ConvergenceStopCriterion[P: ParamDiff](count: Long, epsilon: Double)
extends StopCriterion[P]
{
  val steps = StepCountStopCriterion[P](count)

  lazy val diff = ParamDiff[P].diff _

  def apply(iteration: Long, params: P, prev: Option[P]) = {
    steps(iteration, params, prev) ||
      prev.exists(a => diff(params, a) < epsilon)
  }
}

case class Estimation[P](iterations: Long, params: P)

trait EstimationStep[P]
{
  def apply(weights: P): P
}

trait Estimator[P]
{
  def initialParams: P

  val step: EstimationStep[P]

  def result(iteration: Long, par: P) = {
    Estimation(iteration, par)
  }

  def run(stop: StopCriterion[P]): Estimation[P] = {
    @tailrec
    def go(iteration: Long, par: P, prev: Option[P]): Estimation[P] = {
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

object LearnConf
{
  sealed trait LearnMode

  case object Batch
  extends LearnMode

  case object Online
  extends LearnMode
}
