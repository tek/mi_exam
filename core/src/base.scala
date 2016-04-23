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

@typeclass trait Sample[S]
{
  def classes: Map[Double, ModelClass]
  def cls(a: S): ModelClass
  def feature(a: S): Col
  def value(a: S): Double
  def range: Double = 1d

  def predictedClass(pred: Double): ModelClass = {
    classes
      .minBy { case (v, cls) => (v - pred).abs }
      ._2
  }
}

trait StopCriterion[M]
{
  def apply(iteration: Long, params: M, prev: Option[M]): Boolean
}

case class StepCountStopCriterion[M](count: Long)
extends StopCriterion[M]
{
  def apply(iteration: Long, params: M, prev: Option[M]) = iteration >= count
}

@typeclass trait ParamDiff[M]
{
  def diff(a: M, b: M): Double
}

case class ConvergenceStopCriterion[M: ParamDiff](count: Long, epsilon: Double)
extends StopCriterion[M]
{
  val steps = StepCountStopCriterion[M](count)

  lazy val diff = ParamDiff[M].diff _

  def apply(iteration: Long, params: M, prev: Option[M]) = {
    steps(iteration, params, prev) ||
      prev.exists(a => diff(params, a) < epsilon)
  }
}

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

trait ModelCreator[P, M]
{
  def run(est: Estimation[P]): M
}

case class IdModelCreator[P]()
extends ModelCreator[P, P]
{
  def run(est: Estimation[P]): P = est.params
}

case class Prediction[S, M, V](sample: S, model: M, value: V)

trait Predictor[M, V]
{
  def apply[S: Sample](sample: S, model: M): Prediction[S, M, V]
}

trait Optimizer[M, V]
{
  def apply[S: Sample](a: Prediction[S, M, V]): M
}

object LearnConf
{
  sealed trait LearnMode

  case object Batch
  extends LearnMode

  case object Online
  extends LearnMode
}

@typeclass trait ModelState[S]
{
  def output(a: S): Double
}
