package tryp
package mi

trait StopCriterion[M]
{
  def apply(iteration: Long, params: M, prev: Option[M]): Boolean
}

case class StepCountStopCriterion[M](count: Long)
extends StopCriterion[M]
{
  def apply(iteration: Long, params: M, prev: Option[M]) = iteration >= count
}

trait ConvergenceStopCriterion[M]
extends StopCriterion[M]
{
  def count: Long

  val steps = StepCountStopCriterion[M](count)

  def apply(iteration: Long, params: M, prev: Option[M]) = {
    steps(iteration, params, prev) || converged(params, prev)
  }

  def converged(params: M, prev: Option[M]): Boolean
}

case class EmpiricalErrorStopCriterion[M: EmpiricalError]
(count: Long, epsilon: Double)
extends ConvergenceStopCriterion[M]
{
  lazy val value = EmpiricalError[M].value _

  def converged(params: M, prev: Option[M]) =
    value(params) < epsilon
}

case class ParamDiffStopCriterion[M: ParamDiff](count: Long, epsilon: Double)
extends ConvergenceStopCriterion[M]
{
  lazy val diff = ParamDiff[M].diff _

  def converged(params: M, prev: Option[M]) =
      prev.exists(a => diff(params, a) < epsilon)
}
