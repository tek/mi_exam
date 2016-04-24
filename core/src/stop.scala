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
