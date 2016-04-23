package tryp
package mi

abstract class SampleValidation[S: Sample, O]
{
  val data: S

  def state: O

  def output: Double

  def predictedClass: ModelClass

  def error(cost: Func2): Double

  def actualClass = data.cls

  def success = actualClass == predictedClass

  def successInfo = {
    if (success) s"correct class"
    else f"wrong class: $predictedClass ($output%g)"
  }

  def info =
    s"${actualClass} (${data.feature.data.mkString(", ")}): $successInfo"
}

case class SV[S: Sample](data: S, state: Double)
extends SampleValidation[S, Double]
{
  def output = state

  lazy val predictedClass = Sample[S].predictedClass(output)

  def error(cost: Func2) = cost.f(data.value, output)
}

case class EstimationStats(successes: Int, errors: Nel[Double])
{
  def totalError = errors.unwrap.sum

  def success = (successes, count)

  def count = errors.length
}

case class Validation[S, O](data: Nel[SampleValidation[S, O]])
{
  def stats(cost: Func2) = {
    val errors = data.map(_.error(cost))
    EstimationStats(data.filter(_.success).length, errors)
  }
}

abstract class Validator[S: Sample, P, O]
{
  val data: Nel[S]

  def run(weights: P): Validation[S, O]
}
