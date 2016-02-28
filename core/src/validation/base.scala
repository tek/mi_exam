package tryp
package mi

trait SampleValidation[A, O]
{
  val data: A
  val pred: O
  def success: Boolean
  def info: String
  def error(cost: Func2): Double
}

case class EstimationStats(errors: Nel[Double])
{
  def total = errors.unwrap.sum
}

case class Validation[A, O](data: Nel[SampleValidation[A, O]])
{
  def stats(cost: Func2) = {
    val errors = data.map(_.error(cost))
    EstimationStats(errors)
  }
}

abstract class Validator[A: Sample, P, O]
{
  val data: Nel[A]

  def run(weights: P): Validation[A, O]
}
