package tryp
package mi

trait SampleValidation[A, O]
{
  val data: A
  val pred: O
  def success: Boolean
  def info: String
}

case class Validation[A, O](data: Nel[SampleValidation[A, O]])

abstract class Validator[A: Sample, P, O]
{
  val data: Nel[A]

  def run(weights: P): Validation[A, O]
}
