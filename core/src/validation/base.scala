package tryp
package mi

import breeze._
import linalg._

trait SampleValidation
{
  def success: Boolean

  def error(cost: Func2): Vali[Double]

  def info: String
}

abstract class SV[S: Sample, V]
(implicit val mc: ModelClasses[S, V])
extends SampleValidation
{
  val data: S

  def output: V

  lazy val predictedClass: ModelClass[S] = mc.predictedClass(output)

  def actualClass = data.cls

  def success = actualClass == predictedClass

  def successInfo = {
    if (success) s"correct class"
    else f"wrong class: $predictedClass ($output)"
  }

  def info =
    s"${actualClass} (${data.feature.data.mkString(", ")}): $successInfo"
}

case class DSV[S: Sample](data: S, output: Double)
(implicit mc: ModelClasses[S, Double])
extends SV[S, Double]
{
  def error(cost: Func2) = data.value.map(cost.f(_, output)).toValidatedNel
}

case class ColSV[S: Sample](data: S, output: Col)
(implicit mc: ModelClasses[S, Col])
extends SV[S, Col]
{
  def error(cost: Func2) = {
    import cost._
    val v = data.value map (cost(_, output)) map (sum(_))
    v.toValidatedNel
  }
}

case class EstimationStats(successes: Int, errors: Nel[Double])
{
  def totalError = errors.tail.sum

  def success = (successes, count)

  def count = errors.length
}

trait Validation
{
  def data: Nel[SampleValidation]
  def stats(cost: Func2): Vali[EstimationStats]
}

case class Val(data: Nel[SampleValidation])
extends Validation
{
  def stats(cost: Func2): Vali[EstimationStats] = {
    data.traverseU(_.error(cost))
      .map(EstimationStats(data.filter(_.success).length, _))
  }
}

abstract class Validator[M]
{
  def run(model: M): Validation
}
