package tryp
package mi

abstract class SampleValidation[S: Sample, V]
(implicit val mc: ModelClasses[S, V])
{
  val data: S

  def output: V

  lazy val predictedClass: ModelClass[S] = mc.predictedClass(output)

  def error(cost: Func2): Vali[Double]

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
extends SampleValidation[S, Double]
{
  def error(cost: Func2) = data.value.map(cost.f(_, output)).toValidatedNel
}

case class ColSV[S: Sample](data: S, output: Col)
(implicit mc: ModelClasses[S, Col])
extends SampleValidation[S, Col]
{
  def error(cost: Func2) = {
    ???
    // data.value map (cost.f[Col](_, output)) map (_.sum)
  }
}

case class EstimationStats(successes: Int, errors: Nel[Double])
{
  def totalError = errors.unwrap.sum

  def success = (successes, count)

  def count = errors.length
}

case class Validation[S, V](data: Nel[SampleValidation[S, V]])
{
  def stats(cost: Func2): Vali[EstimationStats] = {
    data.traverseU(_.error(cost))
      .map(EstimationStats(data.filter(_.success).length, _))
  }
}

abstract class Validator[S: Sample, M, V]
{
  val data: Nel[S]

  def run(weights: M): Validation[S, V]
}
