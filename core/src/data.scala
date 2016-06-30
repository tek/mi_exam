package tryp
package mi

trait ModelClass[S]

trait NamedClass[S]
extends ModelClass[S]
{
  def name: String

  override def toString = name
}

trait AutoClass[S]
extends NamedClass[S]
{
  def name = this.className.toLowerCase
}

case class LabeledClass[S](name: String)
extends NamedClass[S]

@tc trait ModelClasses[S]
{
  def value(a: ModelClass[S]): Validated[String, Double]
  def valueOrNaN(a: ModelClass[S]): Double = value(a).getOrElse(Double.NaN)
}

@tc abstract class Sample[S: ModelClasses]
{
  def classes: Nel[ModelClass[S]]
  def cls(a: S): ModelClass[S]
  def feature(a: S): Col
  def range: Double = 1d
  def featureCount: Int

  def value(a: S): ValiDouble = ModelClasses[S].value(cls(a))

  def valueOrNaN(a: S): Double = ModelClasses[S].valueOrNaN(cls(a))

  def predictedClass(pred: Double): ModelClass[S] =
    classes minBy (cls => (ModelClasses[S].valueOrNaN(cls) - pred).abs)
}

@tc trait ParamDiff[M]
{
  def diff(a: M, b: M): Double
}

@tc trait ModelState[S]
{
  def output(a: S): Double
}
