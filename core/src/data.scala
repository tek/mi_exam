package tryp
package mi

trait ModelClass[S]
{
  def name: String

  override def toString = name
}

trait AutoClass[S]
extends ModelClass[S]
{
  def name = this.className.toLowerCase
}

case class LabeledClass[S](name: String)
extends ModelClass[S]

@tc trait ModelClasses[S]
{
  def classes: Nel[ModelClass[S]]
  def value(a: ModelClass[S]): Validated[String, Double]
  def valueOrNaN(a: ModelClass[S]): Double = value(a).getOrElse(Double.NaN)
}

@tc abstract class Sample[S: ModelClasses]
{
  def cls(a: S): ModelClass[S]
  def feature(a: S): Col
  def range: Double = 1d
  def featureCount: Int

  def classes = ModelClasses[S].classes

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
