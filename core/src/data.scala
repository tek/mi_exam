package tryp
package mi

import simulacrum._

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

@typeclass trait ParamDiff[M]
{
  def diff(a: M, b: M): Double
}

@typeclass trait ModelState[S]
{
  def output(a: S): Double
}
