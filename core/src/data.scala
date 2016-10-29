package tryp
package mi

import breeze._
import linalg._
import functions._

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

trait ModelTypes[M]
{
  type Value
  type V = Value
  def nan: Value

  type Param
  type State

  def scalarDistance(a: Value, b: Value): Double
}

object ModelTypes
{
  type ModelTypesEv[M, V0] = ModelTypes[M] { type Value = V0 }

  implicit def instance_ModelTypes_M[M]: ModelTypes[M] =
    new ModelTypes[M] {
      type Value = Double
      def nan = Double.NaN

      def scalarDistance(a: Value, b: Value) = (a - b).abs
    }

  def apply[M](implicit instance: ModelTypes[M]) = instance
}

@tc abstract class Sample[S]
{
  def cls(a: S): ModelClass[S]

  def feature(a: S): Col

  def range: Double = 1d

  def featureCount: Int

  def rank(a: S) = feature(a).size
}

@tc trait ModelValue[A]
{
  def nan: A
  def scalarDistance(a: A, b: A): Double
}

object ModelValue
{
  implicit def instance_ModelValue_Double: ModelValue[Double] =
    new ModelValue[Double] {
      def nan = Double.NaN

      def scalarDistance(a: Double, b: Double) = (a - b).abs
    }

  implicit def instance_ModelValue_Col: ModelValue[Col] =
    new ModelValue[Col] {
      def nan = Col(Double.NaN)
      def scalarDistance(a: Col, b: Col) = euclideanDistance(a, b)
    }
}

trait ModelClassesBase[S]
{
  def classes: Nel[ModelClass[S]]
}

abstract class ModelClasses[S, V: ModelValue]
extends ModelClassesBase[S]
{
  lazy val mv = ModelValue[V]

  type Value = V

  type Param
  type State

  def value(a: ModelClass[S]): Validated[String, V]

  def valueOrNaN(a: ModelClass[S]): V =
    value(a).getOrElse(mv.nan)

  def predictedClass(pred: Value): ModelClass[S] =
    classes minBy (cls => mv.scalarDistance(valueOrNaN(cls), pred))
}

object ModelClasses
{
  abstract class Ops[S, M]
  {
    def typeClassInstance: ModelClasses[S, M]
    def self: ModelClass[S]
    def value = typeClassInstance.value(self)
    def valueOrNaN = typeClassInstance.valueOrNaN(self)
  }

  abstract class SampleOps[S: Sample, M]
  {
    def typeClassInstance: ModelClasses[S, M]
    def self: S
    private[this] def cls = Sample[S].cls(self)
    def value = typeClassInstance.value(cls)
    def valueOrNaN = typeClassInstance.valueOrNaN(cls)
  }

  trait ToModelClassesOps
  {
    implicit def toModelClassesOps[S, M]
    (a: ModelClass[S])
    (implicit tc: ModelClasses[S, M]) =
      new Ops[S, M] {
        val self = a
        val typeClassInstance = tc
      }

    implicit def toModelClassesSampleOps[S: Sample, M]
    (a: S)
    (implicit tc: ModelClasses[S, M]) =
      new SampleOps[S, M] {
        val self = a
        val typeClassInstance = tc
      }
    }

  object ops
  extends ToModelClassesOps
}

@tc trait ParamDiff[M]
{
  def diff(a: M, b: M): Double
}

@tc trait ModelState[S]
{
  def output(a: S): Double
}

@tc trait EmpiricalError[M]
{
  def value(a: M): Double
}
