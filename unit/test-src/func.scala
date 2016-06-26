package tryp
package mi
package unit

import cats._, data.{Func => _, _}, syntax.all._, std.all._

import spire.math._
import spire.algebra._
import spire.implicits._
import spire.random._

import breeze.generic.{UFunc, MappingUFunc}

case class TestA[D <: Func, T <: DFunc[D]](transferU: T)
{
  def col(in: Col) = transferU.f(in)
  def mat(in: Mat) = transferU.f(in)
  def scalar(in: Double) = transferU.f(in)
}

object FunA
extends DFunc[NullFuncBase]
{
  implicit val doubleImpl: Impl[Double, Double] =
    new DI {
      def apply(a: Double) = 0d
    }

  def deriv = NullFunc
}

class FuncSpec
extends Spec
{
  def is = s2"""
  conjure implicit helpers for UFunc

  Impl[Col] $col
  Impl[Mat] $mat
  Impl[Double] $scalar
  """

  lazy val ta = TestA[NullFuncBase, FunA.type](FunA)

  def col = {
    ta.col(Col(5d, 3d)) === Col(0d, 0d)
  }

  def mat = {
    ta.mat(Mat(Array(5d, 3d, 9d))) === Mat(Array(0d, 0d, 0d))
  }

  def scalar = {
    ta.scalar(5d) === 0d
  }
}
