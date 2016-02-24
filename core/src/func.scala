package tryp
package mi

import breeze.generic.{UFunc, MappingUFunc}
import breeze.numerics._
import breeze.linalg.support.CanMapValues

import simulacrum._

import scalaz.Liskov.<~<

trait FuncBase
extends UFunc
with MappingUFunc
{
  type DI = Impl[Double, Double]
  type DI2 = Impl2[Double, Double, Double]
}

trait Func
extends FuncBase
{
  implicit val doubleImpl: DI
}

trait Func2
extends FuncBase
{
  implicit val double2Impl: DI2
}

trait DFunc[D <: Func]
extends Func
{
  def deriv: D
}

trait DFunc2[D <: Func2]
extends Func2
{
  def deriv: D
}

trait NullFuncBase
extends DFunc[NullFuncBase]
with DFunc2[NullFuncBase]
{
  implicit val doubleImpl = new DI {
    def apply(a: Double) = 0d
  }

  implicit val double2Impl = new DI2 {
    def apply(a: Double, b: Double) = 0d
  }
}

object NullFunc
extends NullFuncBase
{
  def deriv = NullFunc
}

case class Const(c: Double)
extends DFunc[NullFuncBase]
{
  implicit val doubleImpl = new DI {
    def apply(a: Double) = c
  }

  def deriv = NullFunc
}

case class Linear(slope: Double, offset: Double)
extends DFunc[Const]
{
  implicit val doubleImpl: DI = new DI {
    def apply(a: Double) = slope * a + offset
  }

  lazy val deriv = Const(slope)
}

object Identity
extends Linear(0d, 1d)

object Diff
extends DFunc2[NullFuncBase]
{
  implicit val double2Impl: DI2 =
    new DI2 {
      def apply(a: Double, b: Double) = b - a
    }

  def deriv = NullFunc
}

object QuadraticError
extends DFunc2[Diff.type]
{
  implicit val double2Impl: DI2 =
    new DI2 {
      def apply(a: Double, b: Double) = pow(0.5 * (b - a), 2)
    }

  def deriv = Diff
}

@typeclass trait ImplImp[A]
{
  def impl[B <: Func](b: B): UFunc.UImpl[B, A, A]
  def impl2[B <: Func2](b: B): UFunc.UImpl2[B, A, A, A]
}

object ImplImp
{
  implicit def anyImplImp[A]
  (implicit
    cmv: CanMapValues[A, Double, Double, A],
    hh: CanMapValues.HandHold[A, Double]
    ): ImplImp[A] =
    new ImplImp[A] {
      def impl[B <: Func](b: B) = {
        import b._
        b.fromLowOrderCanMapValues[A, Double, Double, A]
          .asInstanceOf[UFunc.UImpl[B, A, A]]
      }

      def impl2[B <: Func2](b: B) = {
        import b._
        b.canMapV1DV[A, Double, Double, Double, A]
          .asInstanceOf[UFunc.UImpl2[B, A, A, A]]
      }
    }
}

@typeclass trait Fu[A]
{
  def f[B](a: A)(b: B)(implicit ev: A <~< Func, ii: ImplImp[B]) = {
    implicit val i = ii.impl(ev(a))
    i(b)
  }
}

object Fu
{
  implicit def funcFu[A <: Func]: Fu[A] = new Fu[A] {}
}

@typeclass trait Fu2[A]
{
  def f[B](a: A)(b: B, c: B)(implicit ev: A <~< Func2, ii: ImplImp[B]) = {
    implicit val i = ii.impl2(ev(a))
    i(b, c)
  }
}

object Fu2
{
  implicit def func2Fu[A <: Func2]: Fu2[A] = new Fu2[A] {}
}
