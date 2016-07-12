package tryp
package mi

import scalaz.Zip
import scalaz.syntax.zip._

import breeze.linalg.sum
import breeze.numerics.abs

import cats.Foldable

import org.specs2.matcher.{Matcher, Expectable, NumericMatchers}


@tc trait BeClose[A]
{
  def diff(a: A)(b: A): Double
}

trait BeCloseInstances
{
  implicit def instance_BeClose_A[A](implicit num: Numeric[A]): BeClose[A] =
    new BeClose[A] {
      def diff(a: A)(b: A) = abs(num.toDouble(num.minus(a, b)))
    }

  implicit lazy val instance_BeClose_Col: BeClose[Col] =
    new BeClose[Col] {
      def diff(a: Col)(b: Col) = sum(abs(a - b))
    }

  implicit def instance_BeClose_Traverse
  [F[_]: Foldable: Zip, A: BeClose]
  : BeClose[F[A]] =
    new BeClose[F[A]] {
      def diff(a: F[A])(b: F[A]) = (a fzip b).foldMap {
        case (x, y) => BeClose[A].diff(x)(y)
      }
    }
}

object BeClose
extends BeCloseInstances

class BeCloseMatcher[A: BeClose](target: A, epsilon: Double)
extends Matcher[A]
{
  import BeClose.ops._

  def apply[S <: A](x: Expectable[S]) = {
    val diff = target diff x.value
    def msg(is: String) = s"${x.value} is$is close to $target ± $epsilon"
    result(diff < epsilon, msg(""), msg(" not"), x)
  }
}

trait Matchers
extends NumericMatchers
{
  def closeThreshold = 1e-5

  def beCloseWithin[A: BeClose](target: A, epsilon: Double) =
    new BeCloseMatcher(target, epsilon)

  def beClose[A: BeClose](target: A) =
    beCloseWithin(target, closeThreshold)
}
