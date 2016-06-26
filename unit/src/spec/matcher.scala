package tryp
package mi

import breeze.linalg.sum
import breeze.numerics.abs

import org.specs2.matcher.{Matcher, Expectable, NumericMatchers}

class BeCloseToCol(target: Col, delta: Double)
extends Matcher[Col]
{
  def apply[S <: Col](x: Expectable[S]) = {
    val diff = sum(abs(target - x.value))
    def msg(is: String) = s"${x.value} is$is close to $target ± $delta"
    result(diff < delta, msg(""), msg(" not"), x)
  }
}

trait Matchers
extends NumericMatchers
{
  def closeThreshold = 1e-5

  def beCloseToCol(target: Col, delta: Double): Matcher[Col] =
    new BeCloseToCol(target, delta)

  def beCloseCol(target: Col) = beCloseToCol(target, closeThreshold)

  def beClose(a: Double) = beCloseTo(a, closeThreshold)
}
