package tryp
package mi

import fs2._
import fs2.util._

import breeze._
import math._
import linalg._
import numerics._
import linalg.functions._
import optimize.proximal._
import generic._
import stats._

import org.scalacheck.Gen

case class Kern[A](a: A)(implicit sr: Semiring[A])
{
  def apply[V](v1: V, v2: V)(implicit isVec: V <:< Vector[A]) = {
    var acc = sr.zero
    for(i <- 0 until dim(v1)) {
      acc = sr.+(acc, sr.*(v1(i), v2(i)))
    }
    sr.*(acc, a)
  }
}

class BreezeSpec
extends Spec
{
  def is = s2"""
  test $test
  """

  def test = {
    val m = Mat((0.5d, 0.5d), (1d, 1d), (-1d, -1d), (2d, 2d), (5d, 3d))
    val c = mean(m)
    val cluster = m.rowCols.map(_ - c)
    p(m(*, ::) - Col(1d, 1d))
    p(cluster)
    val v1 = variance(m)
    val v2 = cluster.map(a => a dot a).sum
    p(v1)
    p(scala.math.sqrt(cluster.map(norm(_)).sum))
    p(v2 / (m.data.length - 1))
    1 === 1
  }
}
