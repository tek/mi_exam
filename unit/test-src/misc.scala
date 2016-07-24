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

class MiscSpec
extends Spec
{
  def is = s2"""
  test $test
  """

  def test = {
    val q = QuadraticError
    val a = Col(1d, 2d)
    val b = Col(1d, 3d)
    1 === 1
  }
}
