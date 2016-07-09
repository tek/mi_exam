package tryp
package mi

import fs2._
import fs2.util._

import breeze.math._
import breeze.linalg._
import breeze.numerics._
import breeze.linalg.functions.euclideanDistance
import breeze.plot._
import breeze.optimize.proximal.QuadraticMinimizer
import breeze.optimize.proximal.Constraint
import breeze.optimize.proximal.ProjectPos
import breeze.optimize.proximal.ProjectBox
import breeze.generic.{UFunc, MappingUFunc}

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
    val k = Kern(3d)
    k(Col(1d, 1d), Col(0d, 2d))
    1 === 1
  }
}
