package tryp
package mi

import spire.math._
import spire.algebra._
import spire.implicits._
import spire.random._

import breeze.linalg._
import breeze.numerics._
import breeze.linalg.functions.euclideanDistance

class BreezeSpec
extends Spec
{
  def is = s2"""
  test $test
  """

  val A = new Mat(2, 2, Array(1d, 2d, 3d, 4d))

  val B = Col(6d, 7d)

  def X = pinv(A) * B

  def test = {
    norm(A * X - B) must beCloseTo(0d, 0.1)
  }
}
