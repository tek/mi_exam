package tryp
package mi

import fs2._
import fs2.util._

import spire.math._
import spire.algebra._
import spire.implicits._
import spire.random._

import breeze.linalg._
import breeze.numerics._
import breeze.linalg.functions.euclideanDistance
import breeze.plot._
import breeze.optimize.proximal.QuadraticMinimizer
import breeze.optimize.proximal.Constraint
import breeze.optimize.proximal.ProjectPos
import breeze.optimize.proximal.ProjectBox

import org.scalacheck.Gen

class BreezeSpec
extends Spec
{
  def is = s2"""
  test $test
  """

  def test = {
    val b = 5d
    val w = Col(1d, 1d, 2d)
    val nw = normalize(w)
    val wx = nw * b
    val planeGen: Gen[svm.Plane] = svm.SVMGen.genPlane(3)
    val plane = planeGen.sample.get
    1 === 1
  }
}
