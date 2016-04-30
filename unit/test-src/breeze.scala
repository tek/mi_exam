package tryp
package mi

import fs2._
import fs2.util._
import Step._
import Stream.Handle

import spire.math._
import spire.algebra._
import spire.implicits._
import spire.random._

import breeze.linalg._
import breeze.numerics._
import breeze.linalg.functions.euclideanDistance
import breeze.plot._

class BreezeSpec
extends Spec
{
  def is = s2"""
  test $test
  """

  def test = {
    val fig = Figure()
    val plt = fig.subplot(0)
    val x = linspace(0.0, 5.0, 50)
    val y = linspace(8.0, 3.0, 50)
    plt += plot(x, y)
    fig.refresh()
    Thread.sleep(2000)
    fig.clearPlot(0)
    val plt2 = fig.subplot(0)
    plt2 += scatter(linspace(0.0, 5.0, 5), linspace(0.0, 5.0, 5), { _ => 0.01 })
    fig.refresh()
    Thread.sleep(2000)
    1 === 1
  }
}
