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

class JFreeSpec
extends Spec
{
  def is = s2"""
  main $main
  """

  import scala.concurrent.ExecutionContext.Implicits.global

  import org.jfree.data.xy.{DefaultXYZDataset, XYDataset}
  import org.jfree.chart.renderer.xy._

  import scalax.chart._, api._

  import scala.swing.Swing._

  def main = {
    val r = new XYLineAndShapeRenderer
    val dat = new DefaultXYZDataset()
    val dat2 = new DefaultXYZDataset()
    // val plt = new org.jfree.chart.plot.XYPlot()
    // plt.setDataset(0, dat)
    // plt.setRenderer(0, r)
    // val coll = new XYSeriesCollection()
    val chart = XYLineChart.shapes(dat)
    chart.plot.setDataset(1, dat2)
    // val series1 = List((1d, 1d), (2d, 2d), (2d, 0.5d), (4d, 1d))
    //   .toXYSeries("series 1")
    // val series2 = List((2d, 1d), (2d, 3d), (1d, 0.5d), (4d, 2d))
    //   .toXYSeries("series 2")
    // coll.addSeries(series1)
    // coll.addSeries(series2)
    // p(chart.plot.getDataset)
    hl
    chart.show()
    hl
    Thread.sleep(5000)
    dat.addSeries("seri", Array(Array(1, 2, 3), Array(3, 4, 5), Array(1, 1, 0)))
    dat.addSeries("seri2", Array(Array(3, 4, 5), Array(1, 2, 3), Array(1, 1, 0)))
    Thread.sleep(5000)
    // val series3 = List((1d, 0d), (3d, 0d), (3d, 1d), (2d, 4d))
    //   .toXYSeries("series 3")
    // coll.removeSeries(series1)
    // coll.addSeries(series3)
    // Thread.sleep(5000)
    // p(chart.plot)
    1 === 1
  }
}
