package tryp
package mi
package viz

import fs2.util.Task
import fs2.Strategy

import org.jfree.data.xy.{DefaultXYZDataset, XYDataset}
import org.jfree.chart.renderer.xy._
import org.jfree.chart.axis._

import java.awt.Color
import java.awt.geom.Ellipse2D

import scalax.chart._, api._

import ParamVizData.ops._

case class JFreeData(chart: XYChart, samples: DefaultXYZDataset,
  estimation: DefaultXYZDataset)

case class JFree(figure: Figure, data: List[JFreeData])

object JFree
extends JFreeInstances
{
  def colors = List(Color.yellow, Color.green, Color.red)

  def color(i: Int) =
    colors.lift(i % colors.length).getOrElse(Color.orange)

  def outlineColor = Color.black

  val testSampleColor = Color.gray
}

@tc abstract class JSample[A: SampleVizData: Sample]
{
  import JFree._

  val sampleShape = new Ellipse2D.Double(-4.0, -4.0, 8.0, 8.0)

  lazy val sampleRenderer = {
    val r = new XYLineAndShapeRenderer(false, true)
    r.setBaseOutlinePaint(outlineColor)
    r.setUseOutlinePaint(true)
    r.setUseFillPaint(true)
    val classes = Sample[A].classes.toList
    classes.zipWithIndex foreach {
      case (c, i) =>
        r.setSeriesShape(i, sampleShape)
        r.setSeriesFillPaint(i, color(i))
    }
    val testIndex = classes.length
    r.setSeriesShape(testIndex, sampleShape)
    r.setSeriesFillPaint(testIndex, testSampleColor)
    r
  }
}

object JSample
{
  implicit def any[A: SampleVizData: Sample] =
    new JSample {}
}

@tc abstract class JParam[P]
{
  def renderer: AbstractXYItemRenderer

  lazy val estimationRenderer = {
    val r = renderer
    r.setSeriesOutlinePaint(0, Color.blue)
    r.setSeriesFillPaint(0, Color.blue)
    r
  }
}

final class JFreeViz
[S: SampleVizData: Sample, P: ParamVizData: JParam]
(implicit fconf: FigureConf)
extends Viz[JFree, S, P]
with Logging
{
  type Coords = List[(Double, Double)]

  implicit def strat = Strategy.sequential

  def sampleViz = SampleVizData[S]

  def jsam = JSample[S]

  def jparam = JParam[P]

  def data(xrange: (Double, Double), yrange: (Double, Double)) = {
    val samples = new DefaultXYZDataset
    val estimation = new DefaultXYZDataset
    val ax = new NumberAxis
    val ay = new NumberAxis
    val plot = new XYPlot
    plot.setDomainAxis(ax)
    plot.setRangeAxis(ay)
    val chart = XYChart(plot, "mi", false, ChartTheme.Default)
    ax.setAutoRange(false)
    ax.setRange(xrange._1, xrange._2)
    ay.setAutoRange(false)
    ay.setRange(yrange._1, yrange._2)
    plot.setDataset(0, samples)
    plot.setDataset(1, estimation)
    plot.setRenderer(0, jsam.sampleRenderer)
    plot.setRenderer(1, jparam.estimationRenderer)
    plot.setBackgroundPaint(Color.white)
    JFreeData(chart, samples, estimation)
  }

  def init = {
    val d = sampleViz.projectionRanges
      .map { case (x, y) => data(x, y) }
    val rows = Math.ceil(Math.sqrt(sampleViz.plotCount)).toInt
    val figure = new Figure(fconf.copy(rows = rows), d.map(_.chart))
    JFree(figure, d)
  }

  def setup(a: JFree): Task[Unit] = {
    Task(a.figure.show())
  }

  def fold(a: JFree)(train: List[S], test: List[S]): Task[Unit] = {
    log.debug(s"Viz fold: $train")
    Sample[S].classes.unwrap.zipWithIndex
      .map { case (c, i) =>
        val data = train.filter(_.cls == c).map(_.feature)
        update(a)(_.samples, c.name, data, data.length.gen(0d).toArray)
      }
      .sequence_
      .flatMap { _ =>
        update(a)(_.samples, "test", test.map(_.feature),
          test.length.gen(0d).toArray)
      }
  }

  def step(a: JFree)(params: P): Task[Unit] = {
    log.debug(s"Viz step: $params")
    val plots = params.estimationPlot
    plots
      .zipWithIndex
      .map { case (plot, index) =>
        val z = (0 until plot.points.length).map(plot.size).toArray
        update(a)(_.estimation, s"estimation part $index", plot.points, z)
      }
      .sequence_
  }

  private[this] def update(a: JFree)
  (dataset: JFreeData => DefaultXYZDataset, name: String, data: List[Col],
    size: Array[Double]) = {
      val plots = sampleViz.plots(data, size)
      a.data
        .zip(plots)
        .map {
          case (jdata, plot) =>
            Task(dataset(jdata).addSeries(name, plot))
        }
        .sequence_
    }

  def dataSize = 0.05d
}

trait JFreeInstances
{
  implicit def instance_Viz_JFree
  [S: SampleVizData: Sample, P: ParamVizData: JParam]
  (implicit fconf: FigureConf) =
    new JFreeViz[S, P]
}
