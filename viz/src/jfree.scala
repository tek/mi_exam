package tryp
package mi
package viz

import fs2.Task
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

@tc abstract class JSample[A: SampleVizData]
{
  import JFree._

  implicit def sampleData = SampleVizData[A]

  implicit def sample = sampleData.sample

  val sampleShape = new Ellipse2D.Double(-4.0, -4.0, 8.0, 8.0)

  lazy val classes = sampleData.mc.classes.toList

  lazy val sampleRenderer = {
    val r = new XYLineAndShapeRenderer(false, true)
    r.setBaseOutlinePaint(outlineColor)
    r.setUseOutlinePaint(true)
    r.setUseFillPaint(true)
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

  def train(data: List[A]) =
    classes.zipWithIndex
      .map { case (c, i) =>
        c.name -> data.filter(_.cls == c).map(_.feature)
      }
}

object JSample
{
  implicit def any[A: SampleVizData] =
    new JSample {}
}

@tc abstract class JParam[P: ParamVizData]
{
  def renderer: AbstractXYItemRenderer

  lazy val estimationRenderer = {
    val r = renderer
    r.setSeriesOutlinePaint(0, Color.blue)
    r.setSeriesFillPaint(0, Color.blue)
    r
  }

  def estimation(params: P) = ParamVizData[P].estimationPlot(params)
}

object JFreeViz
{
  /* NOTE setting the axis ranges will be undone by the XYChart constructor,
   * hence it has to happen afterwards.
   */
  def data(xrange: (Double, Double), yrange: (Double, Double),
    sampleRenderer: AbstractXYItemRenderer,
    estimationRenderer: AbstractXYItemRenderer) = {
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
    plot.setRenderer(0, sampleRenderer)
    plot.setRenderer(1, estimationRenderer)
    plot.setBackgroundPaint(Color.white)
    JFreeData(chart, samples, estimation)
  }
}

final class JFreeViz
[S: JSample, P: JParam]
(implicit fconf: FigureConf)
extends Viz[JFree, S, P]
with Logging
{
  import shapeless._
  import syntax.std.tuple._

  import JFreeViz._

  type Coords = List[(Double, Double)]

  implicit def strat = Strategy.sequential

  lazy val jsam = JSample[S]

  import jsam._

  def jparam = JParam[P]

  def init = {
    val d = sampleData.projectionRanges
      .map { case (x, y) =>
        data(x, y, sampleRenderer, jparam.estimationRenderer)
      }
    val rows = Math.ceil(Math.sqrt(sampleData.plotCount)).toInt
    val figure = new Figure(fconf.copy(rows = rows), d.map(_.chart))
    JFree(figure, d)
  }

  def setup(a: JFree): Task[Unit] = {
    Task(a.figure.show())
  }

  def fold(a: JFree)(train: List[S], test: List[S]): Task[Unit] = {
    log.debug(s"Viz fold: $train")
    jsam.train(train)
      .map { case (name, data) =>
        update(a)(_.samples, name, data, data.length.gen(0d).toArray)
      }
      .sequence_
      .flatMap { _ =>
        update(a)(_.samples, "test", test.map(_.feature),
          test.length.gen(0d).toArray)
      }
  }

  def step(a: JFree)(params: P): Task[Unit] = {
    log.debug(s"Viz step: $params")
    val plots = jparam.estimation(params)
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
      val plots = sampleData.plots(data, size.some)
      a.data
        .zip(plots)
        .map {
          case (jdata, plot) =>
            Task(dataset(jdata).addSeries(name, plot.toArray))
        }
        .sequence_
    }

  def dataSize = 0.05d
}

class JFreeSimpleViz[S: SampleVizData]
(implicit fconf: FigureConf)
{
  import shapeless._
  import syntax.std.tuple._

  import JFreeViz._

  implicit def strat = Strategy.sequential

  lazy val renderer = new XYLineAndShapeRenderer(true, false)

  lazy val sampleData = SampleVizData[S]

  import sampleData._

  def plotSet =
    sampleData.projectionRanges
      .map { case (x, y) => data(x, y, renderer, renderer) }

  def init = {
    val d = plotSet ++ plotSet
    val figure = new Figure(fconf.copy(rows = 2), d.map(_.chart))
    JFree(figure, d)
  }

  def setup(a: JFree): Task[Unit] = {
    Task(a.figure.show())
  }

  def samples(a: JFree)(data: List[S]) =
    update(a)(_.samples, 0, 3, "samples", data)

  def estimation(a: JFree)(data: List[S]) =
    update(a)(_.estimation, 3, 6, "estimation", data)

  def update(a: JFree)(dataset: JFreeData => DefaultXYZDataset, from: Int,
    to: Int, name: String, data: List[S]) = {
    val cols = data map (_.feature)
    val plots = sampleData.plots(cols, None)
    a.data.slice(from, to).zip(plots).zipWithIndex
      .map {
        case ((d, plot), i) =>
          Task(dataset(d).addSeries(s"$name $i", plot.toArray))
      }
      .sequence_
  }
}

trait JFreeInstances
{
  implicit def instance_Viz_JFree[S: JSample, P: JParam]
  (implicit fconf: FigureConf) =
    new JFreeViz[S, P]

  implicit def instance_SimpleViz_JFree[S: SampleVizData]
  (implicit fconf: FigureConf) =
    new JFreeSimpleViz[S]
}
