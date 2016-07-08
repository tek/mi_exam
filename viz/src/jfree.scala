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

import ParamPlotting.ops._

case class JFreeData(chart: XYChart, samples: DefaultXYZDataset,
  estimation: DefaultXYZDataset)

case class JFree[A](figure: Figure, data: List[JFreeData])

trait JFreeInstances
extends Logging
{
  implicit def strat = Strategy.sequential

  implicit def instance_PlotBackend_JFree[A: SamplePlotting: Sample]
  (implicit fconf: FigureConf) =
    new PlotBackend[JFree[A]] {
      type Coords = List[(Double, Double)]

      def samplePlotting = SamplePlotting[A]

      lazy val estimationRenderer = {
        val r = fconf.shape match {
          case Shape.Line => new XYLineAndShapeRenderer(true, false)
          case Shape.Scatter => new XYBubbleRenderer
        }
        r.setSeriesOutlinePaint(0, Color.blue)
        r.setSeriesFillPaint(0, Color.blue)
        r
      }

      val sampleShape = new Ellipse2D.Double(-4.0, -4.0, 8.0, 8.0)

      val testSampleColor = Color.gray

      lazy val sampleRenderer = {
        val r = new XYLineAndShapeRenderer(false, true)
        r.setBaseOutlinePaint(Color.black)
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

      def colors = List(Color.yellow, Color.green, Color.red)

      def color(i: Int) =
        colors.lift(i % colors.length).getOrElse(Color.orange)

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
        plot.setRenderer(0, sampleRenderer)
        plot.setRenderer(1, estimationRenderer)
        plot.setBackgroundPaint(Color.white)
        JFreeData(chart, samples, estimation)
      }

      def init = {
        val d = samplePlotting.projectionRanges
          .map { case (x, y) => data(x, y) }
        val rows = Math.ceil(Math.sqrt(samplePlotting.plotCount)).toInt
        val figure = new Figure(fconf.copy(rows = rows), d.map(_.chart))
        JFree(figure, d)
      }

      def setup(a: JFree[A]): Task[Unit] = {
        Task(a.figure.show())
      }

      def fold[B: Sample](a: JFree[A])
      (train: List[B], test: List[B]): Task[Unit] = {
        log.debug(s"Plotting fold: $train")
        Sample[B].classes.unwrap.zipWithIndex
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

      def step[P: ParamPlotting](a: JFree[A])(params: P) = {
        log.debug(s"Plotting step: $params")
        val plot = params.estimationPlot
        val z = (0 until plot.points.length).map(plot.size).toArray
        update(a)(_.estimation, "estimation", plot.points, z)
      }

      private[this] def update(a: JFree[A])
      (dataset: JFreeData => DefaultXYZDataset, name: String, data: List[Col],
        size: Array[Double]) = {
          val plots = samplePlotting.plots(data, size)
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
}

object JFree
extends JFreeInstances
