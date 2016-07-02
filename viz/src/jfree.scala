package tryp
package mi
package viz

import fs2.util.Task
import fs2.Strategy

import org.jfree.data.xy.{DefaultXYZDataset, XYDataset}
import org.jfree.chart.renderer.xy._
import org.jfree.chart.axis._

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

      def estimationRenderer =
        fconf.shape match {
          case Shape.Line => new XYLineAndShapeRenderer(true, false)
          case Shape.Scatter => new XYBubbleRenderer
        }

      def data(xrange: (Double, Double), yrange: (Double, Double)) = {
        val samples = new DefaultXYZDataset
        val estimation = new DefaultXYZDataset
        val r1 = new XYBubbleRenderer
        val ax = new NumberAxis
        val ay = new NumberAxis
        ax.setAutoRange(false)
        ax.setRange(xrange._1, xrange._2)
        ay.setAutoRange(false)
        ay.setRange(yrange._1, yrange._2)
        val plot = new XYPlot(samples, ax, ay, r1)
        plot.setDataset(0, samples)
        plot.setDataset(1, estimation)
        plot.setRenderer(0, r1)
        plot.setRenderer(1, estimationRenderer)
        val chart = XYChart(plot, "mi", false, ChartTheme.Default)
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

      def fold(a: JFree[A])(data: List[Col]) = {
        log.debug(s"Plotting fold: $data")
        val size = data.length.gen(dataSize).toArray
        update(a)(_.samples, "samples", data, size)
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
          val tasks = a.data.zip(plots).map {
            case (jdata, plot) => Task {
              dataset(jdata).addSeries(name, plot)
            }
          }
          tasks.sequence_
        }

      def dataSize = 0.05d
    }
}

object JFree
extends JFreeInstances
