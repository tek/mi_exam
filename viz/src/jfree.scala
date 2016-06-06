package tryp
package mi
package viz

import java.awt.{Stroke, Paint}

import org.jfree.data.xy.{DefaultXYZDataset, XYDataset}
import org.jfree.chart.renderer.xy._

import scalax.chart._, api._

import breeze.plot.Plot

import Plotting.ops._

case class JFreeData(chart: XYChart, samples: DefaultXYZDataset,
  estimation: DefaultXYZDataset)

case class JFree[A](figure: Figure, data: List[JFreeData])

trait JFreeInstances
{
  implicit def instance_PlotBackend_JFree[A: SamplePlotting: Sample] =
    new PlotBackend[JFree[A]] {
      type Coords = List[(Double, Double)]

      def samplePlotting = SamplePlotting[A]

      def data = {
        val samples = new DefaultXYZDataset
        val estimation = new DefaultXYZDataset
        val r1 = new XYBubbleRenderer
        val r2 = new XYBubbleRenderer
        val chart = XYLineChart.shapes(samples, "mi")
        chart.plot.setDataset(1, estimation)
        chart.plot.setRenderer(r1)
        chart.plot.setRenderer(1, r2)
        JFreeData(chart, samples, estimation)
      }

      def init = {
        val d = samplePlotting.plotCount.gen(data)
        val figure = new Figure(FigureConf.default("mi"), d.map(_.chart))
        JFree(figure, d)
      }

      def setup(a: JFree[A])(): Unit = {
        a.figure.show()
      }

      def project2d(data: List[Col]) =
        data.toArray.map(a => a(1) -> a(2)).unzip

      def fold(a: JFree[A])(data: List[Col]): Unit = {
        val size = data.length.gen(dataSize).toArray
        update(a)(_.samples, "samples", data, size)
      }

      def step[P: Plotting](a: JFree[A])(params: P) = {
        val plot = params.estimationPlot
        val z = (0 until plot.points.length).map(plot.size).toArray
        update(a)(_.estimation, "estimation", plot.points, z)
      }

      private[this] def update(a: JFree[A])
      (chart: JFreeData => DefaultXYZDataset, name: String, data: List[Col],
        size: Array[Double]) = {
          val plots = samplePlotting.plots(data, size)
          a.data.map(chart).zip(plots).map {
            case (ch, plot) => Task(ch.addSeries(name, plot))
          }
        }

      def dataSize = 0.05d
    }
}

object JFree
extends JFreeInstances
