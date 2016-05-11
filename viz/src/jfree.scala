package tryp
package mi
package viz

import java.awt.{Stroke, Paint}

import org.jfree.data.xy.{DefaultXYZDataset, XYDataset}
import org.jfree.chart.renderer.xy._

import scalax.chart._, api._

import breeze.plot.Plot

import Plotting.ops._

case class JFreeData
(figure: Figure, chart: XYChart, samples: DefaultXYZDataset,
  estimation: DefaultXYZDataset)

trait JFreeDataInstances
{
  implicit lazy val instance_PlotBackend_JFreeData =
    new PlotBackend[JFreeData] {
      def init = {
        val samples = new DefaultXYZDataset
        val estimation = new DefaultXYZDataset
        val r1 = new XYBubbleRenderer
        val r2 = new XYBubbleRenderer
        val chart = XYLineChart.shapes(samples, "mi")
        chart.plot.setDataset(1, estimation)
        chart.plot.setRenderer(r1)
        chart.plot.setRenderer(1, r2)
        val figure = new Figure(FigureConf.default("mi"), List(chart))
        JFreeData(figure, chart, samples, estimation)
      }

      def setup(a: JFreeData)(): Unit = {
        a.figure.show()
      }

      def project2d(data: List[Col]) =
        data.toArray.map(a => a(1) -> a(2)).unzip

      def fold(a: JFreeData)(data: List[Col]): Unit = {
        val (x, y) = project2d(data)
        val z = data.length.gen(dataSize).toArray
        a.samples.addSeries("samples", Array(x, y, z))
      }

      def step[P: Plotting](a: JFreeData)(params: P) = {
        val s = params.estimationPlot
        val (x, y) = project2d(s.points)
        val z = (0 until x.length).map(s.size).toArray
        a.estimation.addSeries("estimation", Array(x, y, z))
      }

      def dataSize = 0.05d
    }
}

object JFreeData
extends JFreeDataInstances
