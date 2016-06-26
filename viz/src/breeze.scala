package tryp
package mi
package viz

import fs2.util.Task
import fs2.Strategy

import org.jfree.data.xy.DefaultXYZDataset

import breeze.plot.{Figure => BreezeFigure, _}

case class BreezeData(figure: BreezeFigure)
{
  def plotNum = 0

  lazy val plot = figure.subplot(plotNum)

  def clear() = figure.clearPlot(plotNum)

  def refresh() = figure.refresh()
}

import Plotting.ops._

trait BreezeInstances
{
  implicit lazy val instance_PlotBackend_BreezeData =
    new PlotBackend[BreezeData] {
      implicit def strat = Strategy.sequential

      def init = BreezeData(BreezeFigure())

      def setup(a: BreezeData) = Task(())

      def fold(a: BreezeData)(b: List[Col]) = {
        Task {
          a.plot += scatterPlot(Scatter(b, _ => dataSize))
        }
      }

      def step[P: Plotting](a: BreezeData)(params: P) = {
        Task {
          a.clear()
          a.plot += scatterPlot(params.estimationPlot)
          a.refresh()
        }
      }

      def dataSize = 0.05d

      def scatterPlot(data: Scatter) = {
        val (x, y) = data.points.map(a => a(1) -> a(2)).unzip
        scatter(x, y, size = data.size)
      }
    }
}

object BreezeData
extends BreezeInstances