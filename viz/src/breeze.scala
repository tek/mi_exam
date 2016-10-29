package tryp
package mi
package viz

import fs2.Task
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

import ParamVizData.ops._

trait BreezeInstances
{
  implicit def instance_PlotBackend_BreezeData
  [S: Sample, P: ParamVizData] =
    new Viz[BreezeData, S, P] {
      implicit def strat = Strategy.sequential

      def init = BreezeData(BreezeFigure())

      def setup(a: BreezeData) = Task(())

      def fold(a: BreezeData)(s: List[S], test: List[S]) = {
        val b = s.map(_.feature)
        Task {
          a.plot += scatterPlot(Dataset(b, b.length.gen(dataSize).toArray))
        }
      }

      def step(a: BreezeData)(params: P) = {
        Task {
          a.clear()
          params.estimationPlot map scatterPlot map a.plot.+=
          a.refresh()
        }
      }

      def dataSize = 0.05d

      def scatterPlot(data: Dataset) = {
        val (x, y) = data.points.map(a => a(1) -> a(2)).unzip
        scatter(x, y, size = data.size)
      }
    }
}

object BreezeData
extends BreezeInstances
