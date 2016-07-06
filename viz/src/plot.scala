package tryp
package mi
package viz

import fs2.util.Task

@tc trait PlotBackend[A]
{
  def setup(a: A): Task[Unit]
  def init: A
  def fold[B: Sample](a: A)(data: List[B]): Task[Unit]
  def step[P: ParamPlotting](a: A)(params: P): Task[Unit]
}

@tc trait ParamPlotting[P]
{
  def estimationPlot(data: P): Dataset
}

@tc abstract class SamplePlotting[A: Sample]
extends AnyRef
{
  type Range = (Double, Double)

  def ranges: List[Range]
  def plotCount: Int
  def projections: List[(Int, Int)]

  def projectionRanges =
    projections.map {
      case (x, y) => ranges(x) -> ranges(y)
    }

  def plots(data: List[Col], size: Array[Double]): List[Array[Array[Double]]] =
  {
    projections map {
      case (a, b) =>
        Array(data.map(_(a)).toArray, data.map(_(b)).toArray, size)
    }
  }
}

case class Dataset(points: List[Col], size: Array[Double])
