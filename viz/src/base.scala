package tryp
package mi
package viz

import fs2.util.Task

abstract class Viz[A, S, P]
{
  def setup(a: A): Task[Unit]
  def init: A
  def fold(a: A)(train: List[S], test: List[S]): Task[Unit]
  def step(a: A)(params: P): Task[Unit]
}

@tc trait ParamVizData[P]
{
  def estimationPlot(data: P): Nel[Dataset]
}

@tc abstract class SampleVizData[A: Sample]
extends AnyRef
{
  type Range = (Double, Double)

  def sample = Sample[A]

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
