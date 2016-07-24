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

@tc abstract class SampleVizData[S: Sample]
(implicit val mc: ModelClassesBase[S])
extends AnyRef
{
  type Range = (Double, Double)

  implicit lazy val sample = Sample[S]

  def ranges: List[Range]
  def plotCount: Int
  def projections: List[(Int, Int)]

  def projectionRanges =
    projections.map {
      case (x, y) => ranges(x) -> ranges(y)
    }

  def plots(data: List[Col], size: Option[Array[Double]])
  : List[Tuple3[Array[Double], Array[Double], Array[Double]]] =
  {
    val sz = size | data.length.gen(1d).toArray
    projections map {
      case (a, b) =>
        (data.map(_(a)).toArray, data.map(_(b)).toArray, sz)
    }
  }
}

case class Dataset(points: List[Col], size: Array[Double])
