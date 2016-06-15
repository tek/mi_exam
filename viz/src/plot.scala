package tryp
package mi
package viz

import fs2.util.Task

@tc trait PlotBackend[A]
{
  def setup(a: A): Task[Unit]
  def init: A
  def fold(a: A)(data: List[Col]): Task[Unit]
  def step[P: Plotting](a: A)(params: P): Task[Unit]
}

@tc abstract class Plotting[P]
extends AnyRef
{
  def estimationPlot(data: P): Scatter
}

@tc abstract class SamplePlotting[A: Sample]
extends AnyRef
{
  type Range = (Double, Double)

  def ranges: List[Range]
  def plotCount: Int
  def plots(data: List[Col], size: Array[Double]): List[Array[Array[Double]]]
  def projectionRanges: List[(Range, Range)]
}

case class Scatter(points: List[Col], size: Int => Double)
