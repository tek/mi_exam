package tryp
package mi
package viz

@tc trait PlotBackend[A]
{
  def setup(a: A)(): Unit
  def init: A
  def fold(a: A)(data: List[Col]): Unit
  def step[P: Plotting](a: A)(params: P): Unit
}

@tc abstract class Plotting[P]
extends AnyRef
{
  def estimationPlot(data: P): Scatter
}

@tc abstract class SamplePlotting[A: Sample]
extends AnyRef
{
  def plotCount: Int
  def plots(data: List[Col], size: Array[Double]): List[Array[Array[Double]]]
}

case class Scatter(points: List[Col], size: Int => Double)
