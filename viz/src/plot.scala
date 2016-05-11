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
  def estimationCenters(data: P): Array[Array[Double]]
  def estimationPlot(data: P): Scatter
}

case class Scatter(points: List[Col], size: Int => Double)
