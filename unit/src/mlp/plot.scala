package tryp
package mi
package mlp
package unit

import viz._

import fs2._, util._

import breeze.plot._

trait PlottingInstances
{
  implicit lazy val instance_Plotting_Weights =
    new ParamPlotting[Weights] {
      def estimationCenters(data: Weights): Array[Array[Double]] = {
        Array(Array())
      }

      def estimationPlot(data: Weights): Dataset = {
        Dataset(List(), Array(1d))
      }
    }
}
