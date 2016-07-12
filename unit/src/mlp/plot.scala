package tryp
package mi
package mlp
package unit

import viz._

import fs2._, util._

import breeze.plot._

trait VizInstances
{
  implicit lazy val instance_Viz_Weights =
    new ParamVizData[Weights] {
      def estimationCenters(data: Weights): Array[Array[Double]] = {
        Array(Array())
      }

      def estimationPlot(data: Weights): Nel[Dataset] = {
        Nel(Dataset(List(), Array(1d)))
      }
    }
}
