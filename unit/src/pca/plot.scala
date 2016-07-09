package tryp
package mi
package pca
package unit

import viz._

trait PlotInstances
{
  implicit lazy val instance_ParamPlotting_PCA =
    new ParamPlotting[PCA] {
      def estimationPlot(data: PCA): Dataset = {
        Dataset(List(Col(0d, 0d)), Array(1d))
      }
    }
}
