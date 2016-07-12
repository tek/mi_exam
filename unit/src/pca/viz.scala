package tryp
package mi
package pca
package unit

import org.jfree.chart.renderer.xy._

import viz._

trait PlotInstances
{
  implicit lazy val instance_ParamViz_PCA =
    new ParamVizData[PCA] {
      def estimationPlot(data: PCA): Nel[Dataset] = {
        data.scaledBasis
          .map(a => Dataset(List(data.µ, data.µ + a), Array(1d, 1d)))
          .nel(Dataset(List(), Array(1d)))
      }
    }

  implicit def instance_JParam_PCA[A]
  : JParam[PCA] =
    new JParam[PCA] {
      def renderer = new XYLineAndShapeRenderer(true, false)
    }
}
