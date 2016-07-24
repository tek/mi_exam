package tryp
package mi
package kmeans
package unit

import org.jfree.chart.renderer.xy._

import viz._

trait PlotInstances
{
  implicit lazy val instance_ParamVizData_KMeans =
    new ParamVizData[KMeans] {
      def estimationPlot(data: KMeans): Nel[Dataset] = {
        val sz = data.variances.toArray
        Nel(Dataset(data.centers, sz))
      }
    }

  implicit def instance_JParam_KMeans[A]
  : JParam[KMeans] =
    new JParam[KMeans] {
      def renderer = new XYBubbleRenderer
    }
}
