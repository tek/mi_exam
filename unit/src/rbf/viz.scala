package tryp
package mi
package rbf
package unit

import org.jfree.chart.renderer.xy._

import viz._

trait PlotInstances
{
  implicit lazy val instance_Plot_RBFs_GaussBF =
    new ParamVizData[RBFs[GaussBF]] {
      def estimationPlot(data: RBFs[GaussBF]): Nel[Dataset] = {
        val v = data.bf.map(_.variance / 5d).tail.toArray
        Nel(Dataset(data.centers.tail.toList, v))
      }
    }

  implicit def instance_JParam_RBFs[A]
  : JParam[RBFs[GaussBF]] =
    new JParam[RBFs[GaussBF]] {
      def renderer = new XYBubbleRenderer
    }
}
