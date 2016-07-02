package tryp
package mi
package rbf
package unit

import fs2._, util._

import breeze.plot._

import viz._

trait PlotInstances
{
  implicit lazy val instance_Plot_RBFs_GaussBF =
    new ParamPlotting[RBFs[GaussBF]] {
      def estimationPlot(data: RBFs[GaussBF]): Dataset = {
        val v = data.bf.map(_.variance / 5d).unwrap.toArray
        Dataset(data.centers.unwrap.toList, v)
      }
    }
}
