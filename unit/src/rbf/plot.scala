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
    new Plotting[RBFs[GaussBF]] {
      def estimationCenters(data: RBFs[GaussBF]): Array[Array[Double]] = {
        data.centers.unwrap.toArray.map(_.toArray)
      }

      def estimationPlot(data: RBFs[GaussBF]): Scatter = {
        val v = data.bf.map(_.variance / 5d).unwrap
        Scatter(data.centers.unwrap.toList, v)
      }
    }
}
