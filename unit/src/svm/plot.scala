package tryp
package mi
package svm
package unit

import viz._

trait PlotInstances
{
  implicit lazy val instance_Plot_SVM =
    new Plotting[SVM] {
      def estimationPlot(data: SVM): Scatter = {
        Scatter(List(), List())
      }
    }
}
