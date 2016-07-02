package tryp
package mi
package svm
package unit

import viz._
import breeze.linalg.normalize

trait PlotInstances
{
  implicit lazy val instance_ParamPlotting_SVM =
    new ParamPlotting[SVM] {
      def intersect(data: SVM, v: Col) = {
        data.offset / (data.normal dot v) * v
      }

      def estimationPlot(data: SVM): Dataset = {
        import data._
        val rank = normal.length
        val b1 = intersect(data, normal)
        val b2 = intersect(data, normal + Col.ones[Double](rank))
        val dir = normalize(b2 - b1)
        val x1 = b1 + 20d * dir
        val x2 = b1 - 20d * dir
        Dataset(List(x1, x2), Array(1d, 1d))
      }
    }
}
