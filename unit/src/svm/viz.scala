package tryp
package mi
package svm
package unit

import org.jfree.chart.renderer.xy._

import breeze.linalg.normalize

import viz._

trait PlotInstances
{
  implicit lazy val instance_ParamViz_SVM =
    new ParamVizData[SVM] {
      def intersect(data: SVM, v: Col) = {
        data.offset / (data.normal dot v) * v
      }

      def estimationPlot(data: SVM): Nel[Dataset] = {
        import data._
        val rank = normal.length
        val b1 = intersect(data, normal)
        val b2 = intersect(data, normal + Col.ones[Double](rank))
        val dir = normalize(b2 - b1)
        val x1 = b1 + 20d * dir
        val x2 = b1 - 20d * dir
        Nel(Dataset(List(x1, x2), Array(1d, 1d)))
      }
    }

  implicit def instance_JParam_SVM[A]
  : JParam[SVM] =
    new JParam[SVM] {
      def renderer = new XYLineAndShapeRenderer(true, false)
    }
}
