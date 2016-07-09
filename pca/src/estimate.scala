package tryp
package mi
package pca

import cats.data.Validated._

import breeze.linalg._
import breeze.numerics._

case class PCAEstimator[S: Sample]
(data: Nel[S], config: PCALearnConf)
extends SimpleEstimator[PCA]
{
  def x = data.map(_.feature).unwrap

  def xm = sum(x) / x.length.toDouble

  def centered = x map (_ - xm)

  def go = Valid(PCA(Mat(1d)))
}
