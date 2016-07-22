package tryp
package mi
package ica

import cats.data.Validated._

import breeze.linalg._
import breeze.numerics._

case class ICAEstimator[S: Sample]
(data: Nel[S], config: ICALearnConf)
extends SimpleEstimator[ICA]
{
  lazy val n = data.length.toDouble

  def go = {
    ICA(Mat.zeros(1, 1)).valid
  }
}
