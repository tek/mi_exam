package tryp
package mi
package svm
package unit

trait IrisSpec
extends IrisSpecBase[SVM, Double]
{
  def title = "Support Vector Machine"

  val eta = 1d

  val lambda = 2d

  override def trials = Some(1)

  lazy val conf = SVMLearnConf.default(lambda)

  lazy val msv = SVM.msv(data, conf, sconf)
}

class NormalIrisSpec
extends IrisSpec

class PlottedIrisSpec
extends PlottedIrisSpecBase[SVM, Double]
with IrisSpec
