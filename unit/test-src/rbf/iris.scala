package tryp
package mi
package rbf
package unit

trait IrisSpec
extends IrisSpecBase[RBFs[GaussBF], Double]
{
  def title = "Radial Basis Functions"

  val rbfs = 3

  val eta = 1d

  val lambda = 2d

  // override def trials = Some(1)

  lazy val conf = RBFLearnConf.default[GaussBF, Iris](rbfs, eta, lambda)

  lazy val msv = RBF.msv(data, conf, sconf)
}

class PlottedIrisSpec
extends PlottedIrisSpecBase[RBFs[GaussBF], Double]
with IrisSpec
