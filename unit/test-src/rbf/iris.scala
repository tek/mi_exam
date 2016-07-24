package tryp
package mi
package rbf
package unit

import viz._

trait IrisSpec
extends IrisSpecBase[RBFs[GaussBF], RBFs[GaussBF], Double]
{
  def title = "Radial Basis Functions"

  val rbfs = 3

  val eta = 1d

  val lambda = 2d

  // override def trials = Some(1)

  lazy val conf = RBFLearnConf.default[GaussBF, Iris](rbfs, eta, lambda)

  lazy val msv = RBF.msv(data, conf, sconf)
}

class NormalIrisSpec
extends Spec
with IrisSpec
with MSVSpec[Iris, RBFs[GaussBF], RBFs[GaussBF], Double]

class PlottedIrisSpec
extends PlottedIrisSpecBase[RBFs[GaussBF], RBFs[GaussBF], Double]
with IrisSpec
