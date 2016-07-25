package tryp
package mi
package rbf
package unit

import viz._

import RBF._

trait IrisSpec
extends IrisSpecBase[RBFs[GaussBF], RBFNet[GaussBF], Double]
{
  def title = "Radial Basis Functions"

  val rbfs = 3

  val eta = 1d

  val lambda = 2d

  // override def trials = Some(1)

  lazy val conf = RBFLearnConf.default[GaussBF, Iris](rbfs, eta, lambda)
}

class NormalIrisSpec
extends MSVSpec[
Iris, RBFs[GaussBF], RBFNet[GaussBF], Double, RBFLearnConf[GaussBF]]
with IrisSpec

class PlottedIrisSpec
extends PlottedIrisSpecBase[
RBFs[GaussBF], RBFNet[GaussBF], Double, RBFLearnConf[GaussBF]]
with IrisSpec
