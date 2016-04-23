package tryp
package mi
package rbf

class IrisSpec
extends IrisSpecBase
{
  def title = "Radial Basis Functions"

  val rbfs = 3

  val eta = 1d

  val lambda = 2d

  // override def trials = Some(1)

  lazy val conf = RBFLearnConf.default[GaussBF, Iris](rbfs, eta, lambda)

  lazy val msv = RBF.msv(data, conf, sconf)
}
