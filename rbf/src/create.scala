package tryp
package mi
package rbf

import breeze._
import linalg._

case class RBFModelCreator[S: Sample, P: BasisFunction]
(data: Nel[S], config: RBFLearnConf[P])
(implicit mc: MC[S])
extends ModelCreator[RBFs[P], RBFNet[P]]
{
  lazy val predict = RBFPredictor[P](config)

  def features = data map(_.feature)

  lazy val targets = Col(data map(_.valueOrNaN) tail: _*)

  def weights(rbfs: RBFs[P]) = {
    val out = features.map(predict.rbfOut(_, rbfs.bf).tail.toArray).tail
    val phi = Mat(out: _*)
    val trans = phi.t
    val leftCoeff = trans * phi
    val rightCoeff = trans * targets
    pinv(leftCoeff) * rightCoeff
  }

  def run(est: Est[RBFs[P]]): RBFNet[P] = {
    val rbfs = est.params
    RBFNet(rbfs, weights(rbfs))
  }
}
