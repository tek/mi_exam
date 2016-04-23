package tryp
package mi
package mlp

class IrisSpec
extends IrisSpecBase
{
  def title = "Multilayer Perceptron"

  import MLP._

  val layers = Nel(4)

  val epsilon = 1e-4d

  // transfer function slope
  def beta = 3.3d

  // gradient coefficient
  def eta = 1.0d

  def bias = true

  // def trials = Some(1)

  override val foldMargin = 0.3d

  lazy val transfer = new Logistic(beta)

  implicit lazy val conf =
    MLPLearnConf.default(transfer, eta, layers, RandomWeights,
      bias = bias, mode = LearnConf.Online)

  lazy val msv = MLP.msv(data, conf, sconf)

  def main = {
    msv.logInfoShort()
    msv.totalError must be_<=(margin)
  }
}
