package tryp
package mi
package mlp
package unit

class IrisSpec
extends IrisSpecBase[Weights, MLP]
with MSVSpec[Iris, Weights, MLP]
{
  def title = "Multilayer Perceptron"

  import MLP._

  val layers = Nel(4)

  override val epsilon = 1e-4d

  // transfer function slope
  def beta = 3.3d

  // gradient coefficient
  def eta = 1.0d

  def bias = true

  // def trials = Some(1)

  override val foldMargin = 0.3d

  lazy val transfer = new Logistic(beta)

  lazy val msv = MLP.msv(data, conf, sconf)

  lazy val conf =
    MLPLearnConf.default(transfer, eta, layers, RandomWeights, bias = bias,
      mode = LearnConf.Online)
}
