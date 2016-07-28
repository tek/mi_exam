package tryp
package mi
package mlp
package unit

import MLP._

class IrisSpec
extends MSVSpec[Iris, Weights, MLP, Double, MLPLearnConf]
with IrisSpecBase[Weights, MLP, Double]
{
  def title = "Multilayer Perceptron"

  import MLP._

  val layers = Nel(4)

  override val epsilon0 = 1e-4d

  // transfer function slope
  def beta = 3.3d

  // gradient coefficient
  def eta = 1.0d

  def bias = true

  // def trials = Some(1)

  override val foldMargin = 0.3d

  lazy val transfer = new Logistic(beta)

  implicit lazy val conf =
    MLPLearnConf.default(transfer, eta, layers, RandomWeights, bias = bias,
      mode = LearnConf.Online)
}
