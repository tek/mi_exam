package tryp
package mi
package mlp

case class MLPModelCreator(config: MLPLearnConf)
extends ModelCreator[Weights, MLP]
{
  def run(est: Est[Weights]): MLP =
    MLP.init(Col(0), config.transfer.deriv, est.params)
}
