package tryp
package mi
package ica

case class ICAPredictor(config: ICALearnConf)
extends Predictor[ICA, ICA, Double]
{
  def apply[S: Sample](sample: S, model: ICA)
  (implicit mc: MC[S])
  : Prediction[S, ICA, Double] =
    Prediction(sample, model, 1d, mc.predictedClass(1d))
}

case class ICAValidator[S: Sample]
(data: Nel[S], config: ICALearnConf)
(implicit mc: MC[S])
extends Validator[ICA]
{
  lazy val predict = ICAPredictor(config)

  def verify(model: ICA)(sample: S): SampleValidation = {
    val pred = predict(sample, model)
    DSV(sample, pred.value)
  }

  def run(model: ICA) = {
    val pred = data map(verify(model))
    Val(pred)
  }
}
