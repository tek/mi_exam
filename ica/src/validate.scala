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
extends Validator[S, ICA, Double]
{
  lazy val predict = ICAPredictor(config)

  def verify(model: ICA)(sample: S): SampleValidation[S, Double] = {
    val pred = predict(sample, model)
    DSV(sample, pred.value)
  }

  def run(model: ICA) = {
    val pred = data map(verify(model))
    Validation(pred)
  }
}

case class ICAModelSelectionValidator[S, P]
(cross: CrossValidator[S, ICA, ICA, Double], cost: Func2)
extends MSV[S, ICA, ICA, Double]
