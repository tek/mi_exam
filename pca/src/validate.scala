package tryp
package mi
package pca

case class PCAPredictor(config: PCALearnConf)
extends Predictor[PCA, PCA, Double]
{
  def apply[S: Sample](sample: S, model: PCA)
  (implicit mc: MC[S])
  : Prediction[S, PCA, Double] =
    Prediction(sample, model, 1d, mc.predictedClass(1d))
}

case class PCAValidator[S: Sample]
(data: Nel[S], config: PCALearnConf)
(implicit mc: MC[S])
extends Validator[PCA]
{
  lazy val predict = PCAPredictor(config)

  def verify(model: PCA)(sample: S): SampleValidation = {
    val pred = predict(sample, model)
    DSV(sample, pred.value)
  }

  def run(model: PCA) = {
    val pred = data map(verify(model))
    Val(pred)
  }
}
