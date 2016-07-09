package tryp
package mi
package pca

case class PCAPredictor(config: PCALearnConf)
extends Predictor[PCA, Double]
{
  def apply[S: Sample](sample: S, model: PCA)
  : Prediction[S, PCA, Double] = ???
}

case class PCAValidator[S: Sample]
(data: Nel[S], config: PCALearnConf)
extends Validator[S, PCA, Double]
{
  lazy val predict = PCAPredictor(config)

  def verify(model: PCA)(sample: S): SampleValidation[S, Double] = {
    val pred = predict(sample, model)
    SV(sample, pred.value)
  }

  def run(model: PCA) = {
    val pred = data map(verify(model))
    Validation(pred)
  }
}

case class PCAModelSelectionValidator[S, P]
(cross: CrossValidator[S, PCA, PCA, Double], cost: Func2)
extends ModelSelectionValidator[S, PCA, Double]
