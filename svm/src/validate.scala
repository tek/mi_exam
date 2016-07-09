package tryp
package mi
package svm

case class SVMPredictor(config: SVMLearnConf)
extends Predictor[SVM, Double]
{
  def apply[S: Sample](sample: S, model: SVM)
  : Prediction[S, SVM, Double] = {
    val pred = config.eval(model).classify(sample.feature)
    Prediction(sample, model, pred, Sample[S].predictedClass(pred))
  }
}

case class SVMValidator[S: Sample]
(data: Nel[S], config: SVMLearnConf)
extends Validator[S, SVM, Double]
{
  lazy val predict = SVMPredictor(config)

  def verify(model: SVM)(sample: S): SampleValidation[S, Double] = {
    val pred = predict(sample, model)
    SV(sample, pred.value)
  }

  def run(model: SVM) = {
    val pred = data map(verify(model))
    Validation(pred)
  }
}

case class SVMModelSelectionValidator[S, P]
(cross: CrossValidator[S, SVM, SVM, Double], cost: Func2)
extends ModelSelectionValidator[S, SVM, Double]
