package tryp
package mi
package svm

case class SVMPredictor(config: SVMLearnConf)
extends Predictor[SVM, SVM, Double]
{
  def apply[S: Sample](sample: S, model: SVM)
  (implicit mc: MC[S])
  : Prediction[S, SVM, Double] = {
    val pred = config.eval(model).classify(sample.feature)
    Prediction(sample, model, pred, mc.predictedClass(pred))
  }
}

case class SVMValidator[S: Sample]
(data: Nel[S], config: SVMLearnConf)
(implicit mc: MC[S])
extends Validator[S, SVM, Double]
{
  lazy val predict = SVMPredictor(config)

  def verify(model: SVM)(sample: S): SampleValidation[S, Double] = {
    val pred = predict(sample, model)
    DSV(sample, pred.value)
  }

  def run(model: SVM) =
    Validation(data map(verify(model)))
}

case class SVMModelSelectionValidator[S, P]
(cross: CrossValidator[S, SVM, SVM, Double], cost: Func2)
extends ModelSelectionValidator[S, SVM, SVM, Double]
