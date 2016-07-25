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
extends Validator[SVM]
{
  lazy val predict = SVMPredictor(config)

  def verify(model: SVM)(sample: S): SV[S, Double] = {
    val pred = predict(sample, model)
    DSV(sample, pred.value)
  }

  def run(model: SVM) =
    Val(data map(verify(model)))
}
