package tryp
package mi
package rbf

import breeze._
import linalg._
import functions._
import numerics._

import LearnConf._

case class RBFPredictor[P: BasisFunction](config: RBFLearnConf[P])
extends Predictor[RBFNet[P], RBFNet[P], Double]
{
  def rbfOut(data: Col, bf: Nev[P]): Nev[Double] = {
    bf.map(_.output(data))
  }

  def apply[S: Sample](sample: S, model: RBFNet[P])
  (implicit mc: MC[S])
  : Prediction[S, RBFNet[P], Double] = {
    val r = Col(rbfOut(sample.feature, model.bf).unwrap.toArray)
    val pred = model.weights.t * r
    Prediction(sample, model, pred, mc.predictedClass(pred))
  }
}

case class RBFValidator[S: Sample, P: BasisFunction]
(data: Nel[S], config: RBFLearnConf[P])
(implicit mc: MC[S])
extends Validator[RBFNet[P]]
{
  lazy val predict = RBFPredictor[P](config)

  def verify(model: RBFNet[P])(sample: S): SampleValidation =
  {
    val pred = predict(sample, model)
    DSV(sample, pred.value)
  }

  def run(model: RBFNet[P]) = {
    val pred = data map(verify(model))
    Val(pred)
  }
}
