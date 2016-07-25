package tryp
package mi
package kmeans

import breeze._
import numerics._
import linalg._
import util._

case class KMeansPredictor(config: KMeansLearnConf)
{
  def apply[S: Sample](sample: S, model: KMeans)
  (implicit mc: MC[S])
  : Prediction[S, KMeans, Col] = {
    val x = sample.feature
    val pred = model.centers.nelOption map {
      _ minBy (a => norm(a - x))
    }
    val c = pred getOrElse x
    Prediction(sample, model, c, mc.predictedClass(c))
  }
}

case class KMeansValidator[S: Sample]
(data: Nel[S], config: KMeansLearnConf)
(implicit mc: MC[S])
extends Validator[KMeans]
{
  lazy val predict = KMeansPredictor(config)

  def verify(model: KMeans)(sample: S): SV[S, Col] = {
    val pred = predict(sample, model)
    ColSV(sample, pred.value)
  }

  def run(model: KMeans) = {
    Val(data map verify(model))
  }
}
