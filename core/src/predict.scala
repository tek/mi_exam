package tryp
package mi

case class Prediction[S, M, V](sample: S, model: M, value: V,
  cls: ModelClass[S])

trait Predictor[P, M, V]
{
  def apply[S: Sample](sample: S, model: P)
  (implicit mc: ModelClasses[S, V])
  : Prediction[S, M, V]
}
