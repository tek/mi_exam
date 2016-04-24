package tryp
package mi

case class Prediction[S, M, V](sample: S, model: M, value: V)

trait Predictor[M, V]
{
  def apply[S: Sample](sample: S, model: M): Prediction[S, M, V]
}
