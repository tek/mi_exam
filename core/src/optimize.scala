package tryp
package mi

trait Optimizer[P, M, V]
{
  def apply[S: Sample](a: Prediction[S, M, V])
  (implicit mc: ModelClasses[S, V]): P
}
