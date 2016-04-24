package tryp
package mi

trait Optimizer[M, V]
{
  def apply[S: Sample](a: Prediction[S, M, V]): M
}
