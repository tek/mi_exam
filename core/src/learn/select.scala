package tryp
package mi

case class ModelSelectionConf(steps: Int, epsilon: Double, folds: Int,
  trials: Option[Int])

object ModelSelectionConf
{
  def default(
    steps: Int = 10000,
    epsilon: Double = 1e-4d,
    folds: Int = 10,
    trials: Option[Int] = None
    ) =
      ModelSelectionConf(steps, epsilon, folds, trials)
}
