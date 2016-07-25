package tryp
package mi

case class MSConf(steps: Int, epsilon: Double, folds: Int,
  trials: Option[Int], cost: Func2)

object MSConf
{
  def default(
    steps: Int = 10000,
    epsilon: Double = 1e-4d,
    folds: Int = 10,
    trials: Option[Int] = None,
    cost: Func2 = QuadraticError
    ) =
      MSConf(steps, epsilon, folds, trials, cost)
}
