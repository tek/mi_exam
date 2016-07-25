package tryp
package mi

case class MSConf(steps: Int, epsilon0: Double, epsilon: Double, folds: Int,
  trials: Option[Int], cost: Func2)

object MSConf
{
  def default(
    steps: Int = 10000,
    epsilon0: Double = 1e-4d,
    epsilon: Double = 1e-4d,
    folds: Int = 10,
    trials: Option[Int] = None,
    cost: Func2 = QuadraticError
    ) =
      MSConf(steps, epsilon0, epsilon, folds, trials, cost)
}
