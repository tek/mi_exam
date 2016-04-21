package tryp
package mi
package rbf

class IrisSpec
extends Spec
{
  def is = s2"""
  main $main
  """

  type P = GaussBF

  val steps = 1000

  val centroids = 3

  val eta = 0.1d

  val lambda = 2d

  val epsilon = 0.000000001d

  // def trials = None
  def trials = Some(1)

  lazy val data = Iris.loadNel

  val cost = QuadraticError

  implicit lazy val conf =
    RBFLearnConf.default[P, Iris](steps, centroids, eta, lambda,
      LearnConf.Batch)

  val stop = RBFConvergenceStopCriterion[P](steps, epsilon)

  lazy val validator = CrossValidator[Iris, Params[P], RState](
    15, data, RBFEstimator[Iris, P](_, conf),
    RBFValidator[Iris, P](_, conf), stop, trials)

  def main = {
    val result = cats.data.XorT(validator.result)
    val errors = result.swap.collectRight
    val results = result.collectRight
    val stats = results.map(_.validation.stats(cost))
    val error = stats.map(_.total).sum / data.length
    results.foreach {
      case Model(Estimation(iter, _), Validation(data)) =>
        if (iter == steps) log.info("training hasn't converged")
        else log.info(s"training converged after $iter iterations")
        data.unwrap.foreach { res => log.info(res.info) }
    }
    error must be_<=(0.0003d * trials.getOrElse(data.length))
  }
}
