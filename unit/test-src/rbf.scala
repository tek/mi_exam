package tryp
package mi
package rbf

import spire.math._
import spire.algebra._
import spire.implicits._
import spire.random._

class RBFSpec
extends Spec
{
  def is = s2"""
  main $main
  """

  type P = GaussParams

  val steps = 10000

  val epsilon = 0.00001

  def trials = 1.some

  lazy val data = Iris.loadNel

  val cost = QuadraticError

  implicit lazy val conf =
    RBFLearnConf.default()

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
        hl
        if (iter == steps) p("training hasn't converged")
        else p(s"training converged after $iter iterations")
        data.unwrap.foreach { res => p(res.info) }
    }
    error must be_<=(0.0003d * trials.getOrElse(data.length))
  }
}
