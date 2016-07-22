package tryp
package mi

trait MSVSpecBase[S, P, M, V]
extends Spec
{
  type MSV = ModelSelectionValidator[S, P, M, V]

  def msvError(msv: MSV) = {
    msv.validation.map { _.map { v =>
      v.printer.short()
      v.foldError
    } }
  }

  def train(msv: MSV, margin: Double) =
    msvError(msv) computes beValid(be_<=(margin))

  def trials: Option[Int] = None

  def epsilon = 1e-2d

  def folds = 10

  lazy val sconf = ModelSelectionConf.default(
    epsilon = epsilon,
    trials = trials,
    folds = folds,
  )
}

trait MSVSpec[S, P, M, V]
extends MSVSpecBase[S, P, M, V]
{
  def is = s2"""
  $title

  learn a model with cross-validation ${train(msv, margin)}
  """

  def title: String

  val msv: MSV

  def margin = foldMargin * (trials | sconf.folds)

  val foldMargin = 1e-1d
}
