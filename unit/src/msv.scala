package tryp
package mi

trait MSVSpecBase[S, P, M, V]
extends tryp.SpecCommon
with mi.Matchers
{
  type MSV0 = MSV[S, P, M, V]

  def msvError(msv: MSV0) = {
    msv.validation.map { _.map { v =>
      v.printer.short()
      v.foldError
    } }
  }

  def train(msv: MSV0, margin: Double) =
    msvError(msv) computes beValid(be_<=(margin))

  def trials: Option[Int] = None

  def epsilon = 1e-2d

  def folds = 10

  val sconf = ModelSelectionConf.default(
    epsilon = epsilon,
    trials = trials,
    folds = folds,
  )
}

trait MSVSpec[S, P, M, V]
extends Spec
with MSVSpecBase[S, P, M, V]
{
  def is = s2"""
  $title

  learn a model with cross-validation ${train(msv, margin)}
  """

  def title: String

  val msv: MSV0

  def margin = foldMargin * (trials | sconf.folds)

  val foldMargin = 1e-1d
}
