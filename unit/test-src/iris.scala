package tryp
package mi

trait IrisSpecBase
extends Spec
{
  def is = s2"""
  $title

  learn a model with cross-validation $train
  """

  def title: String

  lazy val data = Iris.loadNel

  def trials = None

  lazy val sconf = ModelSelectionConf.default(
    epsilon = 1e-8d,
    trials = trials)

  val foldMargin = 1e-1d

  def margin = foldMargin * (trials | sconf.folds)

  val msv: ModelSelectionValidator[_, _, _]

  def train = {
    msv.logInfoShort()
    msv.foldError must be_<=(margin)
  }
}
