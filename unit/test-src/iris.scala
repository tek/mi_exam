package tryp
package mi

import viz._

abstract class IrisSpecBase[P, O]
extends Spec
{
  def is = s2"""
  $title

  learn a model with cross-validation $train
  """

  def title: String

  lazy val data = Iris.loadNel

  def trials: Option[Int] = None

  lazy val sconf = ModelSelectionConf.default(
    epsilon = 1e-2d,
    trials = trials,
    folds = 10
    )

  val foldMargin = 1e-1d

  def margin = foldMargin * (trials | sconf.folds)

  val msv: ModelSelectionValidator[Iris, P, O]

  def train = {
    msv.printer.short()
    msv.unsafeValidation.foldError must be_<=(margin)
  }
}

abstract class PlottedIrisSpecBase[P: Plotting, O]
extends IrisSpecBase[P, O]
{
  override def is = s2"""
  $title

  plot intermediates while learning a model with cross-validation $plotTrain
  """

  override def performableTimeout = 2.minutes

  // override def trials = 1.some

  def stepInterval = 200.millis

  implicit def fconf = FigureConf.default("mi", width = 1000, height = 1000)

  lazy val pms =
    PlottedModelSelection[Iris, JFree[Iris], P, O](msv, stepInterval)

  lazy val error =
    pms.main
      .map { valid =>
        valid.printer.short()
        valid.foldError
      }

  lazy val plotTrain =
    error computes be_<=(margin)
}
