package tryp
package mi

import viz._

trait IrisSpecBase[P, O]
extends MSVSpec[Iris, P, O]
{
  lazy val data = Iris.loadNel
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

  def validationError(pm: ModelSelectionValidation[Iris, P, O]) = {
    pm.printer.short()
    pm.foldError
  }

  lazy val error =
    pms.main map (_ map validationError)

  lazy val plotTrain =
    error computes beValid(be_<=(margin))
}
