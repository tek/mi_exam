package tryp
package mi
package viz

trait PlottedSpecHelpers[A, P, O]
extends MSVSpecBase[A, P, O]
{
  type PMS = PlottedModelSelection[A, JFree[A], P, O]

  override def performableTimeout = 2.minutes

  def stepInterval = 200.millis

  def estimationShape: Shape = Shape.Scatter

  implicit def fconf =
    FigureConf.default("mi", width = 1000, height = 1000,
      shape = estimationShape)

  def mkPms(msv: MSV)
  (implicit paramPlotting: ParamPlotting[P],
    plotBE: PlotBackend[JFree[A]],
    sample: Sample[A]) = 
      PlottedModelSelection[A, JFree[A], P, O](msv, stepInterval)

  def validationError(pm: ModelSelectionValidation[A, P, O]) = {
    pm.printer.short()
    pm.foldError
  }

  def error(s: PMS) =
    s.main map (_ map validationError)

  def trainPms(pms: PMS, margin: Double) =
    error(pms) computes beValid(be_<=(margin))
}

abstract class PlottedSpecBase[A: Sample, P: ParamPlotting, O]
extends PlottedSpecHelpers[A, P, O]
{
  override def is = s2"""
  $title

  plot intermediates while learning a model with cross-validation $plotTrain
  """

  def title: String

  def msv: MSV

  def margin = foldMargin * (trials | sconf.folds)

  val foldMargin = 1e-1d

  def pms: PMS

  lazy val plotTrain =
    error(pms) computes beValid(be_<=(margin))
}

abstract class PlottedIrisSpecBase[P: ParamPlotting, O]
extends PlottedSpecBase[Iris, P, O]
{
  implicit override def fconf = super.fconf

  def pms = mkPms(msv)
}
