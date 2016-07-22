package tryp
package mi
package viz

trait PlottedSpecHelpers[A, P, M, V]
extends MSVSpecBase[A, P, M, V]
{
  type PMS = PlottedModelSelection[A, JFree, P, M, V]

  override def performableTimeout = 2.minutes

  def stepInterval = 200.millis

  def estimationShape: Shape = Shape.Scatter

  implicit def fconf =
    FigureConf.default("mi", width = 1000, height = 1000,
      shape = estimationShape)

  def mkPms(msv: MSV)(implicit plotBE: Viz[JFree, A, P]) = 
      PlottedModelSelection[A, JFree, P, M, V](msv, stepInterval)

  def validationError(pm: ModelSelectionValidation[A, P, V]) = {
    pm.printer.short()
    pm.foldError
  }

  def error(s: PMS) =
    s.main map (_ map validationError)

  def trainPms(pms: PMS, margin: Double) =
    error(pms) computes beValid(be_<=(margin))
}

abstract class PlottedSpecBase[A: Sample, P: ParamVizData, M, V]
extends PlottedSpecHelpers[A, P, M, V]
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

abstract class PlottedIrisSpecBase[P: ParamVizData: JParam, M, V]
extends PlottedSpecBase[Iris, P, M, V]
{
  implicit override def fconf = super.fconf

  def pms = mkPms(msv)
}
