package tryp
package mi

trait MSVSpecBase[S, P, M, V]
extends tryp.SpecCommon
with mi.Matchers
{
  type MSV0 = MSV[S, P]

  def msvError(msv: MSV0) = {
    msv.validation.map { _.map { v =>
      v.printer.short()
      v.foldError
    } }
  }

  def train(msv: MSV0, margin: Double) =
    msvError(msv) computes beValid(be_<=(margin))

  def trials: Option[Int] = None

  def epsilon0 = 1e-2d

  def folds = 10

  def steps = 10000

  implicit val sconf = MSConf.default(
    steps = steps,
    epsilon0 = epsilon0,
    trials = trials,
    folds = folds,
  )
}

abstract class MSVSpec[S: Sample, P, M, V, C]
(implicit cm: CreateMSV[S, P, M, C])
extends Spec
with MSVSpecBase[S, P, M, V]
{
  def is = s2"""
  $title

  learn a model with cross-validation ${train(msv, margin)}
  """

  def title: String

  implicit def conf: C

  def data: Nel[S]

  lazy val msv: MSV0 = MSV.create(data)

  def margin = foldMargin * (trials | sconf.folds)

  val foldMargin = 1e-1d
}

abstract class SimpleMSVSpec[S: Sample, P, C]
(implicit cm: CreateMSV[S, P, P, C])
extends MSVSpec[S, P, P, Double, C]
