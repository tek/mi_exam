package tryp
package mi
package svm
package unit

import breeze.linalg._
import breeze.numerics._
import breeze.linalg.functions.euclideanDistance

case class Dat(feature: Col, cls: ModelClass[Dat])

object Dat
{
  case object One extends AutoClass[Dat]
  case object Two extends AutoClass[Dat]

  val values = Map[ModelClass[Dat], Double](
    One -> -1d,
    Two -> 1d,
  )

  implicit def instance_ModelClasses_Dat: ModelClasses[Dat] =
    new ModelClasses[Dat] {
      def value(a: ModelClass[Dat]) =
        Validated.fromOption(Dat.values.get(a), s"no class for $a")

      lazy val classes = Nel(One: ModelClass[Dat], Two)
    }

  implicit lazy val datSample: Sample[Dat] =
    new Sample[Dat] {
      def cls(a: Dat) = a.cls

      def feature(a: Dat) = a.feature

      def featureCount = 2
    }
}

trait InternalBase
extends Spec
{
  val data: Nel[Dat]

  def lambda = 0.5d

  def kernel: KernelFunc = LinearKernel

  lazy val conf = SVMLearnConf.default(lambda, kernel = kernel)

  lazy val stop = StepCountStopCriterion[SVM](1)

  lazy val train = SVMEstimator(data, conf)

  lazy val model = train.go

  lazy val predict = SVMPredictor(conf)

  lazy val validator = SVMValidator[Dat](data, conf)

  lazy val pred = train.go map (predict(data.head, _))

  lazy val b = train.offset.getOrElse(Inf)

  def pt(d: Dat) = d.valueOrNaN * (train.w dot d.feature + b)
}

class TrivialSVMSpec
extends InternalBase
{
  def is = s2"""
  Support Vector Machine

  data gram matrix ${train.gramX must_== gramX}
  label matrix $labels
  quadratic form ${train.form must_== form}
  normal vector ${train.w must beCloseCol(normal)}
  offset $offset
  support vectors in plane equation $support
  point on plane ${point must beClose(b)}
  predict training data $predictTrain
  """

  import Dat._

  val x1 = Col(1d, 1d)

  val x2 = Col(3d, 1d)

  val d1 = Dat(x1, One)

  val d2 = Dat(x2, Two)

  lazy val data = Nel(d1, d2)

  def gramX = Mat((2d, 4d), (4d, 10d))

  def labels = train.spanY must_== Mat((1d, -1d), (-1d, 1d))

  def form = Mat((2d, -4d), (-4d, 10d))

  def normal = Col(1d, 0d)

  def supportValue = 1d

  def support =
    train.supports.length must_== 2 and (
      train.supports.map(pt).toList must contain(beClose(supportValue)).forall)

  def offset = train.offset must beValid(beClose(2))

  def point = train.eval(Col(2d, 1d))

  def predictTrain =
    model.map(m => predict(d1, m).cls -> predict(d2, m).cls) must
      beValid(One -> Two)
}

class KernelSVMSpec
extends TrivialSVMSpec
{
  val coeff = 2d

  object TestKernel
  extends KernelFunc
  {
    def apply(v1: Col, v2: Col): Double = (v1 dot v2) * coeff
  }

  override def kernel = TestKernel

  override def gramX = super.gramX * coeff

  override def form = super.form * coeff

  override def normal = super.normal / coeff

  override def supportValue = super.supportValue / coeff

  override def point = super.point * 2
}

class SimpleSVMSpec
extends InternalBase
{
  def is = s2"""
  Support Vector Machine

  normal vector $normal
  data points in the plane equation $dataPoints
  """

  import Dat._

  override def lambda = 1e-2

  lazy val data = Nel(
    Dat(Col(0d, 1d), One),
    Dat(Col(1d, 1d), One),
    Dat(Col(1d, 2d), One),
    Dat(Col(3d, 1d), Two),
    Dat(Col(3d, 3d), Two),
    Dat(Col(4d, 3d), Two),
    Dat(Col(10d, 3d), Two),
  )

  def normal = normalize(train.w) must beCloseToCol(Col(1d, 0d), 1e-2)

  def dataPoints = data.map(pt).toList must contain(be_>=(0d)).forall
}

class ComplexSVMSpec
extends InternalBase
{
  import Dat._

  import viz._

  import PlotBackend.ops._

  def is = s2"""
  Support Vector Machine

  plot $plot
  data points in the plane equation $dataPoints
  """

  implicit lazy val datSamplePlotting =
    new SamplePlotting[Dat] {
      lazy val range = (-9d, 9d)

      lazy val ranges = List(range, range)

      lazy val plotCount = 1

      lazy val projections =
        List((0, 1))
    }

  lazy val data =
    Nel(
      Dat(Col(4.682161942599795, -4.24450562186186), Two),
      Dat(Col(-0.6117916465937832, -0.7024392055677455), One),
      Dat(Col(3.08649538396474, -8.557304235680366), Two),
      Dat(Col(-0.01515121521553045, -0.20326750998426046), One),
      Dat(Col(2.217469993018402, 1.1534543725023876), One),
      Dat(Col(-0.9167868968577179, 0.501287189226614), One),
    )

  override def lambda = .1d

  def plot = {
    implicit val fconf =
     FigureConf.default("mi", width = 1000, height = 1000, shape = Shape.Line)
    val plot = PlotBackend[JFree[Dat]]
    val svm = model.toOption.get
    // val j = plot.init
    // val t = for {
    //   _ <- j.setup
    //   _ <- j.fold(data.unwrap)
    //   _ <- j.step(svm)
    // } yield ()
    // t.unsafeRun
    // Thread.sleep(3000)
    1 === 1
  }

  def dataPoints = data.map(pt).toList must contain(be_>=(0d)).forall
}
