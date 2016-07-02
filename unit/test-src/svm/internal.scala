package tryp
package mi
package svm

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

  implicit val datSample: Sample[Dat] =
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

  val conf: SVMLearnConf

  lazy val stop = StepCountStopCriterion[SVM](1)

  lazy val train = SVMEstimator(data, conf)

  lazy val predict = SVMPredictor(conf)

  lazy val validator = SVMValidator[Dat](data, conf)

  // lazy val pred = predict(data.head, ???)

  lazy val b = train.offset.getOrElse(Inf)

  def pt(d: Dat) = d.valueOrNaN * (train.w dot d.feature + b)
}

class TrivialSVMSpec
extends InternalBase
{
  def is = s2"""
  Support Vector Machine

  data dot products $dataDot
  label matrix $labels
  gram matrix $gram
  normal vector $normal
  offset $offset
  support vectors in plane equation $support
  point on plane $point
  """

  import Dat._

  def lambda = 0.5d

  val x1 = Col(1d, 1d)

  val x2 = Col(3d, 1d)

  lazy val data = Nel(Dat(x1, One), Dat(x2, Two))

  lazy val features = data map (_.feature)

  lazy val conf = SVMLearnConf.default(lambda)

  def dataDot = train.xdot must_== Mat((2d, 4d), (4d, 10d))

  def labels = train.yg must_== Mat((1d, -1d), (-1d, 1d))

  def gram = train.gram must_== Mat((2d, -4d), (-4d, 10d))

  def normal = train.w must beCloseCol(Col(1d, 0d))

  def support =
    train.supports.length must_== 2 and (
      train.supports.map(pt).toList must contain(beClose(1)).forall)

  def offset = train.offset must beValid(beClose(-2))

  def point = train.w dot Col(2d, 1d) must beClose(-b)
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

  def lambda = 1e-2

  lazy val data = Nel(
    Dat(Col(0d, 1d), One),
    Dat(Col(1d, 1d), One),
    Dat(Col(1d, 2d), One),
    Dat(Col(3d, 1d), Two),
    Dat(Col(3d, 3d), Two),
    Dat(Col(4d, 3d), Two),
    Dat(Col(10d, 3d), Two),
  )

  lazy val features = data map (_.feature)

  lazy val conf = SVMLearnConf.default(lambda)

  def normal = normalize(train.w) must beCloseToCol(Col(1d, 0d), 1e-2)

  def dataPoints = data.map(pt).toList must contain(be_>=(0d)).forall
}
