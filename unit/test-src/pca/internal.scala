package tryp
package mi
package pca
package unit

import breeze.linalg._
import breeze.numerics._
import breeze.linalg.functions.euclideanDistance

case class Dat(feature: Col)

object Dat
{
  case object NoClass extends AutoClass[Dat]

  implicit def instance_ModelClasses_Dat: ModelClasses[Dat] =
    new ModelClasses[Dat] {
      def value(a: ModelClass[Dat]) =
        Validated.fromOption(NaN.some, s"no class for $a")

      lazy val classes = Nel(NoClass: ModelClass[Dat])
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

  def kernel: KernelFunc = LinearKernel

  lazy val conf = PCALearnConf.default(kernel = kernel)

  lazy val stop = StepCountStopCriterion[PCA](1)

  lazy val train = PCAEstimator(data, conf)

  lazy val model = train.go

  lazy val predict = PCAPredictor(conf)

  lazy val validator = PCAValidator[Dat](data, conf)
}

class TrivialPCASpec
extends InternalBase
{
  def is = s2"""
  Principal Component Analysis

  empirical mean $mean
  centered data $centered
  covariance matrix $covariance
  eigen data $eigen
  """

  import Dat._

  val x1 = Col(1d, 1d)

  val x2 = Col(4d, 4d)

  val x3 = Col(2d, 3d)

  val d1 = Dat(x1)

  val d2 = Dat(x2)

  lazy val data = Nel(d1, d2)

  def mean = train.xm must_== Col(2.5d, 2.5d)

  def centered = train.centered must_== Mat(Col(-1.5d, -1.5d), Col(1.5d, 1.5d))

  def covariance =
    train.covariance must_== Mat(Col(4.5d, 4.5d), Col(4.5d, 4.5d))

  lazy val s = 1d / sqrt(2d)

  def eigen =
    train.eigen must_== List(0.0 -> Col(-s, s), 9.0 -> Col(s, s))
}

class SimplePCASpec
extends InternalBase
{
  def is = s2"""
  Principal Component Analysis

  eigen data $eigen
  principal components $pcs
  """

  import Dat._
  import shapeless._
  import syntax.std.tuple._

  val theta = 45d

  val rotate = {
    val c = cos(theta)
    val s = sin(theta)
    Mat((c, -s), (s, c))
  }

  val translate = Col(2d, 2d)

  object trans
  extends Poly1
  {
    implicit def col = at[Col](a => (rotate * a) + translate)
  }

  val xo = (Col(2d, 0d), Col(-2d, 0d), Col(0d, 1d))

  val (x1, x2, x3) = xo map trans

  lazy val data = Nel(Dat(x1), Dat(x2), Dat(x3))

  def eigen = train.allEigenvalues must beClose(List(4d, 1d / 3d))

  def pcs = train.pcs must beCloseWithin(List(Col(0.525, 0.850)), 1e-2)
}
