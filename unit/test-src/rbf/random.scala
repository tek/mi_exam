package tryp
package mi
package rbf

import breeze.linalg._
import breeze.numerics._
import breeze.linalg.functions.euclideanDistance
import breeze.stats.distributions.MultivariateGaussian

import org.specs2.ScalaCheck
import org.specs2.scalacheck._
import org.scalacheck._
import org.scalacheck.util.Buildable
import Prop._
import Arbitrary.arbitrary

case class Data(feature: Col, num: Int)

case class ClassConf(num: Int, features: Int, mean: Col, covariance: Double,
  members: Int)
  {
    lazy val dist =
      MultivariateGaussian(mean, diag(Col.fill(features)(covariance)))
  }

case class DataClass(conf: ClassConf, data: Nel[Data])
{
  def num = conf.num
  lazy val value = num.toDouble
  lazy val label = LabeledClass(s"cls $num")
}

trait DataSample
extends Sample[Data]
{
  def data: List[DataClass]

  lazy val nums = data.map(_.num)

  def classes = data.map(c => c.value -> c.label).toMap

  def cls(a: Data) = LabeledClass(s"cls ${a.num}")

  def value(a: Data) = a.num.toDouble

  def feature(a: Data) = a.feature

  override def range = 2 * RBFGen.range
}

object RBFGen
{
  import Gen._

  def range: Double = 10d

  def oneAndOfN[F[_], A](count: Int, gen: Int => Gen[A])
  (implicit b: Buildable[A, F[A]]) = for {
    head <- gen(0)
    tail <- Gen.sequence[F[A], A](1 to count map gen)
  } yield cats.data.OneAnd[F, A](head, tail)

  def nelOfN[A] = oneAndOfN[List, A] _

  def genCol(features: Int) = for {
    d <- containerOfN[Array, Double](features, choose(-range, range))
  } yield Col(d)

  def createClass(conf: ClassConf) = {
      val data = conf.members.genNel(Data(conf.dist.draw(), conf.num))
      DataClass(conf, data)
    }

  def genClass(num: Int, features: Int, members: Range) = for {
    memberCount <- choose(members.min, members.max)
    mean <- genCol(features)
    covariance <- choose[Double](0.0001d, range)
  } yield ClassConf(num, features, mean, covariance, memberCount)

  def rbf(maxFeatures: Int, maxClasses: Int, members: Range) =
    for {
      features <- choose(3, maxFeatures)
      classCount <- choose(3, maxClasses)
      classes <- nelOfN(classCount, genClass(_, features, members))
    } yield classes
}

class RandomSpec
extends Spec
with ScalaCheck
{
  implicit lazy val params = Parameters(minTestsOk = 1)

  def is = s2"""
  randomly generated gaussian clusters ${forAllNoShrink(dataGen)(check)}
  """

  import RBFGen._

  val folds = 10

  lazy val dataGen: Gen[Nel[ClassConf]] =
    rbf(5, 5, Range(folds * 5, folds * 10))

  def mkSample(d: Nel[DataClass]) =
    new DataSample {
      def data = d.unwrap
      def featureCount = d.head.conf.features
    }

  val trials = Some(1)

  val sconf = ModelSelectionConf.default(folds = folds, trials = trials,
    epsilon = 1e-15d)

  def check(conf: Nel[ClassConf]) = {
    val classes = conf.map(createClass)
    val data = classes.flatMap(_.data)
    implicit val sample: Sample[Data] = mkSample(classes)
    val lconf = 
      RBFLearnConf.default[GaussBF, Data](rbfs = classes.length, eta = 0.5d)
    val result = RBF.msv(data.shuffle, lconf, sconf)
    result.printer.short()
    val margin = 1e-5d * (trials | data.length)
    result.unsafeValidation.totalError must be_<=(margin)
  }
}
