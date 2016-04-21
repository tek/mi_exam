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
import Prop._
import Arbitrary.arbitrary

case class Data(feature: Col, num: Int)

case class DataClass(num: Int, mean: Col, covariance: Double, data: Seq[Data])

trait DataSample
extends Sample[Data]
{
  def data: List[DataClass]

  lazy val offset = data.map(_.num).min

  lazy val nums = data.map(_.num % offset)

  def classes = nums.map(n => n.toDouble -> LabeledClass(s"cls $n")).toMap

  def cls(a: Data) = LabeledClass(s"cls ${a.num % offset}")

  def value(a: Data) = (a.num % offset).toDouble

  def feature(a: Data) = a.feature

  override def range = data.map(_.mean).map(max(_)).max
}

object RBFGen
{
  lazy val classNum = Iterator.range(1, Int.MaxValue)

  def col(features: Int) = for {
    d ← Gen.containerOfN[Array, Double](features, arbitrary[Double])
  } yield Col(d)

  def cls(features: Int, maxCount: Int) = for {
    count <- Gen.choose(3, maxCount)
    mean <- col(features)
    covariance <- Gen.choose[Double](0.0001d, 50d)
    dist = MultivariateGaussian(mean, diag(Col.fill(features)(covariance)))
  } yield {
    val num = classNum.next
    DataClass(num, mean, covariance, count.gen(Data(dist.draw(), num)))
  }

  def rbf(maxFeatures: Int, maxClasses: Int, maxMembers: Int) = for {
    features <- Gen.choose(3, maxFeatures)
    classes <- Gen.choose(3, maxClasses)
    c <- Gen.containerOfN[List, DataClass](classes, cls(features, maxMembers))
  } yield c
}

class RandomSpec
extends Spec
with ScalaCheck
{
  implicit lazy val params = Parameters(minTestsOk = 1)

  def is = s2"""
  randomly generated gaussian clusters $clusters
  """

  import RBFGen._

  lazy val dataGen: Gen[List[DataClass]] = rbf(5, 5, 5)

  def mkSample(d: List[DataClass]) = new DataSample { def data = d }

  def clusters = {
    forAll(dataGen) { (data: List[DataClass]) =>
      implicit val sample: Sample[Data] = mkSample(data)
      p(data)
      data.size > 0
    }
  }
}
