package tryp
package mi

import scala.annotation.tailrec

import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.MultivariateGaussian

case class Data(feature: Col, num: Int)

case class DataClass(num: Int)
extends ModelClass[Data]
{
  def name = s"cls $num"
}

@tc trait ClusterGen[A]
{
  def gen(a: A)(count: Int): Nel[Col]
}

case class GaussianCluster(mean: Col, covariance: Double Xor Mat)
{
  lazy val rank = mean.length

  def cov = covariance valueOr (a => diag(Col.fill(rank)(a)))

  lazy val dist = MultivariateGaussian(mean, cov)
}

object GaussianCluster
{
  implicit lazy val instance_ClusterGen_GaussianCluster
  : ClusterGen[GaussianCluster] =
    new ClusterGen[GaussianCluster] {
      def gen(a: GaussianCluster)(count: Int) = count genNel a.dist.draw()
    }
}

case class RingCluster
(mean: Col, covariance: Double Xor Mat, radius: Double, epsilon: Double)
{
  lazy val gauss =
    GaussianCluster(mean, covariance bimap (_ * radius, _ * radius))
}

object RingCluster
{
  import ClusterGen.ops._

  implicit lazy val instance_ClusterGen_RingCluster
  : ClusterGen[RingCluster] =
    new ClusterGen[RingCluster] {
      def one(a: RingCluster) = {
        val x = a.gauss.dist.draw()
        val dist = abs(norm(a.mean - x) - a.radius)
        val good = dist < a.epsilon
        good -> x
      }

      @tailrec
      def collect(a: RingCluster)(data: Nel[Col], count: Int): Nel[Col] = {
        if (count == 0) data
        else {
          val (good, x) = one(a)
          val next = if (good) (x :: data) else data
          val nextCount = if (good) count - 1 else count
          collect(a)(next, nextCount)
        }
      }

      @tailrec
      def init(a: RingCluster)(count: Int): Nel[Col] = {
        val (good, x) = one(a)
        if (good) collect(a)(Nel(x), count - 1)
        else init(a)(count)
      }

      def gen(a: RingCluster)(count: Int) = {
        init(a)(count)
      }
    }
}

case class StrictCluster()

case class ClassCluster[A: ClusterGen]
(num: Int, rank: Int, mean: Col, clusterGen: A, members: Int)
{
  import ClusterGen.ops._

  def gen() = clusterGen.gen(members)
}

case class ClassData(clusters: Nel[ClassCluster[_]], data: Nel[Data])
{
  def conf = clusters.head
  def num = conf.num
  lazy val value = num.toDouble
  lazy val label = DataClass(num)
}

trait DataSample
extends Sample[Data]
{
  def data: Nel[ClassData]

  lazy val classMap = data.map(a => a.num -> a.label).unwrap.toMap

  def cls(a: Data) = classMap.get(a.num).getOrElse(LabeledClass("invalid"))

  def feature(a: Data) = a.feature
}
