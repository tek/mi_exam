package tryp
package mi

import breeze.linalg._
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

case class RingCluster(gauss: GaussianCluster)

object RingCluster
{
  import ClusterGen.ops._

  implicit lazy val instance_ClusterGen_RingCluster
  : ClusterGen[RingCluster] =
    new ClusterGen[RingCluster] {
      def gen(a: RingCluster)(count: Int) = a.gauss.gen(count)
    }
}

case class ClassCluster[A: ClusterGen]
(num: Int, rank: Int, clusterGen: A, members: Int)
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
