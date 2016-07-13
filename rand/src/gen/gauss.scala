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

case class ClassCluster(num: Int, rank: Int, mean: Col,
  covariance: Double Xor Mat, members: Int)
  {
    def cov = covariance valueOr (a => diag(Col.fill(rank)(a)))

    lazy val dist = MultivariateGaussian(mean, cov)
  }

case class ClassData(clusters: Nel[ClassCluster], data: Nel[Data])
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