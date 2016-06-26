package tryp
package mi

import breeze.linalg._
import breeze.stats.distributions.MultivariateGaussian

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
}
