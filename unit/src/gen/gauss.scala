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

case class ClassConf(num: Int, features: Int, mean: Col, covariance: Double,
  members: Int)
  {
    lazy val dist =
      MultivariateGaussian(mean, diag(Col.fill(features)(covariance)))
  }

case class ClassData(conf: ClassConf, data: Nel[Data])
{
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
