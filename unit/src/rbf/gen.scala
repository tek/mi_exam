package tryp
package mi
package rbf

import org.scalacheck._
import org.scalacheck.util.Buildable

case class RBFData(classes: Nel[ClassConf])
extends RandomConf

object RBFGen
extends GenBase
{
  import Gen._
  import GenBase._

  def range: Double = 10d

  def genClass(num: Int, features: Int, members: Range) = for {
    memberCount <- choose(members.min, members.max)
    mean <- genCol(features)
    covariance <- choose[Double](0.0001d, range)
  } yield ClassConf(num, features, mean, covariance, memberCount)

  def rbf(maxFeatures: Int, maxClasses: Int, members: Range) =
    for {
      features <- choose(3, maxFeatures - 1)
      classCount <- choose(3, maxClasses - 1)
      classes <- nelOfN(classCount, genClass(_, features, members))
    } yield RBFData(classes)
}
