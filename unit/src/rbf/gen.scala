package tryp
package mi
package rbf

import org.scalacheck._
import org.scalacheck.util.Buildable

case class RBFData(rank: Int, classes: Nel[ClassConf])
extends RandomConf

object RBFGen
extends GenBase
{
  import Gen._
  import GenBase._

  def range: Double = 10d

  def genClass(num: Int, rank: Int, members: Range) = for {
    memberCount <- choose(members.min, members.max)
    mean <- genCol(rank)
    covariance <- choose[Double](0.0001d, range)
  } yield ClassConf(num, rank, mean, covariance, memberCount)

  def rbf(maxRank: Int, maxClasses: Int, members: Range) =
    for {
      rank <- choose(3, maxRank - 1)
      classCount <- choose(3, maxClasses - 1)
      classes <- nelOfN(classCount, genClass(_, rank, members))
    } yield RBFData(rank, classes)
}
