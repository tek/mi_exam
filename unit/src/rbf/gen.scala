package tryp
package mi
package rbf

import org.scalacheck._
import org.scalacheck.util.Buildable

case class RBFData(rank: Int, classes: Nel[ClassCluster])
extends RandomConf

object RBFData
extends RBFDataInstances
{
  import Gen._
  import GenBase.nelOfN

  lazy val genData = GenData[RBFData]

  def genClass(num: Int, rank: Int, members: Range) = for {
    memberCount <- choose(members.min, members.max)
    mean <- genData.genSample(rank)
    covariance <- choose[Double](0.0001d, genData.domainRange)
  } yield ClassCluster(num, rank, mean, covariance, memberCount)

  def rbf(maxRank: Int, maxClasses: Int, members: Range) =
    for {
      rank <- choose(3, maxRank - 1)
      classCount <- choose(3, maxClasses - 1)
      classes <- nelOfN(classCount, genClass(_, rank, members))
    } yield RBFData(rank, classes)
}

trait RBFDataInstances
{
  implicit lazy val instance_GenData_RBFData: GenData[RBFData] =
    new GenData[RBFData] {
      def sampleRange: Double = 10d

      def domainRange = 10d
    }
}
