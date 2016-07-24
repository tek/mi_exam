package tryp
package mi
package rbf

import org.scalacheck._
import org.scalacheck.util.Buildable

case class RBFData(rank: Int, classes: Nel[ClassCluster[_]])

object RBFGen
extends GenBase[RBFData]
{
  import Gen._
  import GenBase._

  def genClass(num: Int, rank: Int, members: Range) = for {
    memberCount <- choose(members.min, members.max)
    mean <- genData.genSample(rank)
    covariance <- choose[Double](0.0001d, genData.domainRange)
  } yield gaussCluster(num, rank, mean, covariance.left, memberCount)

  def rbf(maxRank: Int, maxClasses: Int, members: Range) =
    for {
      rank <- choose(3, maxRank - 1)
      classCount <- choose(3, maxClasses - 1)
      classes <- clusters(classCount, genClass(_, rank, members))
    } yield RBFData(rank, classes)
}

object RBFData
extends RBFDataInstances

trait RBFDataInstances
{
  implicit lazy val instance_GenData_RBFData: GenData[RBFData] =
    new GenData[RBFData] {
      def rank(a: RBFData) = a.rank

      def classes(a: RBFData) = a.classes

      def sampleRange: Double = 10d

      def domainRange = 10d
    }

  implicit lazy val instance_MSVGen_RBFData
  : MSVGen[RBFData, RBFs[GaussBF], RBFs[GaussBF], Double] =
      new MSVGen[RBFData, RBFs[GaussBF], RBFs[GaussBF], Double] {
        def margin(cd: CheckData[RBFData]) =
          1e-5d

        def msv(cd: CheckData[RBFData])
        (sconf: ModelSelectionConf)
        (implicit mc: ModelClasses[Data, Double], s: Sample[Data])
        = {
          val lconf =
            RBFLearnConf.default[GaussBF, Data](
              rbfs = cd.classes.length, eta = 1d)
          RBF.msv(cd.data.shuffle, lconf, sconf)
        }
      }
}
