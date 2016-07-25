package tryp
package mi
package kmeans

import org.scalacheck._
import org.scalacheck.util.Buildable

import KMeans._

case class KMeansData(rank: Int, classes: Nel[ClassCluster[_]])

object KMeansGen
extends GenBase[KMeansData]
{
  import Gen._
  import GenBase._

  def genClass(num: Int, rank: Int, members: Range) = for {
    memberCount <- choose(members.min, members.max)
    mean <- genData.genSample(rank)
    covariance <- choose[Double](0.0001d, genData.varianceRange)
  } yield gaussCluster(num, rank, mean, covariance.left, memberCount)

  def kmeans(maxRank: Int, maxClasses: Int, members: Range) =
    for {
      rank <- choose(2, maxRank - 1)
      classCount <- choose(2, maxClasses - 1)
      classes <- clusters(classCount, genClass(_, rank, members))
    } yield KMeansData(rank, classes)
}

object KMeansData
extends KMeansDataInstances

trait KMeansDataInstances
{
  implicit lazy val instance_GenData_KMeansData: GenData[KMeansData] =
    new GenData[KMeansData] {
      def rank(a: KMeansData) = a.rank

      def classes(a: KMeansData) = a.classes

      override def varianceRange = 2d

      def sampleRange: Double = 10d

      def domainRange = 10d
    }

  implicit lazy val instance_MSVGen_KMeansData
  : MSVGen[KMeansData, KMeans, KMeans, Col, KMeansLearnConf] =
      new MSVGen[KMeansData, KMeans, KMeans, Col, KMeansLearnConf] {
        def margin(cd: CheckData[KMeansData]) =
          15d * cd.conf.rank * cd.genData.sampleRange

        def lconf(cd: CheckData[KMeansData], sc: MSConf)
        (implicit s: Sample[Data]) =
          KMeansLearnConf.ee(sc)

        def createMSV(implicit s: Sample[Data], mc: MC[Data]) = imp.imp

        override def sconf(cd: CheckData[KMeansData], sc: MSConf) =
        {
          val epsilon = sc.epsilon0 * cd.conf.rank * cd.genData.sampleRange
          sc.copy(epsilon = epsilon)
        }
      }
}
