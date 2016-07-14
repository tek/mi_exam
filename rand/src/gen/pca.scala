package tryp
package mi
package pca

import breeze.numerics._
import breeze.linalg._
import functions.euclideanDistance

import org.scalacheck.Gen

case class Plane(normal: Col, bias: Double, pivot: Col)

case class PCAData(rank: Int, classes: Nel[ClassCluster[_]])
extends RandomConf

object PCAGen
extends GenBase[PCAData]
{
  import Gen._
  import GenBase._

  import genData._

  def ringLimit = 10d

  def rings(maxRank: Int, maxClasses: Int, members: Range) =
    for {
      rank <- choose(2, maxRank)
      count <- choose(2, maxClasses)
      variance <- choose[Double](0.0001d, vari)
      clust <- clusters(count, a => genRing(a, rank, members, Col.zeros(rank),
        variance, ringLimit * ((a + 1d) / count)))
    } yield PCAData(rank, clust)

  def pca(maxRank: Int, members: Range) =
    for {
      rank <- choose(2, maxRank)
      rot <- rotationMatrix(rank)
      trans <- genSample(rank)
      variance <- GenBase.genPosSample(rank, 1d)
      cov = diag(variance)
      cluster <- genCluster(0, rank, members, Col.zeros(rank), cov)
    } yield PCAData(rank, Nel(cluster))
}

object PCAData
extends PCADataInstances
{
  lazy val genData = GenData[PCAData]
}

trait PCADataInstances
{
  implicit lazy val instance_GenData_PCAData: GenData[PCAData] =
    new GenData[PCAData] {
      def sampleRange: Double = 10d

      def domainRange = 10d
    }
}
