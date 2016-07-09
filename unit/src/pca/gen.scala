package tryp
package mi
package pca

import breeze.numerics._
import breeze.linalg._
import functions.euclideanDistance

import org.specs2.ScalaCheck
import org.specs2.scalacheck._
import org.scalacheck._
import org.scalacheck.util.Buildable
import Prop._
import Arbitrary.arbitrary

case class Plane(normal: Col, bias: Double, pivot: Col)

case class PCAData(rank: Int, cluster: ClassCluster)
extends RandomConf
{
  def classes = Nel(cluster)
}

object PCAGen
extends GenBase[PCAData]
{
  import Gen._

  import genData._

  def genPlane(rank: Int) = for {
    w <- genSample(rank)
    normal = normalize(w)
    bias <- choose[Double](0d, sampleRange)
    pivot <- pointInPlane(normal, bias, rank)
  } yield Plane(normal, bias, pivot)

  def pca(maxFeatures: Int, members: Range) =
    for {
      rank <- choose(2, maxFeatures)
      cluster <- genCluster(0, rank, members, Col(0d))
    } yield PCAData(rank, cluster)
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
