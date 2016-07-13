package tryp
package mi
package svm

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

case class SVMData(rank: Int, plane: Plane, one: Nel[ClassCluster[_]],
  two: Nel[ClassCluster[_]])
extends RandomConf
{
  def classes = one combine two
}

object SVMGen
extends GenBase[SVMData]
{
  import Gen._

  import genData._

  def genLinearClass(num: Int, rank: Int, members: Range, pivot: Col,
    direction: Col) =
      for {
        dist <- choose(3d, 5d)
        mean = pivot + (dist * direction)
        cluster <- genUniCluster(num, rank, members, mean)
      } yield cluster

  def genPlane(rank: Int) = for {
    w <- genSample(rank)
    normal = normalize(w)
    bias <- choose[Double](0d, sampleRange)
    pivot <- pointInPlane(normal, bias, rank)
  } yield Plane(normal, bias, pivot)

  def linearSvm(maxRank: Int, members: Range) =
    for {
      rank <- choose(2, maxRank)
      plane <- genPlane(rank)
      one <- genLinearClass(1, rank, members, plane.pivot, plane.normal)
      two <- genLinearClass(-1, rank, members, plane.pivot, -plane.normal)
    } yield SVMData(rank, plane, Nel(one), Nel(two))

  def threeClusterPolySvm(maxRank: Int, members: Range) =
    for {
      rank <- choose(2, maxRank)
      plane <- genPlane(rank)
      offset = plane.pivot * sampleRange
      oneA <- genUniCluster(1, rank, members, plane.pivot - offset)
      oneB <- genUniCluster(1, rank, members, plane.pivot + offset)
      two <- genUniCluster(-1, rank, members, plane.pivot)
    } yield SVMData(rank, plane, Nel(oneA, oneB), Nel(two))
}

object SVMData
extends SVMDataInstances
{
  lazy val genData = GenData[SVMData]
}

trait SVMDataInstances
{
  implicit lazy val instance_GenData_SVMData: GenData[SVMData] =
    new GenData[SVMData] {
      def sampleRange: Double = 10d

      def domainRange = 10d
    }
}
