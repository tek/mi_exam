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

case class SVMData(rank: Int, plane: Plane, one: Nel[ClassCluster],
  two: Nel[ClassCluster])
extends RandomConf
{
  def classes = one combine two
}

object SVMGen
extends GenBase[SVMData]
{
  import Gen._

  import genData._

  def genCluster(num: Int, rank: Int, members: Range, mean: Col) =
    for {
      memberCount <- choose(members.min, members.max)
      covariance <- choose[Double](0.0001d, sampleRange / 2d)
    } yield ClassCluster(num, rank, mean, covariance, memberCount)

  def genLinearClass(num: Int, rank: Int, members: Range, pivot: Col,
    direction: Col) =
      for {
        dist <- choose(3d, 5d)
        mean = pivot + (dist * direction)
        cluster <- genCluster(num, rank, members, mean)
      } yield cluster

  def pointInPlane(normal: Col, bias: Double, rank: Int) =
    for {
      l <- genSample(rank)
    } yield l * (bias / (normal dot l))

  def genPlane(rank: Int) = for {
    w <- genSample(rank)
    normal = normalize(w)
    bias <- choose[Double](0d, sampleRange)
    pivot <- pointInPlane(normal, bias, rank)
  } yield Plane(normal, bias, pivot)

  def linearSvm(maxFeatures: Int, members: Range) =
    for {
      rank <- choose(2, maxFeatures)
      plane <- genPlane(rank)
      one <- genLinearClass(1, rank, members, plane.pivot, plane.normal)
      two <- genLinearClass(-1, rank, members, plane.pivot, -plane.normal)
    } yield SVMData(rank, plane, Nel(one), Nel(two))

  def threeClusterPolySvm(maxFeatures: Int, members: Range) =
    for {
      rank <- choose(2, maxFeatures)
      plane <- genPlane(rank)
      offset = plane.pivot * sampleRange
      oneA <- genCluster(1, rank, members, plane.pivot - offset)
      oneB <- genCluster(1, rank, members, plane.pivot + offset)
      two <- genCluster(-1, rank, members, plane.pivot)
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
