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

case class SVMData(rank: Int, plane: Plane, one: ClassConf, two: ClassConf)
extends RandomConf
{
  def classes = Nel(one, two)
}

object SVMGen
extends GenBase[SVMData]
{
  import Gen._

  import genData._

  def genClass(num: Int, rank: Int, members: Range, pivot: Col,
    direction: Col) =
    for {
      memberCount <- choose(members.min, members.max)
      dist <- choose(2d, 3d)
      mean = pivot + (dist * direction)
      covariance <- choose[Double](0.0001d, sampleRange / 2d)
    } yield ClassConf(num, rank, mean, covariance, memberCount)

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

  def svm(maxFeatures: Int, members: Range) =
    for {
      rank <- choose(3, maxFeatures)
      plane <- genPlane(rank)
      one <- genClass(1, rank, members, plane.pivot, plane.normal)
      two <- genClass(-1, rank, members, plane.pivot, -plane.normal)
    } yield SVMData(rank, plane, one, two)
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
