package tryp
package mi
package svm

import breeze.numerics._
import breeze.linalg.norm
import breeze.linalg.functions.euclideanDistance

import org.specs2.ScalaCheck
import org.specs2.scalacheck._
import org.scalacheck._
import org.scalacheck.util.Buildable
import Prop._
import Arbitrary.arbitrary

case class Plane(normal: Col, bias: Double)

case class SVMData(plane: Plane, one: ClassConf, two: ClassConf)
{
  def classes = Nel(one, two)
}

object SVMGen
{
  import Gen._

  def range: Double = 10d

  def oneAndOfN[F[_], A](count: Int, gen: Int => Gen[A])
  (implicit b: Buildable[A, F[A]]) = for {
    head <- gen(0)
    tail <- Gen.sequence[F[A], A](1 to count map gen)
  } yield cats.data.OneAnd[F, A](head, tail)

  def nelOfN[A] = oneAndOfN[List, A] _

  def genCol(features: Int) = for {
    d <- containerOfN[Array, Double](features, choose(-range, range))
  } yield Col(d)

  def createClass(conf: ClassConf) = {
      val data = conf.members.genNel(Data(conf.dist.draw(), conf.num))
      DataClass(conf, data)
    }

  def genClass(num: Int, features: Int, members: Range, direction: Col) =
    for {
      memberCount <- choose(members.min, members.max)
      dist <- choose(0d, 3d)
      mean = dist * direction
      covariance <- choose[Double](0.0001d, range)
    } yield ClassConf(num, features, mean, covariance, memberCount)

  def genPlane(features: Int) = for {
    normal <- genCol(features)
    bias <- choose[Double](0d, range)
  } yield Plane(normal / norm(normal), bias)

  def svm(maxFeatures: Int, maxClasses: Int, members: Range) =
    for {
      features <- choose(3, maxFeatures)
      plane <- genPlane(features)
      one <- genClass(0, features, members, plane.normal)
      two <- genClass(0, features, members, -plane.normal)
    } yield SVMData(plane, one, two)
}
