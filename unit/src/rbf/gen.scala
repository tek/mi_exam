package tryp
package mi
package rbf

import org.scalacheck._
import org.scalacheck.util.Buildable

object RBFGen
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

  def genClass(num: Int, features: Int, members: Range) = for {
    memberCount <- choose(members.min, members.max)
    mean <- genCol(features)
    covariance <- choose[Double](0.0001d, range)
  } yield ClassConf(num, features, mean, covariance, memberCount)

  def rbf(maxFeatures: Int, maxClasses: Int, members: Range) =
    for {
      features <- choose(3, maxFeatures)
      classCount <- choose(3, maxClasses)
      classes <- nelOfN(classCount, genClass(_, features, members))
    } yield classes
}
