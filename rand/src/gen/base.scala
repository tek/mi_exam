package tryp
package mi

import org.scalacheck._
import org.scalacheck.util.Buildable

import breeze.linalg._

object GenBase
{
  import Gen._

  def oneAndOfN[F[_], A](count: Int, gen: Int => Gen[A])
  (implicit b: Buildable[A, F[A]]) = for {
    head <- gen(0)
    tail <- Gen.sequence[F[A], A](1 to count map gen)
  } yield cats.data.OneAnd[F, A](head, tail)

  def nelOfN[A] = oneAndOfN[List, A] _

  def genSample(rank: Int, range: Double) = for {
    d <- containerOfN[Array, Double](rank, choose(-range, range))
  } yield Col(d)

  def genPosSample(rank: Int, range: Double) = for {
    d <- containerOfN[Array, Double](rank, choose(0.0001d, range))
  } yield Col(d)

  def createClass(conf: ClassCluster) = {
    val data = conf.members.genNel(Data(conf.dist.draw(), conf.num))
    ClassData(Nel(conf), data)
  }

  def genMat(rows: Int, cols: Int, range: Double) =
    for {
      d <- containerOfN[Array, Double](rows * cols, choose(-range, range))
    } yield Mat.create(rows, cols, d)

  def rotationMatrix(rank: Int) =
    for {
      mat <- genMat(rank, rank, 1d)
    } yield qr(mat).q
}

abstract class GenBase[A: GenData]
{
  import Gen._

  lazy val genData = GenData[A]

  def genSample(rank: Int) = GenBase.genSample(rank, genData.sampleRange)

  def vari = genData.sampleRange / 2d

  def genUniCluster(num: Int, rank: Int, members: Range, mean: Col) =
    for {
      memberCount <- choose(members.min, members.max)
      variance <- choose[Double](0.0001d, vari)
    } yield ClassCluster(num, rank, mean, variance.left, memberCount)

  def genCluster(num: Int, rank: Int, members: Range, mean: Col, cov: Mat) =
    for {
      memberCount <- choose(members.min, members.max)
    } yield ClassCluster(num, rank, mean,
      (cov * vari).right, memberCount)

  def pointInPlane(normal: Col, bias: Double, rank: Int) =
    for {
      l <- genSample(rank)
    } yield l * (bias / (normal dot l))
}

trait RandomConf
{
  def rank: Int
  def classes: Nel[ClassCluster]
}

@tc trait GenData[A]
{
  def sampleRange: Double

  def domainRange: Double

  def genSample(rank: Int) = GenBase.genSample(rank, sampleRange)
}
