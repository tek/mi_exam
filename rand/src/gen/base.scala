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

  def genMat(rows: Int, cols: Int, range: Double) =
    for {
      d <- containerOfN[Array, Double](rows * cols, choose(-range, range))
    } yield Mat.create(rows, cols, d)

  def rotationMatrix(rank: Int) =
    for {
      mat <- genMat(rank, rank, 1d)
    } yield qr(mat).q

  def clusters = oneAndOfN[List, ClassCluster[_]] _
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
      gen = GaussianCluster(mean, variance.left)
    } yield ClassCluster(num, rank, mean, gen, memberCount)

  def gaussCluster(num: Int, rank: Int, mean: Col, cov: Double Xor Mat,
    members: Int) = {
      val gen = GaussianCluster(mean, cov)
      ClassCluster(num, rank, mean, gen, members)
  }

  def genCluster(num: Int, rank: Int, members: Range, mean: Col, cov: Mat) =
    for {
      memberCount <- choose(members.min, members.max)
    } yield gaussCluster(num, rank, mean, (cov * vari).right, memberCount)

  def ringWidth = 0.5

  def genRing(num: Int, rank: Int, members: Range, mean: Col, cov: Double,
    radius: Double) =
    for {
      memberCount <- choose(members.min, members.max)
      gen = RingCluster(mean, cov.left, radius, ringWidth)
    } yield ClassCluster(num, rank, mean, gen, memberCount)

  def pointInPlane(normal: Col, bias: Double, rank: Int) =
    for {
      l <- genSample(rank)
    } yield l * (bias / (normal dot l))
}

@tc trait GenData[A]
{
  def rank(a: A): Int

  def classes(a: A): Nel[ClassCluster[_]]

  def sampleRange: Double

  def domainRange: Double

  def genSample(rank: Int) = GenBase.genSample(rank, sampleRange)

  def createCluster(cluster: ClassCluster[_]) = {
    val data = cluster.gen().map(a => Data(a, cluster.num))
    ClassData(Nel(cluster), data)
  }

  def createClass(cls: Nel[ClassCluster[_]]) = {
    cls map createCluster reduceLeft ((a, b) =>
        ClassData(a.clusters.combine(b.clusters), a.data.combine(b.data)))
  }

  def createClasses(classes: Nel[Nel[ClassCluster[_]]]) =
    classes map createClass
}
