package tryp
package mi

import org.scalacheck._
import org.scalacheck.util.Buildable

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

  def createClass(conf: ClassCluster) = {
    val data = conf.members.genNel(Data(conf.dist.draw(), conf.num))
    ClassData(Nel(conf), data)
  }
}

abstract class GenBase[A: GenData]
{
  lazy val genData = GenData[A]

  def genSample(rank: Int) = GenBase.genSample(rank, genData.sampleRange)
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
