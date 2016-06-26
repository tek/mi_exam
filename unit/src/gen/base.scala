// package tryp
// package mi

// import org.scalacheck._
// import org.scalacheck.util.Buildable

// object GenBase
// {
//   import Gen._

//   def oneAndOfN[F[_], A](count: Int, gen: Int => Gen[A])
//   (implicit b: Buildable[A, F[A]]) = for {
//     head <- gen(0)
//     tail <- Gen.sequence[F[A], A](1 to count map gen)
//   } yield cats.data.OneAnd[F, A](head, tail)

//   def nelOfN[A] = oneAndOfN[List, A] _

//   def genCol(rank: Int, range: Double) = for {
//     d <- containerOfN[Array, Double](rank, choose(-range, p(range)))
//   } yield Col(d)

//   def createClass(conf: ClassConf) = {
//     val data = conf.members.genNel(Data(conf.dist.draw(), conf.num))
//     ClassData(conf, data)
//   }
// }

// trait GenBase
// {
//   def range: Double

//   def genCol(rank: Int) = GenBase.genCol(rank, range)
// }

// trait RandomConf
// {
//   def classes: Nel[ClassConf]
// }