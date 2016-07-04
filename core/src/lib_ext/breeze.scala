package tryp
package mi

import breeze.plot.DomainFunction
import cats.Foldable

final class MatOps(val self: Mat)
extends AnyVal
{
  def dims = (self.rows, self.cols)
}

trait ToMatOps
{
  implicit def ToMatOps(m: Mat) = new MatOps(m)
}

trait BreezeInstances
{
  implicit def instance_DomainFunction_Vector[A]
  : DomainFunction[Vector[A], Int, A] = {
    new DomainFunction[Vector[A], Int, A] {
      def domain(t: Vector[A]): IndexedSeq[Int] = 0 until t.length

      def apply(t: Vector[A], k: Int): A = t(k)
    }
  }
}

final class BreezeFoldableOps[F[_]: Foldable, A: ClassTag](val self: F[A])
{
  def toCol = Col(Foldable[F].toList(self): _*)
}

trait ToBreezeFoldableOps
{
  implicit def ToBreezeFoldableOps[F[_]: Foldable, A: ClassTag](x: F[A])
  : BreezeFoldableOps[F, A] =
    new BreezeFoldableOps(x)
}

final class BreezeSeqOps[A: ClassTag](val self: Seq[A])
{
  def toCol = Col(self: _*)
}

trait ToBreezeSeqOps
{
  implicit def ToBreezeSeqOps[A: ClassTag](x: Seq[A]): BreezeSeqOps[A] =
    new BreezeSeqOps(x)
}
