package tryp
package mi

import breeze.plot.DomainFunction

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
