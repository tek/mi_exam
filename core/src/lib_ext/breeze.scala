package tryp
package mi

final class MatOps(val self: Mat)
extends AnyVal
{
  def dims = (self.rows, self.cols)
}

trait ToMatOps
{
  implicit def ToMatOps(m: Mat) = new MatOps(m)
}
