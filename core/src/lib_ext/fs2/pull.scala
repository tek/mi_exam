package tryp
package mi

import fs2._
import fs2.util._

trait PullFunctions
{
  private type Trans[F[_], A, B] = Handle[F, A] => Pull[F, B, Handle[F, B]]

  // def recFoldEmit[F[_], A, B, C >: A, D](z: C)(f: (C, A) => C)(g: C => D)
  // : Trans[F, A, D Xor A] = {
  //   Pull.receive1Option {
  //     case Some(a #: h) =>
  //       Pull.output1(a.right[D]) >> recFoldEmit(f(z, a))(f)(g)(h)
  //     case None =>
  //       Pull.output1(g(z).left[A]) >> Pull.done
  //   }
  // }
}
