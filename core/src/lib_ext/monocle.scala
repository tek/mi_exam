package monocle
package function

import tryp._

import cats.data._
import cats.std.vector._

trait NonEmptyVectorIndexLens
{
  implicit def nevIndex[A]: Index[NonEmptyVector[A], Int, A] =
    new Index[NonEmptyVector[A], Int, A] {
      def index(i: Int) =
        Optional[NonEmptyVector[A], A](_.lift(i))(a => v =>
            if(v.isDefinedAt(i)) v.updated(i, a) else v)
    }
}
