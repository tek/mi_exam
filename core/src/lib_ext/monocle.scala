package monocle
package function

import tryp._

import cats.data._
import cats.instances.vector._

trait NonEmptyVectorIndexLens
{
  implicit def nevIndex[A]: Index[NonEmptyVector[A], Int, A] =
    new Index[NonEmptyVector[A], Int, A] {
      def index(i: Int) =
        Optional[NonEmptyVector[A], A](_.get(i))(a => v =>
            v.updated(i, a) | v)
    }
}
