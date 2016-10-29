package tryp

import cats._

import fs2.Stream

trait StreamInstances
{
  implicit def instance_MonadCombine_Stream[F[_]]: MonadCombine[Stream[F, ?]] =
    new MonadCombine[Stream[F, ?]] {
      def empty[A] = Stream.empty[F, A]

      def pure[A](a: A): Stream[F, A] = Stream.emit(a)

      def flatMap[A, B](a: Stream[F, A])
      (f: A => Stream[F, B]): Stream[F, B] =
        a flatMap f

      def combineK[A](fa: Stream[F, A], fb: Stream[F, A]) = fa ++ fb

      def tailRecM[A, B](a: A)(f: A => Stream[F, Either[A, B]])
      : Stream[F, B] =
        defaultTailRecM(a)(f)
    }
}

final class XorStreamOps[F[_], A, B](val self: Stream[F, A Xor B])
extends AnyVal
{
  def stripW = self collect {
    case cats.data.Xor.Right(a) => a
  }
}

trait ToXorStreamOps
{
  implicit def ToXorStreamOps[F[_], A, B](v: Stream[F, A Xor B])
  : XorStreamOps[F, A, B] =
    new XorStreamOps(v)
}

final class ListStreamOps[F[_], A](val self: List[Stream[F, A]])
extends AnyVal
{
  def flatSequence =
    self.foldLeft(Stream.empty[F, A])(_ ++ _)
}

trait ToListStreamOps
{
  implicit def ToListStreamOps[F[_], A](x: List[Stream[F, A]])
  : ListStreamOps[F, A] =
    new ListStreamOps(x)
}
