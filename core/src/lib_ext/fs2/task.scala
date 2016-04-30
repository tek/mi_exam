package tryp

import cats._

import fs2.util.Task

trait TaskInstances
{
  implicit def instance_Monad_Task: Monad[Task] =
    new Monad[Task] {
      def pure[A](a: A): Task[A] = Task.now(a)

      def flatMap[A, B](a: Task[A])(f: A => Task[B]): Task[B] =
        a flatMap f
    }
}
