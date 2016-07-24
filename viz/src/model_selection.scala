package tryp
package mi
package viz

import java.util.concurrent.Executors

import fs2._
import fs2.util._
import fs2.async.mutable.Queue
import fs2.async.immutable.Signal

import cats.data.Validated._

import breeze.plot._

import ModelSelection._

case class Plotter[A, B, P]
(plotData: B, data: Option[(Nel[A], Nel[A])])
(implicit strat: Strategy, backend: Viz[B, A, P])
{
  def setup: Task[Plotter[A, B, P]] = {
    backend.setup(plotData).map(_ => this)
  }

  def fold(train: Nel[A], test: Nel[A]): Task[Plotter[A, B, P]] = {
    backend.fold(plotData)(train.unwrap, test.unwrap)
      .map(a => Plotter(plotData, Some((train, test))))
  }

  def step(est: Est[P]): Task[Plotter[A, B, P]] = {
    data
      .map(a => backend.step(plotData)(est.params))
      .getOrElse(Task.now(()))
      .map(a => this)
  }
}

object Plotter
{
  def empty[A, B, P]
  (implicit strat: Strategy, backend: Viz[B, A, P]) =
    Plotter[A, B, P](backend.init, None)
}

object PlottedModelSelection
{
  implicit def strat = Strategy.sequential

  implicit lazy val scheduler = Scheduler.fromFixedDaemonPool(1)

  val sleep = time.sleep[Task] _ andThen Pull.outputs _

  def plotLoop[A, B, P, M]
  (plotter: Plotter[A, B, P], stepInterval: FiniteDuration)
  : Trans[Learn[A, P, M], Unit] = {
    type P1 = P
    import Pull._
    import Learn._
    receive1 {
      case a #: h =>
        val rec = plotLoop((_: Plotter[A, B, P1]), stepInterval)(h)
        a match {
          case Go() =>
            eval(plotter.setup) >> sleep(1.second) >> rec(plotter)
          case Step(e) =>
            eval(plotter.step(e)).flatMap { plt =>
              sleep(stepInterval) >> rec(plt)
            }
          case Fold(train, test) =>
            eval(plotter.fold(train, test)) flatMap { plt =>
              sleep(1.second) >> rec(plt)
            }
          case Done() =>
            sleep(1.second) >> done
          case Result(_) =>
            eval(Task(())) >> rec(plotter)
        }
    }
  }
}

case class PMSCore[A, B, P, M, V]
(msv: MSV[A, P, M, V], q: Queue[Task, Learn[A, P, V]],
  finished: Signal[Task, Boolean], stepInterval: FiniteDuration)
(implicit backend: Viz[B, A, P])
{
  import PlottedModelSelection._

  type MS = ModelSelection[A, P, V]
  type MSV = ModelSelectionValidation[A, P, V]
  type Res = String ValidatedNel MSV
  type L = Learn[A, P, V]

  lazy val results: Stream[Task, String ValidatedNel MS] = {
    val em = Stream.empty[Task, String ValidatedNel MS]
    def send(t: Learn[A, P, V]) =
        Stream.eval(q.enqueue1(t)).flatMap(_ => em)
    msv.unified.flatMap {
      case Valid(l @ Learn.Fold(train, test)) => send(l)
      case Valid(l @ Learn.Result(m)) =>
        send(l) ++ Stream.emit(m.valid)
      case Valid(l) => send(l)
      case i @ Invalid(_) => Stream.emit(i)
    }
  }

  lazy val validation =
    MSV.validation(msv, results)

  def plotStream: fs2.Stream[Task, Unit] = {
    val s = Stream.emit(Learn.Go[A, P, V]()) ++ q.dequeue
    finished.interrupt(s.pull(plotLoop(Plotter.empty[A, B, P], stepInterval)))
  }

  def waitForCompletion(res: Either[Unit, Res])
  : Trans[Option[Either[Unit, Res]], Res] = {
    import Pull._
    receive1 {
      case a #: h =>
        a match {
          case Some(SLeft(_)) => waitForCompletion(res)(h)
          case Some(r @ SRight(_)) => waitForCompletion(r)(h)
          case None => res.map(output1(_)).fold(_ => done, identity) >> done
        }
    }
  }

  def plotWithTerminator: Stream[Task, Option[Either[Unit, Res]]] =
    plotStream.map(_ => SLeft[Unit, Res](())).noneTerminate

  def mainT: Stream[Task, Option[Either[Unit, Res]]] =
    validation.map(SRight[Unit, Res](_).some)

  def main =
    mainT
      .merge(plotWithTerminator)
      .pull(waitForCompletion(SLeft[Unit, Res](())))
}

case class PlottedModelSelection[S, B, P, M, V]
(msv: MSV[S, P, M, V],
  stepInterval: FiniteDuration = 100.millis)
(implicit backend: Viz[B, S, P])
{
  import PlottedModelSelection._

  type MSV = ModelSelectionValidation[S, P, V]
  type Res = String ValidatedNel MSV

  def queue = async.unboundedQueue[Task, Learn[S, P, V]]

  def finished = async.signalOf[Task, Boolean](false)

  def core =
    for {
      q <- Stream.eval(queue)
      f <- Stream.eval(finished)
    } yield PMSCore(msv, q, f, stepInterval)

  def main: Stream[Task, Res] =
    core flatMap (_.main)
}
