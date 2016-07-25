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

case class Plotter[S, B, P]
(plotData: B, data: Option[(Nel[S], Nel[S])])
(implicit strat: Strategy, backend: Viz[B, S, P])
{
  def setup: Task[Plotter[S, B, P]] = {
    backend.setup(plotData).map(_ => this)
  }

  def fold(train: Nel[S], test: Nel[S]): Task[Plotter[S, B, P]] = {
    backend.fold(plotData)(train.unwrap, test.unwrap)
      .map(a => Plotter(plotData, Some((train, test))))
  }

  def step(est: Est[P]): Task[Plotter[S, B, P]] = {
    data
      .map(a => backend.step(plotData)(est.params))
      .getOrElse(Task.now(()))
      .map(a => this)
  }
}

object Plotter
{
  def empty[S, B, P]
  (implicit strat: Strategy, backend: Viz[B, S, P]) =
    Plotter[S, B, P](backend.init, None)
}

object PlottedModelSelection
{
  implicit def strat = Strategy.sequential

  implicit lazy val scheduler = Scheduler.fromFixedDaemonPool(1)

  val sleep = time.sleep[Task] _ andThen Pull.outputs _

  def plotLoop[S, B, P, M]
  (plotter: Plotter[S, B, P], stepInterval: FiniteDuration)
  : Trans[Learn[S, P], Unit] = {
    type P1 = P
    import Pull._
    import Learn._
    receive1 {
      case a #: h =>
        val rec = plotLoop((_: Plotter[S, B, P1]), stepInterval)(h)
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

case class PMSCore[S, B, P, M, V]
(msv: MSV[S, P], q: Queue[Task, Learn[S, P]],
  finished: Signal[Task, Boolean], stepInterval: FiniteDuration)
(implicit backend: Viz[B, S, P])
{
  import PlottedModelSelection._

  type Res = Vali[MSVData]
  type L = Learn[S, P]

  lazy val results: Stream[Task, Vali[ModelSelection]] = {
    val em = Stream.empty[Task, Vali[ModelSelection]]
    def send(t: Learn[S, P]) =
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
    val s = Stream.emit(Learn.Go[S, P]()) ++ q.dequeue
    finished.interrupt(s.pull(plotLoop(Plotter.empty[S, B, P], stepInterval)))
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
(msv: MSV[S, P],
  stepInterval: FiniteDuration = 100.millis)
(implicit backend: Viz[B, S, P])
{
  import PlottedModelSelection._

  def queue = async.unboundedQueue[Task, Learn[S, P]]

  def finished = async.signalOf[Task, Boolean](false)

  def core =
    for {
      q <- Stream.eval(queue)
      f <- Stream.eval(finished)
    } yield PMSCore(msv, q, f, stepInterval)

  def main: Stream[Task, Vali[MSVData]] =
    core flatMap (_.main)
}
