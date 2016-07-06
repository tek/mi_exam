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
import PlotBackend.ops._
import ParamPlotting.ops._

case class Plotter[A: Sample, B: PlotBackend, P: ParamPlotting]
(plotData: B, data: Option[(Nel[A], Nel[A])])
(implicit strat: Strategy)
{
  def setup: Task[Plotter[A, B, P]] = {
    plotData.setup.map(_ => this)
  }

  def fold(train: Nel[A], test: Nel[A]): Task[Plotter[A, B, P]] = {
    plotData.fold(train.unwrap)
      .map(a => Plotter(plotData, Some((train, test))))
  }

  def step(est: Est[P]): Task[Plotter[A, B, P]] = {
    data
      .map(a => plotData.step(est.params))
      .getOrElse(Task.now(()))
      .map(a => this)
  }
}

object Plotter
{
  def empty[A: Sample, B: PlotBackend, P: ParamPlotting](implicit strat: Strategy) =
    Plotter[A, B, P](PlotBackend[B].init, None)
}

object PlottedModelSelection
{
  implicit def strat = Strategy.sequential

  implicit lazy val scheduler = Scheduler.fromFixedDaemonPool(1)

  val sleep = time.sleep[Task] _ andThen Pull.outputs _

  def plotLoop[A, B, P, O]
  (plotter: Plotter[A, B, P], stepInterval: FiniteDuration)
  : Trans[Learn[A, P, O], Unit] = {
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

case class PMSCore
[A: Sample, B: PlotBackend, P: ParamPlotting, O]
(msv: ModelSelectionValidator[A, P, O], q: Queue[Task, Learn[A, P, O]],
  finished: Signal[Task, Boolean], stepInterval: FiniteDuration)
{
  import PlottedModelSelection._

  type M = ModelSelection[A, P, O]
  type MSV = ModelSelectionValidation[A, P, O]
  type Res = String ValidatedNel MSV
  type L = Learn[A, P, O]

  lazy val results: Stream[Task, String ValidatedNel M] = {
    val em = Stream.empty[Task, String ValidatedNel M]
    def send(t: Learn[A, P, O]) =
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
    ModelSelectionValidator.validation(msv, results)

  def plotStream: fs2.Stream[Task, Unit] = {
    val s = Stream.emit(Learn.Go[A, P, O]()) ++ q.dequeue
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

case class PlottedModelSelection
[A: Sample, B: PlotBackend, P: ParamPlotting, O]
(msv: ModelSelectionValidator[A, P, O],
  stepInterval: FiniteDuration = 100.millis)
{
  import PlottedModelSelection._

  type MSV = ModelSelectionValidation[A, P, O]
  type Res = String ValidatedNel MSV

  def queue = async.unboundedQueue[Task, Learn[A, P, O]]

  def finished = async.signalOf[Task, Boolean](false)

  def core =
    for {
      q <- Stream.eval(queue)
      f <- Stream.eval(finished)
    } yield PMSCore(msv, q, f, stepInterval)

  def main: Stream[Task, Res] =
    core flatMap (_.main)
}
