package tryp
package mi
package viz

import java.util.concurrent.Executors

import fs2._
import fs2.util._

import breeze.plot._

import ModelSelection._
import PlotBackend.ops._
import Plotting.ops._

case class Plotter[A: Sample, B: PlotBackend, P: Plotting]
(plotData: B, data: Option[(Nel[A], Nel[A])])
{
  def setup: Task[Plotter[A, B, P]] = {
    Task.delay {
      plotData.setup()
      p("setup")
      this
    }
  }

  def fold(train: Nel[A], test: Nel[A]): Plotter[A, B, P] = {
    plotData.fold(train.unwrap.map(_.feature))
    Plotter(plotData, Some((train, test)))
  }

  def step(est: Estimation[P]): Task[Plotter[A, B, P]] = {
    Task.delay {
      hl
      data foreach { a =>
        plotData.step(est.params)
      }
      this
    }
  }
}

object Plotter
{
  def empty[A: Sample, B: PlotBackend, P: Plotting] =
    Plotter[A, B, P](PlotBackend[B].init, None)
}

case class PlottedModelSelection[A: Sample, B: PlotBackend, P: Plotting, O]
(msv: ModelSelectionValidator[A, P, O],
  stepInterval: FiniteDuration = 100.millis)
{
  type MS = ModelSelectionValidation[A, P, O]

  type L = Learn[A, P, O]

  implicit def strat = Strategy.sequential

  implicit lazy val scheduler =
    Executors.newScheduledThreadPool(2)

  lazy val q = async.unboundedQueue[Task, Learn[A, P, O]].unsafeRun

  lazy val finished = async.signalOf[Task, Boolean](false).unsafeRun

  lazy val results: Stream[Task, ModelSelection[A, P, O]] = {
    val em = Stream.empty[Task, ModelSelection[A, P, O]]
    def send(t: Learn[A, P, O]) =
        Stream.eval(q.enqueue1(t)).flatMap(_ => em)
    msv.unified.flatMap {
      case l @ Learn.Fold(train, test) => send(l)
      case l @ Learn.Result(m) =>
        send(l) ++ Stream.emit(m)
      case l => send(l)
    }
  }

  lazy val validation = {
    results.runLog.run.map { res =>
      val stats = res.map(_.validation.stats(msv.cost))
      ModelSelectionValidation(res, stats, msv.config, msv.totalCount)
    }
  }

  def unsafeValidation = validation.unsafeRun

  def plotLoop(plotter: Plotter[A, B, P]): Trans[Learn[A, P, O], Unit] = {
    type P1 = P
    import Pull._
    import Learn._
    receive1 {
      case a #: h =>
        val rec = plotLoop((_: Plotter[A, B, P1]))(h)
        a match {
          case Go() =>
            eval(plotter.setup) >> outputs(time.sleep(3.seconds)) >>
              rec(plotter)
          case Step(e) =>
            eval(plotter.step(e)) >> outputs(time.sleep(stepInterval)) >>
              rec(plotter)
          case Fold(train, test) =>
            rec(plotter.fold(train, test))
          case Done() =>
            done
          case Result(_) =>
            eval(Task(())) >> rec(plotter)
        }
    }
  }

  def plotStream: fs2.Stream[Task, Unit] = {
    val s = Stream.eval(Task(Learn.Go[A, P, O]())) ++ q.dequeue
    finished.interrupt(s.open.flatMap(plotLoop(Plotter.empty[A, B, P])).run)
  }

  def waitForCompletion(res: Either[Unit, MS])
  : Trans[Option[Either[Unit, MS]], MS] = {
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

  def plotWithTerminator: Stream[Task, Option[Either[Unit, MS]]] =
    plotStream.map(_ => SLeft[Unit, MS](())).noneTerminate

  def mainT: Stream[Task, Option[Either[Unit, MS]]] =
    Stream.eval(validation).map(SRight[Unit, MS](_).some)

  def main =
    mainT
      .merge(plotWithTerminator)
      .open
      .flatMap(waitForCompletion(SLeft[Unit, MS](())))
      .run
}
