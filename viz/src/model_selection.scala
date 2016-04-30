package tryp
package mi

import fs2._
import fs2.util._
import Step._
import Stream.Handle

import cats.data.Xor._

case class PlottedModelSelection[A, P, O]
(msv: ModelSelectionValidator[A, P, O])
{
  implicit def strat = Strategy.sequential

  lazy val q = async.unboundedQueue[Task, Estimation[P]].unsafeRun

  lazy val results = {
    msv.unified.flatMap {
      case r @ Right(m) =>
        // TODO send message into queue resulting in a new plot
        Stream.emit(r)
      case l @ Left(e) => Stream.eval(q.enqueue1(e).map(_ => l))
    }
  }

  lazy val validation = {
    results.stripW.vectorChunkN(Int.MaxValue).take(1).map { res =>
      val stats = res.map(_.validation.stats(msv.cost))
      ModelSelectionValidation(res, stats, msv.config, msv.totalCount)
    }
  }

  lazy val unsafeValidation = validation.runLog.run.unsafeRun.headOption

  // def setupPlot = {

  // }

  def plotter = Stream.emit[Task, Int](1)
}
