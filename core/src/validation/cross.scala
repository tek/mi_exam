package tryp
package mi

import fs2._
import fs2.util._
import Step._
import Stream.Handle

import cats.data.Xor._
import cats.data.Validated._

import ModelTypes._

object ModelSelection
{
  type Result[S, P, M] = Stream[Task, String ValidatedNel Learn[S, P, M]]

  type Trans[A, B] = Handle[Task, A] => Pull[Task, B, Handle[Task, B]]
}

import ModelSelection._

case class ModelSelection[S, P, V]
(estimation: Est[P], validation: Validation[S, V])

case class ModelTrainInfo[S, P, V]
(model: ModelSelection[S, P, V], stats: EstimationStats)

trait ModelSelector[S, P, M]
{
  type R = Result[S, P, M]
  def config: ModelSelectionConf

  def data: Nel[S]

  def result: R

  def validatedCount: Int
}

sealed trait Learn[S, P, M]

object Learn
{
  case class Go[A, P, M]()
  extends Learn[A, P, M]

  case class Fold[A, P, M](train: Nel[A], test: Nel[A])
  extends Learn[A, P, M]

  case class Step[A, P, M](estimation: Est[P])
  extends Learn[A, P, M]

  case class Result[A, P, M](model: ModelSelection[A, P, M])
  extends Learn[A, P, M]

  case class Done[A, P, M]()
  extends Learn[A, P, M]

//   case class Error[A, P, O](msg: String)
//   extends Learn[A, P, O]
}

case class CrossValidator[S, P, M, V](data: Nel[S],
  config: ModelSelectionConf, estimator: Nel[S] => Estimator[P],
  modelCreator: Nel[S] => ModelCreator[P, M],
  validator: Nel[S] => Validator[S, M, V])
(implicit mt: ModelTypes[M])
extends ModelSelector[S, P, V]
{
  def result: R =
    intervals.map(interval).flatSequence

  def interval(start: Int): R = {
    separate(start)
      .map(learn)
      .fold(a => Stream.emit(a.invalid), identity)
  }

  def validatedCount = config.trials map(_ * testSize) getOrElse(l.length)

  private[this] lazy val l = data.unwrap

  private[this] lazy val testSize = (l.length / config.folds).max(1)

  private[this] def intervals = {
    val all = 0 until l.length by testSize
    config.trials
      .map(all.take)
      .getOrElse(all)
      .toList
  }

  private[this] lazy val sliceError = s"couldn't slice data by ${config.folds}"

  private[this] def separate(start: Int) = {
    val end = start + testSize
    (l.slice(0, start) ++ l.slice(end, l.length - 1)).nelValid(sliceError) |@|
      l.slice(start, end).nelValid(sliceError)
  }

  /* Stream the estimation intermediates on the writer side of the output of
   * `trans`, then transform into the Learn algebra.
   */
  private[this] def learn(trainData: Nel[S], testData: Nel[S])
  : Stream[Task, String ValidatedNel Learn[S, P, V]] = {
    type PP = P
    type In = String ValidatedNel Est[P]
    import Pull._
    type Out = In Xor In
    def trans(z: Option[In]): Trans[In, Out] = receive1Option {
      case Some(a #: h) =>
        output1(a.left[In]) >> trans(Some(a))(h)
      case None => z match {
        case Some(e) => output1(e.right[In]) >> done
        case None => done
      }
    }
    Stream.emit(Learn.Fold[S, PP, V](trainData, testData).valid) ++
      estimator(trainData).stream.pull(trans(None))
        .map {
          case Left(e) => e map (Learn.Step(_))
          // case Invalid(err) => log.error(s"model selection failed: $err")
          case Right(v) => v map { e =>
            val model = modelCreator(trainData).run(e)
            Learn.Result(ModelSelection(e, validator(testData).run(model)))
          }
        }
  }
}

case class ModelSelectionValidation[S, P, V]
(results: Nel[ModelSelection[S, P, V]], stats: Nel[EstimationStats],
 config: ModelSelectionConf, totalCount: Int)
{
  lazy val totalError = stats.map(_.totalError).toList.sum

  lazy val foldError = totalError / results.length

  def info: Nel[ModelTrainInfo[S, P, V]] =
    results.fzip(stats).map { case (a, b) => ModelTrainInfo(a, b) }

  def successes = stats.map(_.successes).toList.sum

  def printer = MSPrinter(this, config, totalCount)
}

case class MSPrinter(validation: ModelSelectionValidation[_, _, _],
  config: ModelSelectionConf, totalCount: Int)
extends Logging
{
  override def loggerName = List("ms")

  def short() = all(false)

  def verbose() = all(true)

  def fold(info: ModelTrainInfo[_, _, _], verbose: Boolean) = {
    val ModelSelection(Est(iter, _), Validation(data)) = info.model
    val stats = info.stats
    log.info("")
    if (iter == config.steps) log.info("training hasn't converged")
    else log.info(s"training converged after $iter iterations")
    log.info(
      s"${stats.successes.greenString}/${stats.count} classified correctly")
    log.info(f"fold error: ${stats.totalError}%f")
    if (verbose) data.map(_.info).map(log.info(_))
  }

  def all(verbose: Boolean) = {
    validation.info.map(fold(_, verbose))
    log.info("")
    log.info("total error:" + f" $totalError%g".red)
    log.info("average fold error:" + f" $foldError%g".yellow)
    log.info(s"${totalSuccesses.greenString}/$totalCount classified correctly")
  }

  def foldError = validation.foldError

  def totalError = validation.totalError

  def totalSuccesses = validation.successes
}

trait MSV[S, P, M, V]
extends Logging
{
  private[this] type E = Est[P]
  private[this] type MS = ModelSelection[S, P, V]

  override def loggerName = List("msv")

  val cross: ModelSelector[S, P, V]

  val cost: Func2

  def config = cross.config

  lazy val result = cross.result

  def totalCount = cross.validatedCount

  def unified: Result[S, P, V] =
    result ++ Stream.emit(Learn.Done().valid)

  def model: Stream[Task, String ValidatedNel MS] =
    unified
      .collect {
        case i @ Invalid(_) => i
        case Valid(Learn.Result(r)) => Valid(r)
      }

  lazy val validation =
    MSV.validation(this, model)

  def printer = validation map (_.map(_.printer))
}

object MSV
{
  def validation[S, P, M, V](msv: MSV[S, P, M, V],
    in: Stream[Task, String ValidatedNel ModelSelection[S, P, V]])
  : Stream[Task, String ValidatedNel ModelSelectionValidation[S, P, V]] = {
    Stream.eval(in.runLog)
      .map(_.toList.sequenceU)
      .map {
        case i @ Invalid(_) => i
        case Valid(l @ List(_*)) => l.nelValid(s"empty estimation stats")
      }
      .collect {
        case Valid(r) =>
          r.map(_.validation.stats(msv.cost)).sequenceU match {
            case Valid(s) =>
              Valid(ModelSelectionValidation(r, s, msv.config, msv.totalCount))
            case Invalid(a) => Invalid(a)
          }
        case Invalid(a) => Invalid(a)
      }
  }

}
