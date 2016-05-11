package tryp
package mi

import fs2._
import fs2.util._
import Step._
import Stream.Handle

import cats.data.Xor._

object ModelSelection
{
  type Result[A, P, O] = Stream[Task, Learn[A, P, O]]

  type Trans[A, B] = Handle[Task, A] => Pull[Task, B, Handle[Task, B]]
}

import ModelSelection._

case class ModelSelection[A, P, O]
(estimation: Estimation[P], validation: Validation[A, O])

case class ModelTrainInfo[A, P, O]
(model: ModelSelection[A, P, O], stats: EstimationStats)

trait ModelSelector[A, P, O]
{
  def config: ModelSelectionConf

  def data: Nel[A]

  def result: List[String Xor Result[A, P, O]]

  def validatedCount: Int
}

sealed trait Learn[A, P, O]

object Learn
{
  case class Go[A, P, O]()
  extends Learn[A, P, O]

  case class Fold[A, P, O](train: Nel[A], test: Nel[A])
  extends Learn[A, P, O]

  case class Step[A, P, O](estimation: Estimation[P])
  extends Learn[A, P, O]

  case class Result[A, P, O](model: ModelSelection[A, P, O])
  extends Learn[A, P, O]

  case class Done[A, P, O]()
  extends Learn[A, P, O]
}

case class CrossValidator[A, P, M, O](data: Nel[A],
  config: ModelSelectionConf, estimator: Nel[A] => Estimator[A, P],
  modelCreator: Nel[A] => ModelCreator[P, M],
  validator: Nel[A] => Validator[A, M, O], stop: StopCriterion[P])
extends ModelSelector[A, P, O]
{
  def result: List[String Xor Result[A, P, O]] = intervals.map(interval).toList

  def interval(start: Int): String Xor Result[A, P, O] = {
    separate(start)
      .map { case (a, b) => learn(a, b) }
  }

  def validatedCount = config.trials map(_ * testSize) getOrElse(l.length)

  private[this] lazy val l = data.unwrap

  private[this] lazy val testSize = (l.length / config.folds).max(1)

  private[this] def intervals = {
    val all = 0 until l.length by testSize
    config.trials map(all.take) getOrElse(all)
  }

  private[this] lazy val sliceError = s"couldn't slice data by ${config.folds}"

  private[this] def separate(start: Int) = {
    val end = start + testSize
    (l.slice(0, start) ++ l.slice(end, l.length - 1)).nelXor(sliceError) |@|
      l.slice(start, end).nelXor(sliceError)
  }

  private[this] def learn(trainData: Nel[A], testData: Nel[A])
  : Stream[Task, Learn[A, P, O]] = {
    type PP = P
    type In = Estimation[P]
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
    Stream.emit(Learn.Fold[A, PP, O](trainData, testData)) ++
      estimator(trainData).stream(stop).open.flatMap(trans(None)).run
        .map {
          case Left(e) => Learn.Step(e)
          case Right(e) =>
            val model = modelCreator(trainData).run(e)
            Learn.Result(ModelSelection(e, validator(testData).run(model)))
        }
  }
}

case class ModelSelectionValidation[A, P, O]
(results: Vector[ModelSelection[A, P, O]], stats: Vector[EstimationStats],
 config: ModelSelectionConf, totalCount: Int)
{
  lazy val totalError = stats.map(_.totalError).sum

  lazy val foldError = totalError / results.length

  def info: Vector[ModelTrainInfo[A, P, O]] =
    results.zip(stats).map { case (a, b) => ModelTrainInfo(a, b) }

  def successes = stats.map(_.successes).sum

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
    val ModelSelection(Estimation(iter, _), Validation(data)) = info.model
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
    validation.info.foreach(fold(_, verbose))
    log.info("")
    log.info("total error:" + f" $totalError%g".red)
    log.info("average fold error:" + f" $foldError%g".yellow)
    log.info(s"${totalSuccesses.greenString}/$totalCount classified correctly")
  }

  def foldError = validation.foldError

  def totalError = validation.totalError

  def totalSuccesses = validation.successes
}

abstract class ModelSelectionValidator[A, P, O]
extends Logging
{
  private[this] type E = Estimation[P]
  private[this] type M = ModelSelection[A, P, O]

  override def loggerName = List("msv")

  val cross: ModelSelector[A, P, O]

  val cost: Func2

  def config = cross.config

  lazy val result = cats.data.XorT(cross.result)

  lazy val errors = result.swap.collectRight

  lazy val results: List[Stream[Task, Learn[A, P, O]]] = result.collectRight

  def totalCount = cross.validatedCount

  def unified: Result[A, P, O] =
    results.foldLeft(Stream.empty: Result[A, P, O])(_ ++ _) ++
      Stream.emit(Learn.Done())

  def model: Stream[Task, ModelSelection[A, P, O]] =
    unified
      .collect { case Learn.Result(r) => r }

  def unsafeModel =
    model
      .runLog.run.unsafeRun

  lazy val unsafeValidation = {
    val res = unsafeModel
    val stats = res.map(_.validation.stats(cost))
    ModelSelectionValidation(res, stats, config, totalCount)
  }

  def printer = MSPrinter(unsafeValidation, config, totalCount)
}
