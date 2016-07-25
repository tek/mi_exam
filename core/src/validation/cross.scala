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
  type Result[S, P] = Stream[Task, String ValidatedNel Learn[S, P]]

  type Trans[A, B] = Handle[Task, A] => Pull[Task, B, Handle[Task, B]]
}

import ModelSelection._

trait ModelSelection
{
  def estimation: Estimation
  def validation: Validation
}

case class MS[P]
(estimation: Est[P], validation: Validation)
extends ModelSelection

case class ModelTrainInfo
(model: ModelSelection, stats: EstimationStats)

sealed trait LearnBase

sealed trait Learn[S, P]
extends LearnBase

object Learn
{
  case class Go[S, P]()
  extends Learn[S, P]

  case class Fold[S, P](train: Nel[S], test: Nel[S])
  extends Learn[S, P]

  case class Step[S, P](estimation: Est[P])
  extends Learn[S, P]

  case class Result[S, P](model: ModelSelection)
  extends Learn[S, P]

  case class Done[S, P]()
  extends Learn[S, P]
}

trait CrossValidatorI[S, P]
{
  type R = Result[S, P]
  def result: R
  def config: MSConf
  def validatedCount: Int
}

case class CrossValidator[S, P, M](data: Nel[S],
  config: MSConf, estimator: Nel[S] => Estimator[P],
  modelCreator: Nel[S] => ModelCreator[P, M],
  validator: Nel[S] => Validator[M])
(implicit mt: ModelTypes[M])
extends CrossValidatorI[S, P]
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
  : Stream[Task, String ValidatedNel Learn[S, P]] = {
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
    Stream.emit(Learn.Fold[S, PP](trainData, testData).valid) ++
      estimator(trainData).stream.pull(trans(None))
        .map {
          case Left(e) => e map (Learn.Step(_))
          // case Invalid(err) => log.error(s"model selection failed: $err")
          case Right(v) => v map { e =>
            val model = modelCreator(trainData).run(e)
            Learn.Result(MS(e, validator(testData).run(model)))
          }
        }
  }
}

case class MSVData
(results: Nel[ModelSelection], stats: Nel[EstimationStats],
 config: MSConf, totalCount: Int)
{
  lazy val totalError = stats.map(_.totalError).toList.sum

  lazy val foldError = totalError / results.length

  def info: Nel[ModelTrainInfo] =
    results.fzip(stats).map { case (a, b) => ModelTrainInfo(a, b) }

  def successes = stats.map(_.successes).toList.sum

  def printer = MSPrinter(this, config, totalCount)
}

case class MSPrinter(validation: MSVData, config: MSConf,
  totalCount: Int)
extends Logging
{
  override def loggerName = List("ms")

  def short() = all(false)

  def verbose() = all(true)

  def fold(info: ModelTrainInfo, verbose: Boolean) = {
    val iter = info.model.estimation.iterations
    val data = info.model.validation.data
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

trait MSVI
{
  def config: MSConf

  def totalCount: Int

  def cost = config.cost
}

case class MSV[S, P](cross: CrossValidatorI[S, P])
extends MSVI
with Logging
{
  override def loggerName = List("msv")

  def config = cross.config

  lazy val result = cross.result

  def totalCount = cross.validatedCount

  def unified: Result[S, P] =
    result ++ Stream.emit(Learn.Done().valid)

  def model =
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
  def validation(msv: MSVI, in: Stream[Task, Vali[ModelSelection]])
  : Stream[Task, String ValidatedNel MSVData] = {
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
              Valid(MSVData(r, s, msv.config, msv.totalCount))
            case Invalid(a) => Invalid(a)
          }
        case Invalid(a) => Invalid(a)
      }
  }

  def create[S: Sample, P, M, C](data: Nel[S])
  (implicit cm: CreateMSV[S, P, M, C], conf: C, sconf: MSConf)
  : MSV[S, P] =
  {
    val cross = 
      CrossValidator[S, P, M](data, sconf, cm.estimator.apply,
        cm.modelCreator.apply, cm.validator.apply)
    MSV(cross)
  }
}

trait CreateEstimator[S, P, C]
{
  def apply(data: Nel[S])
  (implicit conf: C, sconf: MSConf): Estimator[P]
}

trait CreateModelCreator[S, P, M, C]
{
  def apply(data: Nel[S])
  (implicit conf: C, sconf: MSConf): ModelCreator[P, M]
}

object CreateModelCreator
{
  def id[S, P, C] =
    new CreateModelCreator[S, P, P, C] {
      def apply(data: Nel[S])(implicit conf: C, sconf: MSConf) =
        IdModelCreator[P]()
    }
}

trait CreateValidator[S, M, C]
{
  def apply(data: Nel[S])
  (implicit conf: C, sconf: MSConf): Validator[M]
}

trait CreateMSV[S, P, M, C]
{
  def estimator: CreateEstimator[S, P, C]
  def modelCreator: CreateModelCreator[S, P, M, C]
  def validator: CreateValidator[S, M, C]
}

object CreateMSV
{
  implicit def createMSV[S, P, M, C]
  (implicit est: CreateEstimator[S, P, C], mod: CreateModelCreator[S, P, M, C],
    vali: CreateValidator[S, M, C]): CreateMSV[S, P, M, C] =
      new CreateMSV[S, P, M, C] {
        def estimator = est
        def modelCreator = mod
        def validator = vali
      }
}
