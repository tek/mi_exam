package tryp
package mi

case class ModelSelection[A, P, O]
(estimation: Estimation[P], validation: Validation[A, O])

case class ModelTrainInfo[A, P, O]
(model: ModelSelection[A, P, O], stats: EstimationStats)

trait ModelSelector[A, P, O]
{
  def config: ModelSelectionConf

  def data: Nel[A]

  def result: List[Xor[String, ModelSelection[A, P, O]]]

  def validatedCount: Int
}

case class CrossValidator[A, P, M, O](data: Nel[A],
  config: ModelSelectionConf, estimator: Nel[A] => Estimator[A, P],
  modelCreator: Nel[A] => ModelCreator[P, M],
  validator: Nel[A] => Validator[A, M, O], stop: StopCriterion[P])
extends ModelSelector[A, P, O]
{
  def result = intervals.map(interval).toList

  def interval(start: Int): String Xor ModelSelection[A, P, O] = {
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

  private[this] def learn(est: Nel[A], valid: Nel[A]) = {
    val estimation = estimator(est).run(stop)
    val model = modelCreator(est).run(estimation)
    ModelSelection(estimation, validator(valid).run(model))
  }
}

case class ModelSelectionValidation[A, P, O]
(results: List[ModelSelection[A, P, O]], stats: List[EstimationStats])
{
  lazy val totalError = stats.map(_.totalError).sum

  lazy val foldError = totalError / results.length

  def info: List[ModelTrainInfo[A, P, O]] =
    results.zip(stats).map { case (a, b) => ModelTrainInfo(a, b) }

  def successes = stats.map(_.successes).sum
}

abstract class ModelSelectionValidator[A, P, O]
extends Logging
{
  override def loggerName = List("msv")

  val cross: ModelSelector[A, P, O]

  val cost: Func2

  def config = cross.config

  lazy val result = cats.data.XorT(cross.result)

  lazy val errors = result.swap.collectRight

  lazy val results = result.collectRight

  lazy val stats = results.map(_.validation.stats(cost))

  lazy val validation = ModelSelectionValidation(results, stats)

  def foldError = validation.foldError

  def totalError = validation.totalError

  def totalSuccesses = validation.successes

  def totalCount = cross.validatedCount

  def logFold(info: ModelTrainInfo[A, P, O], verbose: Boolean) = {
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

  def logInfo(verbose: Boolean) = {
    validation.info.foreach(logFold(_, verbose))
    log.info("")
    log.info("total error:" + f" $totalError%g".red)
    log.info("average fold error:" + f" $foldError%g".yellow)
    log.info(s"${totalSuccesses.greenString}/$totalCount classified correctly")
  }

  def logInfoShort() = logInfo(false)

  def logInfoVerbose() = logInfo(true)
}
