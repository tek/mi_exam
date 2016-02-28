package tryp
package mi

import cats._, data._

case class Model[A, P, O]
(estimation: Estimation[P], validation: Validation[A, O])

case class CrossValidator[A: Sample, P, O](n: Int, data: Nel[A],
  trainer: Nel[A] => Estimator[P], validator: Nel[A] => Validator[A, P, O],
  stop: StopCriterion[P], trials: Option[Int])
{
  def result = intervals.map(interval).toList

  def interval(start: Int): String Xor Model[A, P, O] = {
    separate(start)
      .map { case (a, b) => learn(trainer(a), validator(b)) }
  }

  private[this] val l = data.unwrap

  private[this] val testSize = l.length / n

  private[this] def intervals = {
    val all = 0 until l.length by testSize
    trials map(all.take) getOrElse(all)
  }

  private[this] lazy val sliceError = s"couldn't slice data by $n"

  private[this] def separate(start: Int) = {
    val end = start + testSize
    (l.slice(0, start) ++ l.slice(end, l.length - 1)).nelXor(sliceError) |@|
      l.slice(start, end).nelXor(sliceError)
  }

  private[this] def learn(est: Estimator[P], valid: Validator[A, P, O]) = {
    val res = est.run(stop)
    Model(res, valid.run(res.params))
  }
}
