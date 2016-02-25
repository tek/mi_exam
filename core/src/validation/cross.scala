package tryp
package mi

import cats._

case class CrossValidator[A: Sample, B, O](n: Int, data: Nel[A],
  trainer: Nel[A] => Trainer[B], validator: Nel[A] => Validator[A, B, O],
  stop: StopCriterion[B])
{
  val l = data.unwrap

  def run = {
    0 until l.length by n map { start =>
      oneRange(start, start + n)
    }
  }

  lazy val sliceError = s"couldn't slice data by $n"

  def separate(start: Int, end: Int) = {
    (l.slice(0, start) ++ l.slice(end, l.length - 1)).nelXor(sliceError) |@|
      l.slice(start, end).nelXor(sliceError)
  }

  def oneRange(start: Int, end: Int) = {
    separate(start, end)
      .map { case (a, b) => oneTrain(trainer(a), validator(b)) }
  }

  def oneTrain(train: Trainer[B], valid: Validator[A, B, O]) = {
    val res = train.run(stop)
    (res, valid.run(res.params))
  }
}
