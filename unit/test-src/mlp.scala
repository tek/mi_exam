package tryp
package mi
package mlp
package unit

import cats._, data.{Func => _, _}

import spire.math._
import spire.algebra._
import spire.implicits._
import spire.random._

import breeze.linalg.{DenseVector, DenseMatrix, Transpose}

trait InternalBase
extends Spec
{
  case class Dat(feature: Col, value: Double)

  object Dat
  {
    implicit val datSample: Sample[Dat] =
      new Sample[Dat] {
        val name = "default"

        def cls(a: Dat) = LabeledClass(name)

        lazy val classes = Map(1.0 -> LabeledClass(name))

        def feature(a: Dat) = a.feature

        def value(a: Dat) = a.value
      }
  }

  val sample: Dat

  val conf: TrainConf

  lazy val train = MLPTrainer(Nel(sample), conf)

  val costFun = QuadraticError

  lazy val initW = train.initialParams

  lazy val f1 = sample.feature(0)

  lazy val pred = train.predict(sample, train.initialParams)

  def optimizer = train.optimize

  def forward = pred.pred

  lazy val backward = optimizer.backprop(forward, pred.param)

  lazy val grad = optimizer.modelGradient(forward, backward)

  lazy val costError = costFun.deriv.f(pred.sample.value, forward.output)
}

class InternalTrivialSpec
extends InternalBase
{
  def is = s2"""
  input coord $input
  output coord $output
  back prop $backProp
  gradient $gradient
  complete gradient $gradientComplete
  cost $cost
  result $result
  """

  val layers = Nel(3)

  val steps = 1

  def eta = 0.7

  def transfer = PseudoIdentity

  val x0 = 0.7

  val w0_00 = 0.4

  val h1_0 = x0 * w0_00

  lazy val sample = Dat(DenseVector(x0, 0d), 1d)

  def weights =
    new ManualWeights(Nel(
      DenseMatrix.create(3, 2, Array(w0_00, 0d, 0d, 0d, 0d, 0d)),
      DenseMatrix.create(1, 3, Array(1d, 0d, 0d))
    ))

  lazy val conf = TrainConf(transfer, eta, layers, steps, weights, costFun)

  def input = {
    forward.in must_== Nel(Col(x0, 0d), Col(h1_0, 0d, 0d), Col(h1_0))
  }

  def output = {
    forward.out must_== Nel(Col(x0, 0d), Col(h1_0, 0d, 0d), Col(h1_0))
  }

  def backProp = {
    backward.head(0) must beCloseTo(h1_0 * h1_0, 0.001)
  }

  def gradient = {
    grad.head(0, 0) must beCloseTo(h1_0 * h1_0 * f1, 0.001)
  }

  lazy val error = h1_0 - sample.value

  def gradientComplete = {
    optimizer(pred).head(0, 0) must_== (grad.head(0, 0) * error)
  }

  def cost = {
    costError must beCloseTo(error, 0.001)
  }

  def result = {
    val r = train.step(initW)
    val d1 = h1_0 * h1_0 * f1
    r.head(0, 0) must beCloseTo((w0_00 - (eta * d1 * error)), 0.001)
  }
}

class InternalComplexSpec
extends InternalBase
{
  def is = s2"""
  input coord $input
  output coord $output
  back prop $backProp
  gradient $gradient
  cost $cost
  """

  lazy val tr = Logistic(beta)

  lazy val td = tr.deriv

  val layers = Nel(3)

  val steps = 1

  def beta = 1d

  def eta = 0.7

  val x0 = 0.7

  val w0_00 = 0.4

  val h1_0 = x0 * w0_00

  val s1_0 = tr.f(h1_0)

  def h2_0 = s1_0 // weight == 1d

  val s2_0 = tr.f(s1_0)

  def weights =
    new ManualWeights(Nel(
      DenseMatrix.create(3, 2, Array(w0_00, 0d, 0d, 0d, 0d, 0d)),
      DenseMatrix.create(1, 3, Array(1d, 0d, 0d))
    ))

  lazy val conf = TrainConf(tr, eta, layers, steps, weights, costFun)

  lazy val sample = Dat(DenseVector(x0, 0d), 1d)

  def input = {
    val ti1 = Col(td.f(x0), td.f(0d))
    forward.in must_== Nel(ti1, Col(h1_0, 0d, 0d), Col(h2_0))
  }

  def output = {
    val n = tr.f(0d)
    forward.out must_== Nel(Col(x0, 0d), Col(s1_0, n, n), Col(s2_0))
  }

  val b1 = td.f(h2_0)

  def backProp = {
    backward.tail(0) must_== Col(b1) and
      (backward.head(0) must_== b1 * td.f(h1_0))
  }

  def gradient = {
    grad.head(0, 0) must_== td.f(h1_0) * b1 * f1
  }

  def cost = {
    val err = s2_0 - sample.value
    costError must beCloseTo(err, 0.001)
  }
}

class IrisSpec
extends Spec
{
  def is = s2"""
  main $main
  """

  val layers = Nel(7)

  val steps = 1000

  val epsilon = 0.0001

  def beta = 3

  def eta = 0.5

  lazy val transfer = new Logistic(beta)

  val cost = QuadraticError

  implicit lazy val conf =
    TrainConf(transfer, eta, layers, steps, RandomWeights, cost)

  val stop = ConvergenceStopCriterion(steps, epsilon)

  lazy val valid = CrossValidator[Iris, Weights, PState](
    5, Iris.loadNel, MLPTrainer[Iris](_, conf), MLPValidator[Iris](_, conf),
    stop)

  def main = {
    valid.run.foreach {
      case Xor.Left(err) => p(err)
      case Xor.Right((TrainResult(iter, _), Validation(data))) =>
        hl
        if (iter == steps) p("training hasn't converged")
        else p(s"training converged after $iter iterations")
        data.unwrap.map(_.result).foreach(p(_))
    }
    1 === 1
  }
}
