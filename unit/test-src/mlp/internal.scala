package tryp
package mi
package mlp

case class Dat(feature: Col, value: Double)

object Dat
{
  object Cls extends AutoClass[Dat]

  implicit def instance_ModelClasses_Dat: ModelClasses[Dat] =
    new ModelClasses[Dat] {
      def value(a: ModelClass[Dat]) = 1.0.valid
    }

  implicit val datSample: Sample[Dat] =
    new Sample[Dat] {
      def cls(a: Dat) = a.cls

      lazy val classes = Nel(Cls: ModelClass[Dat])

      def feature(a: Dat) = a.feature

      def featureCount = 2
    }
}

trait InternalBase
extends Spec
{
  val sample: Dat

  val conf: MLPLearnConf

  def bias = false

  lazy val stop = StepCountStopCriterion[Weights](1)

  lazy val train = MLPEstimator(Nel(sample), conf, stop)

  val costFun = QuadraticError

  lazy val initW = train.initialParams

  lazy val f1 = sample.feature(0)

  lazy val pred = train.step.predict(sample, train.initialParams)

  def optimizer = train.step.optimize

  def forward = pred.value

  lazy val backward = optimizer.backprop(forward, pred.model)

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

  def eta = 0.7

  def transfer = PseudoIdentity

  val x0 = 0.7

  val w0_00 = 0.4

  val h1_0 = x0 * w0_00

  lazy val sample = Dat(Col(x0, 0d), 1d)

  def weights =
    new ManualWeights(Nel(
      Mat.create(3, 2, Array(w0_00, 0d, 0d, 0d, 0d, 0d)),
      Mat.create(1, 3, Array(1d, 0d, 0d))
    ))

  lazy val conf =
    MLPLearnConf.default(transfer, eta, layers, weights, costFun, bias)

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
      Mat.create(3, 2, Array(w0_00, 0d, 0d, 0d, 0d, 0d)),
      Mat.create(1, 3, Array(1d, 0d, 0d))
    ))

  lazy val conf =
    MLPLearnConf.default(tr, eta, layers, weights, costFun, bias)

  lazy val sample = Dat(Col(x0, 0d), 1d)

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
