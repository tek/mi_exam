package tryp
package mi
package mlp

case class Dat(feature: Col, cls: ModelClass[Dat])

object Dat
{
  object Cls extends AutoClass[Dat]

  implicit def instance_ModelClasses_Dat: ModelClasses[Dat, Double] =
    new ModelClasses[Dat, Double] {
      def value(a: ModelClass[Dat]) = 1.0.valid

      lazy val classes = Nel(Cls: ModelClass[Dat])
    }

  implicit val datSample: Sample[Dat] =
    new Sample[Dat] {
      def cls(a: Dat) = a.cls

      def feature(a: Dat) = a.feature

      def featureCount = 2
    }
}

trait InternalBase
extends Spec
{
  val sample: Dat

  lazy val data = Nel(sample)

  val conf: MLPLearnConf

  def bias = false

  lazy val stop = StepCountStopCriterion[Weights](1)

  lazy val est = MLPEstimator(data, conf, stop)

  val costFun = QuadraticError

  lazy val initW = est.initialParams

  lazy val f1 = sample.feature(0)

  def predict = est.step.predict

  lazy val pred = predict(sample, est.initialParams)

  def optimize = est.step.optimize

  def forward = pred.model

  lazy val backward = optimize.backprop(forward)

  lazy val grad = optimize.modelGradient(forward, backward)

  lazy val costError = costFun.deriv.f(pred.sample.valueOrNaN, forward.output)
}

trait InternalTrivialSpecBase
extends InternalBase
{
  val hidden = Nel(3)

  def eta = 0.7

  def transfer = PseudoIdentity

  val x0 = 0.7

  val w0_00 = 0.4

  val h1_0 = x0 * w0_00

  lazy val sample = Dat(Col(x0, 0d), Dat.Cls)

  def weights: Weights.InitMode =
    new Weights.Manual(Nel(
      Mat.create(3, 2, Array(w0_00, 0d, 0d, 0d, 0d, 0d)),
      Mat.create(1, 3, Array(1d, 0d, 0d))
    ))

  def gradientMode: MLPLearnConf.GradientMode = MLPLearnConf.TrivialGradient

  lazy val conf =
    MLPLearnConf.default(2, transfer, eta, hidden, costFun, bias, weights,
      gradientMode = gradientMode)
}

class InternalTrivialSpec
extends InternalTrivialSpecBase
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

  lazy val error = h1_0 - sample.valueOrNaN

  def gradientComplete = {
    optimize(pred).head(0, 0) must_== (grad.head(0, 0) * error)
  }

  def cost = {
    costError must beCloseTo(error, 0.001)
  }

  def result = {
    val r = est.step(initW).getOrElse(sys.error("no result"))
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

  import Dat._

  lazy val tr = Logistic(beta)

  lazy val td = tr.deriv

  val hidden = Nel(3)

  def beta = 1d

  def eta = 0.7

  val x0 = 0.7

  val w0_00 = 0.4

  val h1_0 = x0 * w0_00

  val s1_0 = tr.f(h1_0)

  def h2_0 = s1_0 // weight == 1d

  val s2_0 = tr.f(s1_0)

  def weights =
    new Weights.Manual(Nel(
      Mat.create(3, 2, Array(w0_00, 0d, 0d, 0d, 0d, 0d)),
      Mat.create(1, 3, Array(1d, 0d, 0d))
    ))

  lazy val conf =
    MLPLearnConf.default(2, tr, eta, hidden, costFun, bias, weights)

  lazy val sample = Dat(Col(x0, 0d), Cls)

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
    val err = s2_0 - sample.valueOrNaN
    costError must beCloseTo(err, 0.001)
  }
}

class AnnealedWeightsSpec
extends InternalTrivialSpecBase
{
  def is = s2"""
  run $run
  """

  override def weights = Weights.Annealing

  def run = {
    1 === 1
  }
}
