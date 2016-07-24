package tryp
package mi
package mlp

import spire.math._
import spire.algebra._
import spire.implicits._
import spire.random._

import breeze.linalg.sum
import breeze.numerics.abs

case class LayerState(in: Col, out: Col)

case class Layers(layers: Nel[LayerState])
{
  def addLayer(in: Col, out: Col) =
    copy(layers.combine(Nel(LayerState(in, out))))

  def in = layers.map(_.in)

  def out = layers.map(_.out)

  def output = layers.last.out(0)

  def value = output

  def last = layers.last
}

case class MLP(weights: Weights, layers: Layers)
{
  def addLayer(in: Col, out: Col) =
    copy(layers = layers.addLayer(in, out))

  def in = layers.in

  def out = layers.out

  def output = layers.output
}

object MLP
{
  def init[T <: Func](out: Col, tfDeriv: T, weights: Weights) = {
    MLP(weights, Layers(Nel(LayerState(tfDeriv.f(out), out))))
  }

  implicit def instance_ModelState_MLP: ModelState[MLP] =
    new ModelState[MLP] {
      def output(a: MLP) = a.output
    }

  implicit def instance_ParamDiff_Weights: ParamDiff[Weights] =
    new ParamDiff[Weights] {
      def diff(a: Weights, b: Weights) =
        a.unwrap.zip(b.unwrap)
          .map { case (a, b) => sum(abs(a :- b)) / a.size }
          .sum
    }

  def msv[S: Sample]
  (data: Nel[S], conf: MLPLearnConf, sconf: ModelSelectionConf)
  (implicit mc: MC[S])
  : ModelSelectionValidator[S, Weights, MLP, Double] = {
    val stop = ParamDiffStopCriterion[Weights](sconf.steps, sconf.epsilon)
    lazy val validator =
      CrossValidator[S, Weights, MLP, Double](data, sconf,
        MLPEstimator[S](_, conf, stop), _ => MLPModelCreator(conf),
        MLPValidator[S](_, conf))
    MLPModelSelectionValidator(validator, conf.cost)
  }
}

trait WeightInitializer
{
  def apply(features: Int, layers: Nel[Int], output: Int): Weights
}

object RandomWeights
extends WeightInitializer
{
  def apply(features: Int, layers: Nel[Int], output: Int): Weights = {
    ((features :: layers.unwrap) :+ output)
      .sliding(2)
      .collect {
        case List(a, b) =>
          Mat.rand(b, a) :* (1.0 / a)
      }
      .toList match {
        case List(a, b @ _*) => Nel(a, b: _*)
        case _ => Nel(Mat.fill(1, 1)(1.0))
      }
  }
}

class ManualWeights(weights: Weights)
extends WeightInitializer
{
  def apply(features: Int, layers: Nel[Int], output: Int): Weights = {
    weights
  }
}

case class MLPLearnConf
(transfer: DFunc[_ <: Func], eta: Double, layers: Nel[Int],
  initializer: WeightInitializer, cost: DFunc2[_ <: Func2], bias: Boolean,
  mode: LearnConf.LearnMode)
  {
    def initialParams(features: Int) = {
      val f = features + (if (bias) 1 else 0)
      initializer(f, layers, 1)
    }
  }

object MLPLearnConf
{
  import LearnConf._

  def default(
    transfer: DFunc[_ <: Func] = Logistic(0.5),
    eta: Double = 0.8,
    layers: Nel[Int] = Nel(4, 3),
    initializer: WeightInitializer = RandomWeights,
    cost: DFunc2[_ <: Func2] = QuadraticError,
    bias: Boolean = true,
    mode: LearnMode = Batch
  ) =
      MLPLearnConf(transfer, eta, layers, initializer, cost, bias, mode)
}

case class MLPModelCreator(config: MLPLearnConf)
extends ModelCreator[Weights, MLP]
{
  def run(est: Est[Weights]): MLP =
    MLP.init(Col(0), config.transfer.deriv, est.params)
}

case class MLPPredictor(config: MLPLearnConf)
extends Predictor[Weights, MLP, Double]
{
  def transfer = config.transfer

  lazy val creator = MLPModelCreator(config)

  def input[S: Sample](sample: S) = {
    if (config.bias) Col.vertcat(Col(1d), sample.feature)
    else sample.feature
  }

  def layer(state: MLP, w: Mat) = {
    val in = w * state.layers.last.out
    val out = transfer.f(in)
    state.addLayer(in, out)
  }

  def apply[S: Sample](sample: S, weights: Weights)
  (implicit mc: MC[S])
  : Prediction[S, MLP, Double] = {
    val z = MLP.init(input(sample), transfer.deriv, weights)
    val pred = weights.foldLeft(z)(layer)
    Prediction(sample, pred, pred.output, mc.predictedClass(pred.output))
  }
}

case class BackProp
(transfer: DFunc[_ <: Func], cost: DFunc2[_ <: Func2])
extends Optimizer[Weights, MLP, Double]
{
  lazy val deriv = transfer.deriv

  def backprop(state: MLP): Nel[Col] = {
    val hPrime = state.layers.in.map(deriv.f(_)).reverse
    state.weights
      .toList
      .reverse
      .zip(hPrime.tail)
      .init
      .foldLeft(Nel(hPrime.head)) {
        case (z, (w, h)) =>
          (h :* (w.t * z.head)) :: z
      }
  }

  def modelGradient(state: MLP, deltas: Nel[Col]): Weights = {
    deltas
      .fzip(state.layers.out)
      .map { case (a, b) => a * b.t }
  }

  def apply[S: Sample](pred: Prediction[S, MLP, Double])
  (implicit mc: MC[S]): Weights = {
    val state = pred.model
    val weights = state.weights
    val back = backprop(state)
    val mg = modelGradient(state, back)
    val ce = cost.deriv.f(pred.sample.valueOrNaN, state.output)
    mg.map(_ * ce)
  }
}

abstract class MLPStep[S: Sample]
extends EstimationStep[Weights]
{
  val config: MLPLearnConf

  val data: Nel[S]

  val eta = config.eta / data.length

  lazy val predict = MLPPredictor(config)

  lazy val optimize = BackProp(config.transfer, config.cost)
}

// TODO parallel computing
case class BatchStep[S: Sample](data: Nel[S], config: MLPLearnConf)
(implicit mc: MC[S])
extends MLPStep
{
  def apply(weights: Weights): I = {
    val optimized = data map(a => optimize(predict(a, weights)))
    optimized
      .foldLeft(weights) { (z, pred) =>
        z.fzipWith(pred) { (a, b) => a :- (b * eta) }
      }
      .validNel[String]
  }
}

case class OnlineStep[S: Sample](data: Nel[S], config: MLPLearnConf)
(implicit mc: MC[S])
extends MLPStep
{
  def apply(weights: Weights): I = {
    data
      .foldLeft(weights) { (z, sample) =>
        val o = optimize(predict(sample, z))
        z.fzipWith(o) { (a, b) => a :- (b * eta) }
      }
      .validNel[String]
  }
}

case class MLPEstimator[S: Sample]
(data: Nel[S], config: MLPLearnConf, stop: StopCriterion[Weights])
(implicit mc: MC[S])
extends UniformIterativeEstimator[Weights]
{
  lazy val initialParams = config.initialParams(Sample[S].featureCount)

  lazy val step: MLPStep[S] = {
    if (config.mode == LearnConf.Batch) BatchStep(data, config)
    else OnlineStep(data, config)
  }
}

case class MLPSV[S: Sample](data: S, state: MLP)
(implicit mc: MC[S])
extends SampleValidation[S, Double]
{
  def output = state.output

  override lazy val predictedClass = mc.predictedClass(output)

  def error(cost: Func2) = data.value.map(cost.f(_, output)).toValidatedNel
}

case class MLPValidator[S: Sample]
(data: Nel[S], config: MLPLearnConf)
(implicit mc: MC[S])
extends Validator[S, MLP, Double]
{
  lazy val predict = MLPPredictor(config)

  def verify(model: MLP)(sample: S): SampleValidation[S, Double] = {
    val pred = predict(sample, model.weights)
    MLPSV(sample, pred.model)
  }

  def run(model: MLP) = {
    val pred = data map(verify(model))
    Validation(pred)
  }
}

case class MLPModelSelectionValidator[S]
(cross: CrossValidator[S, Weights, MLP, Double], cost: Func2)
extends ModelSelectionValidator[S, Weights, MLP, Double]
