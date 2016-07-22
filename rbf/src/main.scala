package tryp
package mi
package rbf

import simulacrum._

import scalaz.std.vector.{vectorInstance => zVectorInstance}

import monocle.macros.Lenses
import monocle.function._
import monocle.syntax.apply._

import spire.math.exp
import spire.algebra._
import spire.implicits._
import spire.random._

import breeze.linalg.{sum, squaredDistance, pinv}
import breeze.linalg.functions.euclideanDistance
import breeze.numerics.abs
import breeze.generic.UFunc

import LearnConf._

import BasisFunction.ops._

case class RBFs[P: BasisFunction](bf: Nev[P])
{
  def centers = bf map(_.center)

  def count = bf.length
}

object RBFs
{
  implicit def instance_ParamDiff_RBFNet[P]: ParamDiff[RBFs[P]] =
    new ParamDiff[RBFs[P]] {
      def diff(a: RBFs[P], b: RBFs[P]): Double = {
        val dist = a.centers.fzip(b.centers)
          .map { case (a, b) => euclideanDistance(a, b) }
          .unwrap
        sum(dist) / a.count
      }
    }
}

case class RBFNet[P: BasisFunction](rbfs: RBFs[P], weights: Col)
{
  def bf = rbfs.bf
}

object RBFNet
{
  implicit def instance_ModelState_RBFNet[P]: ModelState[RBFNet[P]] =
    new ModelState[RBFNet[P]] {
      def output(a: RBFNet[P]) = a.output
    }
}

@typeclass abstract class UpdateParams[P: BasisFunction]
extends AnyRef
{
  def updateCenter(index: Int, diff: Col): RBFs[P] => RBFs[P]

  def updateParams(a: P)(all: Nev[P], lambda: Double): P
}

case class RBFLearnConf[P: BasisFunction](rbfs: Int,
  eta: Double, lambda: Double, initialization: Initialization[P], cost: Func2)
  {
    lazy val bf = BasisFunction[P]

    def initialParams(featureCount: Int) =
      initialization.create(featureCount, rbfs)
  }

object RBFLearnConf
{
  def default[P: BasisFunction: Initializer, S: Sample](
    rbfs: Int = 3,
    eta: Double = 0.3,
    lambda: Double = 2.0,
    initialization: Option[Initialization[P]] = None,
    cost: Func2 = QuadraticError
  ) = {
    val init = initialization | new RandomInitialization[P, S]
    RBFLearnConf[P](rbfs, eta, lambda, init, cost)
  }
}

case class RBFPredictor[P: BasisFunction](config: RBFLearnConf[P])
extends Predictor[RBFNet[P], RBFNet[P], Double]
{
  def rbfOut(data: Col, bf: Nev[P]): Nev[Double] = {
    bf.map(_.output(data))
  }

  def apply[S: Sample](sample: S, model: RBFNet[P])
  (implicit mc: MC[S])
  : Prediction[S, RBFNet[P], Double] = {
    val r = Col(rbfOut(sample.feature, model.bf).unwrap.toArray)
    val pred = model.weights.t * r
    Prediction(sample, model, pred, mc.predictedClass(pred))
  }
}

// TODO remove Predictor
// updateWeights isn't really necessary as they aren't part of the optimization
// process
// move updateWeights to Predictor, calculate weights lazily when evaluating
// a data point
case class KMeans[P: BasisFunction: UpdateParams]
(data: Nel[Col], config: RBFLearnConf[P], rbfs: RBFs[P])
{
  import UpdateParams.ops._

  lazy val eta = config.eta / data.length

  /* @return the Col in `t` and its index in `t` that is closest to `z`
   * by euclidean distance metrics
   */
  def closest(z: Col, t: Nev[Col]): (Col, Int) = {
    val c = t
      .map(euclideanDistance(z, _))
      .fzip(t)
      .fzip(Nev(0, (1 until t.length): _*))
      .reduceLeft((a, b) => if(a._1._1 < b._1._1) a else b)
    (c._1._2, c._2)
  }

  def updateCenter(z: RBFs[P], sample: Col) = {
    val (tq, index) = closest(sample, z.bf.map(_.center))
    val diff = (sample - tq) * eta
    UpdateParams[P].updateCenter(index, diff)(z)
  }

  lazy val moveCenters: RBFs[P] = {
    data.foldLeft(rbfs)(updateCenter)
  }

  def updateBf: RBFs[P] = {
    val moved = moveCenters
    RBFs(moved.bf.map(_.updateParams(moved.bf, config.lambda)))
  }
}

case class KMeansStep[S: Sample, P: BasisFunction: UpdateParams]
(data: Nel[Col], config: RBFLearnConf[P])
extends EstimationStep[RBFs[P]]
{
  def apply(params: RBFs[P]): I = {
    KMeans(data, config, params).updateBf.validNel[String]
  }
}

case class RBFEstimator[S: Sample, P: BasisFunction: Initializer: UpdateParams]
(data: Nel[S], config: RBFLearnConf[P], stop: StopCriterion[RBFs[P]])
extends UniformIterativeEstimator[RBFs[P]]
{
  lazy val initialParams = config.initialParams(Sample[S].featureCount)

  lazy val step = KMeansStep(data.map(_.feature), config)
}

case class RBFModelCreator[S: Sample, P: BasisFunction]
(data: Nel[S], config: RBFLearnConf[P])
(implicit mc: MC[S])
extends ModelCreator[RBFs[P], RBFNet[P]]
{
  lazy val predict = RBFPredictor[P](config)

  def features = data map(_.feature)

  lazy val targets = Col(data map(_.valueOrNaN) unwrap: _*)

  def weights(rbfs: RBFs[P]) = {
    val out = features.map(predict.rbfOut(_, rbfs.bf).unwrap.toArray).unwrap
    val phi = Mat(out: _*)
    val trans = phi.t
    val leftCoeff = trans * phi
    val rightCoeff = trans * targets
    pinv(leftCoeff) * rightCoeff
  }

  def run(est: Est[RBFs[P]]): RBFNet[P] = {
    val rbfs = est.params
    RBFNet(rbfs, weights(rbfs))
  }
}

case class RBFValidator[S: Sample, P: BasisFunction]
(data: Nel[S], config: RBFLearnConf[P])
(implicit mc: MC[S])
extends Validator[S, RBFNet[P], Double]
{
  lazy val predict = RBFPredictor[P](config)

  def verify(model: RBFNet[P])(sample: S): SampleValidation[S, Double] =
  {
    val pred = predict(sample, model)
    DSV(sample, pred.value)
  }

  def run(model: RBFNet[P]) = {
    val pred = data map(verify(model))
    Validation(pred)
  }
}

case class RBFModelSelectionValidator[S, P]
(cross: CrossValidator[S, RBFs[P], RBFNet[P], Double], cost: Func2)
extends ModelSelectionValidator[S, RBFs[P], RBFs[P], Double]

object RBF
{
  def msv[S: Sample, P: BasisFunction: Initializer: UpdateParams]
  (data: Nel[S], conf: RBFLearnConf[P], sconf: ModelSelectionConf)
  (implicit mc: MC[S])
  = {
    val stop = ParamDiffStopCriterion[RBFs[P]](sconf.steps, sconf.epsilon)
    lazy val validator =
      CrossValidator[S, RBFs[P], RBFNet[P], Double](data,
        sconf, RBFEstimator[S, P](_, conf, stop),
        RBFModelCreator[S, P](_, conf), RBFValidator[S, P](_, conf))
    RBFModelSelectionValidator(validator, conf.cost)
  }
}
