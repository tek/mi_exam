package tryp
package mi
package rbf

import scalaz.std.vector.{vectorInstance => zVectorInstance}

import breeze._
import linalg._
import functions.euclideanDistance
import numerics._

import LearnConf._

import BasisFunction.ops._

@tc abstract class UpdateParams[P: BasisFunction]
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
extends Validator[RBFNet[P]]
{
  lazy val predict = RBFPredictor[P](config)

  def verify(model: RBFNet[P])(sample: S): SampleValidation =
  {
    val pred = predict(sample, model)
    DSV(sample, pred.value)
  }

  def run(model: RBFNet[P]) = {
    val pred = data map(verify(model))
    Val(pred)
  }
}
