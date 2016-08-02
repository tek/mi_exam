package tryp
package mi
package rbf

import scalaz.std.vector.{vectorInstance => zVectorInstance}

import breeze._
import linalg._
import functions._
import numerics._

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
