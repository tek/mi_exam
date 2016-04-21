package tryp
package mi
package rbf

import spire.math._
import spire.algebra._
import spire.implicits._
import spire.random._

import breeze.linalg._
import breeze.numerics._
import breeze.linalg.functions.euclideanDistance

case class Dat(feature: Col, name: String)

object Dat
{
  val values = Map(
    "lu" → 0.3,
    "ru" → 0.6,
    "ml" → 0.9
  )

  implicit val datSample: Sample[Dat] =
    new Sample[Dat] {
      def cls(a: Dat) = LabeledClass(a.name)

      lazy val classes = Dat.values map {
        case (n, v) => v -> LabeledClass(n)
      }

      def feature(a: Dat) = a.feature

      def value(a: Dat) = Dat.values.get(a.name).getOrElse(-1.0)
    }
}

trait InternalBase
extends Spec
{

  type BF = GaussBF

  val sample: Dat

  val conf: RBFLearnConf[BF]

  def bias = false

  lazy val train = RBFEstimator(Nel(sample), conf)

  def step = train.step

  lazy val initW = train.initialParams

  lazy val f1 = sample.feature(0)

  lazy val pred = step.predict(sample, train.initialParams)

  def optimizer = step.optimize

  def forward = pred.pred
}

class StepSpec
extends InternalBase
{
  def is = s2"""
  closest $closest
  update center internal $updateCenterInternal
  updateCenter $updateCenter
  updateBf $updateBf
  update weights $updateWeights
  """

  val centroids = 3

  val steps = 1

  def eta = 1d

  def lambda = 2d

  lazy val sample = Dat(Col(-1, -1), "ml")

  def x = sample.feature

  lazy val weights = Col(0.3, 0.3, 0.3)

  lazy val c1 = Col(1d, 1d)

  lazy val c2 = Col(-1d, 1d)

  lazy val c3 = Col(-1d, -2d)

  lazy val s1 = 1d

  lazy val s2 = 2d

  lazy val s3 = 1d

  lazy val bf = Nev(GaussBF(c1, s1), GaussBF(c2, s2), GaussBF(c3, s3))

  lazy val params = Params(weights, bf)

  lazy val initialization = new ManualInitialization(params)

  lazy val conf =
    RBFLearnConf.default[BF, Dat](steps, centroids, eta, lambda,
      LearnConf.Batch, initialization = Some(initialization))

  lazy val newC = c3 + (x - c3) * eta

  lazy val d1 = lambda * euclideanDistance(c1, c2)

  lazy val d2 = lambda * euclideanDistance(c2, c1)

  lazy val d3 = lambda * euclideanDistance(newC, c2)

  lazy val updatedBf = step.updateBf(params.bf)

  def closest = step.closest(x, params.bf.map(_.center))._1 must_== c3

  def updateCenterInternal = {
    val s = 4d
    val a = GaussBF(Col(1d, 2d), s)
    val diff = Col(2d, 2d)
    UpdateParams[GaussBF].updateCenter(0, diff)(Nev(a)).head must_==
      GaussBF(Col(3d, 4d), s)
  }

  def updateCenter = {
    step.updateCenter(params.bf, x) must_==
      Nev(GaussBF(c1, s1), GaussBF(c2, s2), GaussBF(newC, s3))
  }

  def updateBf = updatedBf must_==
      Nev(GaussBF(c1, d1), GaussBF(c2, d2), GaussBF(newC, d3))

  def updateWeights = {
    step.updateWeights(updatedBf) must_== Params(Col(0.5, 0.0, 0.5), updatedBf)
  }
}
