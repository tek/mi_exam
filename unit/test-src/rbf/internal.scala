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

case class Dat(feature: Col, cls: ModelClass[Dat])

object Dat
{
  case object LU extends AutoClass[Dat]
  case object RU extends AutoClass[Dat]
  case object ML extends AutoClass[Dat]

  val values = Map[ModelClass[Dat], Double](
    LU -> 0.3,
    RU -> 0.6,
    ML -> 0.9,
  )

  implicit val datSample: Sample[Dat] =
    new Sample[Dat] {
      def cls(a: Dat) = a.cls

      def feature(a: Dat) = a.feature

      def featureCount = 2
    }

    implicit def instance_ModelClasses_Dat: ModelClasses[Dat] =
      new ModelClasses[Dat] {
        def value(a: ModelClass[Dat]) =
          Validated.fromOption(Dat.values.get(a), s"no value for $a")

        lazy val classes = Nel(LU: ModelClass[Dat], RU, ML)
      }
}

trait InternalBase
extends Spec
{
  type BF = GaussBF

  val sample: Dat

  val conf: RBFLearnConf[BF]

  def bias = false

  lazy val data = Nel(sample)

  lazy val stop = StepCountStopCriterion[RBFs[BF]](1)

  lazy val train = RBFEstimator(data, conf, stop)

  def step = train.step

  lazy val predict = RBFPredictor[BF](conf)

  lazy val creator = RBFModelCreator(data, conf)

  lazy val validator = RBFValidator[Dat, BF](data, conf)

  lazy val initP = train.initialParams

  lazy val kmeans = KMeans(Nel(sample.feature), conf, initP)

  lazy val f1 = sample.feature(0)

  lazy val pred = predict(sample, RBFNet(initP, Linalg.randWeightCol(1)))

  def forward = pred.value
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

  import Dat._

  val rbfs = 3

  val steps = 1

  def eta = 1d

  def lambda = 2d

  lazy val sample = Dat(Col(-1, -1), ML)

  def x = sample.feature

  lazy val weights = Col(0.3, 0.3, 0.3)

  lazy val c1 = Col(1d, 1d)

  lazy val c2 = Col(-1d, 1d)

  lazy val c3 = Col(-1d, -2d)

  lazy val s1 = 1d

  lazy val s2 = 2d

  lazy val s3 = 1d

  lazy val bf = RBFs(Nev(GaussBF(c1, s1), GaussBF(c2, s2), GaussBF(c3, s3)))

  lazy val params = RBFNet(bf, weights)

  lazy val init = new ManualInitialization(bf)

  lazy val conf = RBFLearnConf.default[BF, Dat](rbfs, eta, lambda, Some(init))

  lazy val newC = c3 + (x - c3) * eta

  lazy val d1 = lambda * euclideanDistance(c1, c2)

  lazy val d2 = lambda * euclideanDistance(c2, c1)

  lazy val d3 = lambda * euclideanDistance(newC, c2)

  lazy val updatedBf = step(bf)

  def closest = kmeans.closest(x, params.bf.map(_.center))._1 must_== c3

  def updateCenterInternal = {
    val s = 4d
    val a = GaussBF(Col(1d, 2d), s)
    val diff = Col(2d, 2d)
    UpdateParams[GaussBF].updateCenter(0, diff)(RBFs(Nev(a))).bf.head must_==
      GaussBF(Col(3d, 4d), s)
  }

  def updateCenter = {
    kmeans.updateCenter(params.rbfs, x) must_==
      RBFs(Nev(GaussBF(c1, s1), GaussBF(c2, s2), GaussBF(newC, s3)))
  }

  def updateBf = updatedBf must_==
      RBFs(Nev(GaussBF(c1, d1), GaussBF(c2, d2), GaussBF(newC, d3)))

  def updateWeights = {
    creator.weights(updatedBf) must_== Col(0.5, 0.0, 0.5)
  }
}
