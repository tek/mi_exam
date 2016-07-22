package tryp
package mi
package kmeans

import breeze.linalg._
import breeze.numerics._
import breeze.linalg.functions.euclideanDistance

case class Dat(feature: Col, cls: ModelClass[Dat])

object Dat
{
  case object LU extends AutoClass[Dat]
  case object RU extends AutoClass[Dat]
  case object ML extends AutoClass[Dat]

  val values = Map[ModelClass[Dat], Col](
    LU -> Col(1d, 1d),
    RU -> Col(3d, 1d),
  )

  implicit val datColSample: Sample[Dat] =
    new Sample[Dat] {
      type Pred = Col

      def cls(a: Dat) = a.cls

      def feature(a: Dat) = a.feature

      def featureCount = 2

      def predictedClass(a: Dat) = {
        val x = feature(a)
        Dat.values.minBy { case (k, v) => norm(v - x) }
      }
    }

    implicit def instance_ModelClasses_Dat: ModelClasses[Dat, Col] =
      new ModelClasses[Dat, Col] {
        def value(a: ModelClass[Dat]) =
          Validated.fromOption(Dat.values.get(a), s"no value for $a")

        lazy val classes = Nel(LU: ModelClass[Dat], RU, ML)
      }
}

trait InternalBase
extends Spec
{
  val conf: KMeansLearnConf

  def bias = false

  def data: Nel[Dat]

  def sample = data.head

  def x = sample.feature

  lazy val stop = StepCountStopCriterion[KMeans](1)

  lazy val est = KMeansEstimator(data, conf, stop)

  lazy val initP = est.initialParams

  lazy val assign = est.assign

  lazy val move = est.move

  lazy val predict = KMeansPredictor(conf)

  lazy val creator = KMeansModelCreator(data, conf)

  lazy val validator = KMeansValidator[Dat](data, conf)

  lazy val step1 = est.addCenter(initP)

  lazy val step2 = step1 flatMap est.assign.apply

  lazy val step3 = step2 flatMap est.move.apply

  lazy val step4 = step3 flatMap est.addCenter.apply

  lazy val step5 = step4 flatMap est.assign.apply

  lazy val step6 = step5 flatMap est.move.apply

  lazy val f1 = sample.feature(0)
}

class StepSpec
extends InternalBase
{
  def is = s2"""
  first center ${center(step1, 0) must beValid(firstCenter)}
  first assignment ${firstAssignment}
  second center ${center(step4, 0) must beValid(beClose(firstCenter))}
  second assignment ${secondAssignment}
  second move ${center(step6, 0) must beValid(c2)}
  """

  import Dat._

  val steps = 1

  def eta = 1d

  def lambda = 2d

  lazy val c1 = Col(1d, 1d)

  lazy val c2 = Col(3d, 1d)

  lazy val data = Nel(Dat(c1, LU), Dat(c2, RU))

  lazy val conf = KMeansLearnConf.default()

  def center(param: Vali[KMeans], i: Int) =
    param flatMap (_.centers.lift(i).toValidatedNel("no centers in model"))

  def firstCenter = Col(2d, 1d)

  def firstAssignment =
    step2.map(_.assignment) must beValid(be_==(List(Mat(c2, c1))))

  def secondAssignment =
    step5.map(_.assignment) must beValid(be_==(List(Mat(c2), Mat(c1))))
}
