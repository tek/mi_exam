package tryp
package mi

import org.specs2.ScalaCheck
import org.specs2.matcher.MatchResult
import org.specs2.scalacheck._

import org.scalacheck._
import Prop._

import viz._
import GenData.ops._

trait DataModelClasses[V]
{
  def apply(d: Nel[ClassData]): ModelClasses[Data, V]
}

object DataModelClasses
{
  implicit def instance_DataModelClasses_Double: DataModelClasses[Double] =
    new DataModelClasses[Double] {
      def apply(d: Nel[ClassData]) =
        new ModelClasses[Data, Double] {
          lazy val classes = d.map(_.label: ModelClass[Data])

          lazy val classMap = d.map(a => a.num -> a.label).unwrap.toMap

          def value(a: ModelClass[Data]) = a match {
            case DataClass(n) =>
              val v = classMap.get(n).map(_.num.toDouble)
              Validated.fromOption(v, s"no class for $a")
            case _ => Validated.invalid(s"invalid model class $a")
          }
        }
    }

  implicit def instance_DataModelClasses_Col: DataModelClasses[Col] =
    new DataModelClasses[Col] {
      def apply(d: Nel[ClassData]) =
        new ModelClasses[Data, Col] {
          lazy val classes = d.map(_.label: ModelClass[Data])

          lazy val classMap = d.map(a => a.num -> a.label).unwrap.toMap

          def value(a: ModelClass[Data]) = a match {
            case DataClass(n) =>
              val v = d.find(_.num == n).map(_.clusters.head.mean)
              Validated.fromOption(v, s"no class for $a")
            case _ => Validated.invalid(s"invalid model class $a")
          }
        }
    }

  def create[V](d: Nel[ClassData])(implicit dmc: DataModelClasses[V]) =
    dmc(d)
}

case class CheckData[A: GenData](conf: A, classes: Nel[ClassData],
  data: Nel[Data])
  {
    lazy val genData = GenData[A]
  }

abstract class Check[A: GenData](val cd: CheckData[A])
{
  def conf = cd.conf

  def classes = cd.classes

  def data = cd.data

  lazy val genData = GenData[A]

  implicit lazy val sample = {
    new DataSample {
      def data = classes
      def featureCount = conf.rank
      override def range = 2 * genData.sampleRange
    }
  }

  def result: MatchResult[_]
}

abstract class CheckSpec[A: GenData]
extends Spec
with ScalaCheck
{
  import GenBase._

  def is = s2"""
  randomly generated gaussian clusters ${forAllNoShrink(dataGen)(check)}
  """

  lazy val genData = GenData[A]

  def numTests = 15

  implicit lazy val params = Parameters(minTestsOk = numTests)

  def dataGen: Gen[A]

  def mkCheck(cd: CheckData[A]): Check[A]

  def createClasses(clusters: Nel[ClassCluster[_]]): Nel[ClassData] =
    genData.createClasses(clusters.groupBy(_.num))

  def check(conf: A) = {
    val classes = createClasses(conf.classes)
    val data = classes.flatMap(_.data)
    val chk = mkCheck(CheckData(conf, classes, data))
    chk.result
  }
}

trait MSVGen[A, P, M, V, C]
{
  type Create = CreateMSV[Data, P, M, C]

  def msv(cd: CheckData[A])(sc: MSConf)
  (implicit mc: ModelClasses[Data, V], s: Sample[Data])
  : MSV[Data, P] = {
    implicit val sc2 = sconf(cd, sc)
    implicit val lc = lconf(cd, sc2)
    implicit val cm = createMSV
    MSV.create[Data, P, M, C](cd.data.shuffle)
  }

  def createMSV(implicit s: Sample[Data], mc: ModelClasses[Data, V]): Create

  def lconf(cd: CheckData[A], sc: MSConf)(implicit s: Sample[Data]): C

  def sconf(cd: CheckData[A], sc: MSConf): MSConf = sc

  def margin(cd: CheckData[A]): Double
}

class MSVCheck[A: GenData, P, M, V: DataModelClasses, C]
(cd: CheckData[A], override implicit val sconf: MSConf)
(implicit msv: MSVGen[A, P, M, V, C])
extends Check[A](cd)
with MSVSpecBase[Data, P, M, V]
{
  def is = ???

  implicit val mc = DataModelClasses.create[V](classes)

  def result = train(msv.msv(cd)(sconf), msv.margin(cd))
}

abstract class MSVCheckSpec[A: GenData, P, M, V: DataModelClasses, C]
(implicit msv: MSVGen[A, P, M, V, C])
extends CheckSpec
with MSVSpecBase[Data, P, M, V]
{
  def mkCheck(cd: CheckData[A]) = new MSVCheck[A, P, M, V, C](cd, sconf)
}

abstract class SimpleCheckSpec[A: GenData, M, C]
(implicit msv: MSVGen[A, M, M, Double, C])
extends MSVCheckSpec[A, M, M, Double, C]

class PlottedCheck[A: GenData, P: JParam, M, V: DataModelClasses, C]
(cd: CheckData[A], sconf: MSConf, override val estimationShape: Shape)
(implicit msv: MSVGen[A, P, M, V, C])
extends MSVCheck[A, P, M, V, C](cd, sconf)
with viz.PlottedSpecHelpers[Data, P, M, V]
{
  implicit lazy val sampleViz: SampleVizData[Data] =
    new SampleVizData[Data] {
      lazy val range = (-genData.domainRange, genData.domainRange)

      lazy val ranges = plotCount.gen(range)

      lazy val plotCount = conf.rank.min(4)

      lazy val projections =
        (0 until plotCount - 1).map(i => i -> ((i + 1) % plotCount)).toList
    }

  implicit val plotBE = JFree.instance_Viz_JFree[Data, P]

  override def result = trainPms(mkPms(msv.msv(cd)(sconf)), msv.margin(cd))
}

abstract class PlottedCheckSpec
[A: GenData, P: JParam, M, V: DataModelClasses, C]
(implicit msv: MSVGen[A, P, M, V, C])
extends CheckSpec
with MSVSpecBase[Data, P, M, V]
{
  def estimationShape: Shape = Shape.Scatter

  def mkCheck(cd: CheckData[A]) =
    new PlottedCheck[A, P, M, V, C](cd, sconf, estimationShape)
}
