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

abstract class Check[A: GenData]
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

  def mkSample(conf: A, d: Nel[ClassData]) = {
    new DataSample {
      def data = d
      def featureCount = conf.rank
      override def range = 2 * genData.sampleRange
    }
  }

  def mkSampleViz(data: A)
  (implicit mc: ModelClassesBase[Data], sample: Sample[Data])
  : SampleVizData[Data] =
    new SampleVizData[Data] {
      lazy val range = (-genData.domainRange, genData.domainRange)

      lazy val ranges = plotCount.gen(range)

      lazy val plotCount = data.rank.min(4)

      lazy val projections =
        (0 until plotCount - 1).map(i => i -> ((i + 1) % plotCount)).toList
    }

  def dataGen: Gen[A]

  def result(conf: A, classes: Nel[ClassData], data: Nel[Data])
  (implicit sample: Sample[Data]): MatchResult[_]

  def createClasses(clusters: Nel[ClassCluster[_]]): Nel[ClassData] =
    genData.createClasses(clusters.groupBy(_.num))

  def check(conf: A) = {
    val classConf = conf.classes
    val classes = createClasses(classConf)
    val data = classes.flatMap(_.data)
    implicit val sample = mkSample(conf, classes)
    result(conf, classes, data)
  }
}

abstract class MSVCheck[A: GenData, P, M, V: DataModelClasses]
extends Check[A]
with MSVSpecBase[Data, P, M, V]
{
  def msv(classes: Nel[ClassData], data: Nel[Data])
  (implicit mc: ModelClasses[Data, V], sample: Sample[Data]): MSV

  def margin(conf: A): Double

  def trainImpl(conf: A, msv: MSV, margin: Double)
  (implicit mc: ModelClasses[Data, V], sample: Sample[Data]): MatchResult[_] =
    train(msv, margin)

  def result(conf: A, classes: Nel[ClassData], data: Nel[Data])
  (implicit sample: Sample[Data]) = {
    implicit val mc = DataModelClasses.create(classes)
    trainImpl(conf, msv(classes, data), margin(conf))
  }
}

abstract class SimpleCheck[A: GenData, M]
extends MSVCheck[A, M, M, Double]

abstract class PlottedCheck[A: GenData, P: JParam, M, V: DataModelClasses]
extends MSVCheck[A, P, M, V]
with viz.PlottedSpecHelpers[Data, P, M, V]
{
  override def trainImpl(conf: A, msv: MSV, margin: Double)
  (implicit mc: ModelClasses[Data, V], sample: Sample[Data]) = {
    implicit val sp = mkSampleViz(conf)
    implicit val plotBE = JFree.instance_Viz_JFree[Data, P]
    trainPms(mkPms(msv), margin)
  }
}
