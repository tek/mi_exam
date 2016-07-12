package tryp
package mi

import org.specs2.ScalaCheck
import org.specs2.matcher.MatchResult
import org.specs2.scalacheck._

import org.scalacheck._
import Prop._

import viz._

abstract class Check[A <: RandomConf: GenData]
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

  def mkModelClasses(d: Nel[ClassData]) =
    new ModelClasses[Data] {
      lazy val classes = d.map(_.label: ModelClass[Data])

      lazy val classMap = d.map(a => a.num -> a.label).unwrap.toMap
      def value(a: ModelClass[Data]) = a match {
        case DataClass(n) =>
          val v = classMap.get(n).map(_.num.toDouble)
          Validated.fromOption(v, s"no class for $a")
        case _ => Validated.invalid(s"invalid model class $a")
      }
    }

  def mkSample(conf: RandomConf, d: Nel[ClassData]) = {
    implicit val mc = mkModelClasses(d)
    new DataSample {
      def data = d
      def featureCount = conf.rank
      override def range = 2 * genData.sampleRange
    }
  }

  def mkSampleViz(data: RandomConf)
  (implicit sample: Sample[Data]): SampleVizData[Data] =
    new SampleVizData[Data] {
      lazy val range = (-genData.domainRange, genData.domainRange)

      lazy val ranges = plotCount.gen(range)

      lazy val plotCount = data.rank.min(4)

      lazy val projections =
        (0 until plotCount - 1).map(i => i -> (i + 1) % plotCount).toList
    }

  def dataGen: Gen[A]

  def result(conf: A, classes: Nel[ClassData], data: Nel[Data])
  (implicit sample: Sample[Data]): MatchResult[_]

  def createClasses(clusters: Nel[ClassCluster]): Nel[ClassData] =
    clusters
      .groupBy(_.num)
      .map(_.map(createClass))
      .map(_.reduceLeft((a, b) =>
          ClassData(a.clusters.combine(b.clusters), a.data.combine(b.data)))
      )

  def check(conf: A) = {
    val classConf = conf.classes
    val classes = createClasses(classConf)
    val data = classes.flatMap(_.data)
    implicit val sample = mkSample(conf, classes)
    implicit val sampleViz = mkSampleViz(conf)
    result(conf, classes, data)
  }
}

abstract class PlottedCheck[B <: RandomConf: GenData, A, P, O]
extends Check[B]
with viz.PlottedSpecHelpers[A, P, O]
