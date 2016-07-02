package tryp
package mi

import org.specs2.ScalaCheck
import org.specs2.matcher.MatchResult

import org.scalacheck._
import Prop._

import viz._

trait Check[A <: RandomConf]
extends Spec
with ScalaCheck
{
  import GenBase._

  def is = s2"""
  randomly generated gaussian clusters ${forAllNoShrink(dataGen)(check)}
  """

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

  def mkSample(conf: RandomConf, d: Nel[ClassData], r: Double) = {
    implicit val mc = mkModelClasses(d)
    new DataSample {
      def data = d
      def featureCount = conf.rank
      override def range = 2 * r
    }
  }

  def mkSamplePlotting(data: RandomConf, max: Double)
  (implicit sample: Sample[Data]): SamplePlotting[Data] =
    new SamplePlotting[Data] {
      lazy val range = (-max, max)

      lazy val ranges = plotCount.gen(range)

      lazy val plotCount = data.rank.min(4)

      lazy val projections =
        (0 until plotCount).map(i => i -> (i + 1) % plotCount).toList
    }

  def dataGen: Gen[A]

  def result(classData: A, classes: Nel[ClassData], data: Nel[Data])
  (implicit sample: Sample[Data]): MatchResult[_]

  def range: Double

  def check(classData: A) = {
    val conf = classData.classes
    val classes = conf.map(createClass)
    val data = classes.flatMap(_.data)
    implicit val sample = mkSample(classData, classes, range)
    implicit val samplePlotting = mkSamplePlotting(classData, range)
    result(classData, classes, data)
  }
}

abstract class PlottedCheck[B <: RandomConf, A, P, O]
extends Check[B]
with viz.PlottedSpecHelpers[A, P, O]
