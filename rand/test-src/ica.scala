package tryp
package mi
package ica
package unit

import viz._

case class Sig(feature: Col, cls: ModelClass[Sig])

object ICASpec
{
  case object One extends AutoClass[Sig]
  case object Two extends AutoClass[Sig]
  case object Three extends AutoClass[Sig]

  def range = 100d

  implicit def instance_ModelClasses_Sig: ModelClasses[Sig, Double] =
    new ModelClasses[Sig, Double] {
      def classes = Nel[ModelClass[Sig]](One, Two, Three)
      def value(a: ModelClass[Sig]) = 0d.valid
    }

  implicit def instance_Sample_Sig: Sample[Sig] =
    new Sample[Sig] {
      def cls(a: Sig) = a.cls
      def feature(a: Sig) = a.feature
      def featureCount = 2
    }

  implicit def instance_SampleVizData_Sig: SampleVizData[Sig] =
    new SampleVizData[Sig] {
      def ranges = {
        val x = 0d -> range
        val y = -1d -> 1d
        List(x, y, y, y)
      }

      def plotCount = 2

      def projections = List(0 -> 1, 0 -> 2, 0 -> 3)
    }
}

class ICASpec
extends Spec
{
  import ICASpec._

  def is = s2"""
  go $go
  """

  implicit lazy val fconf =
    FigureConf.default("mi", width = 1000, height = 500)

  def go = {
    val s = ICAGen.signals(1d, range) map (Sig(_, One))
    val v = new JFreeSimpleViz[Sig]
    val a = v.init
    val t = for {
      _ <- v.setup(a)
      _ <- v.samples(a)(s)
      _ <- v.estimation(a)(s)
    } yield ()
    t.unsafeRun
    Thread.sleep(5000)
    1 === 1
  }
}
