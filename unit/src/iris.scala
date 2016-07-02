package tryp
package mi

import atto._, Atto._
import atto.compat.cats._

import scalaz.stream._

import spire.implicits._

import viz.SamplePlotting

case class Iris(feature: Col, cls: ModelClass[Iris])

object Iris
extends IrisInstances
{
  object Setosa extends AutoClass[Iris]
  object Versicolor extends AutoClass[Iris]
  object Virginica extends AutoClass[Iris]

  val classes = Map(
    "setosa" -> Setosa,
    "versicolor" -> Versicolor,
    "virginica" -> Virginica,
  )

  lazy val parser: Parser[Option[Iris]] = {
    import atto.parser._
    val feat = numeric.double <~ char(',')
    for {
      f ← manyN(4, feat)
      _ ← string("Iris-")
      n ← takeText
      cls = classes.get(n)
    } yield cls.map(Iris(Col(f: _*), _))
  }

  lazy val datadir = sys.props.get("datadir").getOrElse(".")

  lazy val all: Vector[Iris] = {
    io.linesR(s"$datadir/iris")
      .runLog
      .unsafePerformSyncAttempt
      .getOrElse(Vector())
      .flatMap(Iris.parser.parseOnly(_).option)
      .flatten
  }

  def loadNel(implicit mc: ModelClasses[Iris]) = {
    val targets = all.filter(a => mc.classes.contains(a.cls))
    util.Random.shuffle(targets) match {
      case Vector(head, tail @ _*) => Nel(head, tail: _*)
      case _ => sys.error("no data")
    }
  }
}

trait IrisInstances
{
  implicit def irisSample(implicit mc: ModelClasses[Iris]): Sample[Iris] =
    new Sample[Iris]()(mc) {
      import Iris._
      def cls(a: Iris) = a.cls

      def feature(a: Iris) = a.feature

      override def range = 10d

      def featureCount = 4
    }

  implicit lazy val instance_SamplePlotting_Iris: SamplePlotting[Iris] =
    new SamplePlotting[Iris] {
      def ranges = List((0d, 10d), (0d, 5d), (0d, 10d), (0d, 5d))

      def plotCount = 4

      def projections = List((0, 1), (1, 2), (2, 3), (3, 0))
    }

    implicit lazy val instance_ModelClasses_Iris: ModelClasses[Iris] =
      new ModelClasses[Iris] {
        import Iris._
        def classes = Nel(Setosa, Versicolor, Virginica)

        def value(a: ModelClass[Iris]) =
          a match {
            case Setosa => Validated.valid(0.3)
            case Versicolor => Validated.valid(0.6)
            case Virginica => Validated.valid(0.9)
            case _ => Validated.invalid(s"no class for $a")
          }
      }
}
