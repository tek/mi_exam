package tryp
package mi

import atto._, Atto._
import atto.compat.cats._

import scalaz.stream._

import spire.implicits._

import viz.SamplePlotting

case class Iris(feature: Col, name: String)

object Iris
extends IrisInstances
{
  val values = Map(
    "setosa" → 0.3,
    "versicolor" → 0.6,
    "virginica" → 0.9
  )

  lazy val parser: Parser[Iris] = {
    import atto.parser._
    val feat = numeric.double <~ char(',')
    for {
      f ← manyN(4, feat)
      _ ← string("Iris-")
      n ← takeText
    } yield Iris(Col(f: _*), n)
  }

  lazy val datadir = sys.props.get("datadir").getOrElse(".")

  lazy val load = {
    io.linesR(s"$datadir/iris")
      .runLog
      .unsafePerformSyncAttempt
      .getOrElse(Vector())
      .flatMap(Iris.parser.parseOnly(_).option)
  }

  def loadNel = {
    util.Random.shuffle(load) match {
      case Vector(head, tail @ _*) => Nel(head, tail: _*)
      case _ => sys.error("no data")
    }
  }
}

trait IrisInstances
{
  implicit lazy val irisSample: Sample[Iris] =
    new Sample[Iris] {
      def cls(a: Iris) = LabeledClass(a.name)

      lazy val classes = Iris.values map {
        case (n, v) => v -> LabeledClass(n)
      }

      def feature(a: Iris) = a.feature

      def value(a: Iris) = Iris.values.get(a.name).getOrElse(-1.0)

      override def range = 10d

      def featureCount = 4
    }

  implicit lazy val instance_SamplePlotting_Iris: SamplePlotting[Iris] =
    new SamplePlotting[Iris] {
      def ranges = List((0d, 10d), (0d, 5d), (0d, 10d), (0d, 5d))

      def plotCount = 4

      def projections = List((0, 1), (1, 2), (2, 3), (3, 0))

      def projectionRanges = projections.map {
        case (x, y) => ranges(x) -> ranges(y)
      }

      def plots(data: List[Col], size: Array[Double]) = {
        projections map {
          case (a, b) =>
            Array(data.map(_(a)).toArray, data.map(_(b)).toArray, size)
        }
      }
    }
}
