package tryp
package mi

import cats.data.NonEmptyList

import atto._, Atto._
import atto.compat.NonEmptyListy

import scalaz.stream._

import spire.implicits._

import viz.SampleVizData

case class Iris(feature: Col, cls: ModelClass[Iris])

object Iris
extends IrisInstances
{
  object Setosa extends AutoClass[Iris]
  object Versicolor extends AutoClass[Iris]
  object Virginica extends AutoClass[Iris]

  implicit def instance_NonEmptyListy_Nel: NonEmptyListy[Nel] =
    new NonEmptyListy[Nel] {
      def cons[A](a: A, as: List[A]) = NonEmptyList(a, as)
      def toList[A](a: Nel[A]) = a.toList
    }

  val classes = Map(
    "setosa" -> Setosa,
    "versicolor" -> Versicolor,
    "virginica" -> Virginica,
  )

  lazy val parser: Parser[Option[Iris]] = {
    import atto.parser._
    val feat = numeric.double <~ char(',')
    for {
      f ← manyN[Nel, Double](4, feat)
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

  private[this] def toNel(data: Vector[Iris]) =
    data match {
      case Vector(head, tail @ _*) => Nel(head, tail: _*)
      case _ => sys.error("no data")
    }

  private[this] def forClasses[V](mc: ModelClasses[Iris, V]) =
    all.filter(a => mc.classes.exists(_ == a.cls))

  def loadNel[V](implicit mc: ModelClasses[Iris, V]) =
    toNel(forClasses(mc))

  def loadNelRandom[V](implicit mc: ModelClasses[Iris, V]) =
    toNel(util.Random.shuffle(forClasses(mc)))
}

trait IrisInstances
{
  implicit def irisSample: Sample[Iris] =
    new Sample[Iris] {
      import Iris._
      def cls(a: Iris) = a.cls

      def feature(a: Iris) = a.feature

      override def range = 10d

      def featureCount = 4
    }

  implicit lazy val instance_SampleVizData_Iris: SampleVizData[Iris] =
    new SampleVizData[Iris] {
      def ranges = List((0d, 10d), (0d, 5d), (0d, 10d), (0d, 5d))

      def plotCount = 4

      def projections = List((0, 1), (1, 2), (2, 3), (3, 0))
    }

    implicit def instance_ModelClasses_Iris: ModelClasses[Iris, Double] =
      new ModelClasses[Iris, Double] {
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
