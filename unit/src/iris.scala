package tryp
package mi
package mlp
package unit

import atto._, Atto._
import atto.syntax.stream.all._

import scalaz.stream._

import breeze.linalg.{DenseVector, DenseMatrix, Transpose}

import spire.implicits._

case class Iris(feature: Col, name: String)
{
  lazy val cls = name

  lazy val value: Double = Iris.values.get(name).getOrElse(-1.0)
}

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
    } yield Iris(DenseVector(f: _*), n)
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
    load match {
      case Vector(head, tail @ _*) => Nel(head, tail: _*)
      case _ => sys.error("no data")
    }
  }
}

trait IrisInstances
{
  implicit lazy val irisSample: Sample[Iris] =
    new Sample[Iris] {
      def feature(a: Iris) = a.feature
      def value(a: Iris) = a.value
    }
}
