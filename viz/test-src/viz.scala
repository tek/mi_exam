package tryp
package mi
package viz

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._

import fs2._
import fs2.util._
import Step._
import Stream.Handle

import spire.math._
import spire.algebra._
import spire.implicits._
import spire.random._

import breeze.linalg._
import breeze.numerics._
import breeze.linalg.functions.euclideanDistance
import breeze.plot._

import org.specs2.matcher.MatcherMacros._

trait LightningSpecBase
extends Spec
{
  def sessionName = "mi_spec"

  def sessionId = "c0930eaa-0c4f-4e2a-9b6e-7dbf8c4c1dee"

  lazy val lgn = Lightning.default

  lazy val session = lgn.session(sessionId)

  lazy val viz = session.viz

  val name = tryp.Random.string(10)
}

class CreateSessionSpec
extends LightningSpecBase
{
  def is = s2"""
  create the test session named '$sessionName' $main
  """

  def main =
    Lightning.default.createSession(sessionName) computes matchA[SessionCodec]
}

class CreateVizSpec
extends LightningSpecBase
{
  def is = s2"""
  create and delete a visualization $main
  """

  def main = {
    val res = for {
      v <- viz.findOrCreate("line", name)
      t1 <- viz.find(_.id == v.id)
      deleted <- viz.delete(p(v.id))
      t2 <- viz.find(_.id == v.id)
    } yield (t1.flatMap(_.description), deleted.description, t2)
    res computes_== Tuple3(Some(name), Some(name), None)
  }
}

class DeleteAllVizSpec
extends LightningSpecBase
{
  def is = s2"""
  delete all visualizations $main
  """

  lazy val res = for {
    vs1 <- viz.all
    _ <- vs1.map(_.id).map(viz.delete).sequence
    vs2 <- viz.all
  } yield vs2

  def main = res computes beEmpty
}

class PlotVizSpec
extends LightningSpecBase
{
  def is = s2"""
  test $test
  """

  val op =
    for {
      vs1 <- viz.all
      _ <- vs1.map(_.id).map(viz.delete).sequence
      v <- viz.findOrCreate("line", name)
      res <- viz.update(v, Line(List(List(1, 2, 3, 4, 5))))
    } yield res

  def test = op computes haveClass[Any]
}
