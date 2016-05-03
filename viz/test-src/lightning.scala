package tryp
package mi
package viz

import java.nio.file._

import scala.collection.JavaConversions._

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._

import fs2._
import fs2.util._
import fs2.io.file._
import fs2.async.channel

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
  implicit def start = Local.strat

  def sessionName = "mi_spec"

  lazy val sessionDir =
    sys.props.get("tryp.mi.sessions")
      .map(Paths.get(_))
      .getOrElse(sys.error("session dir unset"))

  lazy val sessionFile = sessionDir.resolve(sessionName)

  lazy val lgn = Lightning.default

  lazy val sessions = lgn.sessions

  lazy val sessionId =
    Files.readAllLines(sessionFile).toList
      .headOption
      .getOrElse(sys.error("session id file empty"))

  lazy val session = Session(lgn.api, sessionId, sessionName)

  lazy val viz = session.viz

  val name = tryp.Random.string(10)
}

class CreateSessionSpec
extends LightningSpecBase
{
  def is = sequential ^ s2"""
  create session dir $create
  create the test session and save the id $main
  """

  def create = {
    Files.createDirectories(sessionDir)
    Files.deleteIfExists(sessionFile)
    Files.createFile(sessionFile)
    Files.isDirectory(sessionDir) must beTrue
  }

  def createSession =
    Stream.eval(sessions.create(sessionName))

  def sessionNameBytes: Pipe[Task, Session, Byte] =
    _
      .map(_.id)
      .map(_.toList.map(_.toByte))
      .flatMap(_.map(Stream.emit[Task, Byte](_)).foldK[Stream[Task, ?], Byte])

  def writeSession =
    channel.observe(createSession)(
      sessionNameBytes andThen writeAll[Task](sessionFile))

  def main = writeSession computes matchA[Session].name(sessionName)
}

class CreateVizSpec
extends LightningSpecBase
{
  def is = s2"""
  create and delete a visualization $main
  """

  lazy val res = for {
    v <- viz.findOrCreate("line", name)
    t1 <- viz.find(_.id == v.id)
    deleted <- viz.delete(p(v.id))
    t2 <- viz.find(_.id == v.id)
  } yield (t1.flatMap(_.description), deleted.description, t2)

  def main = res computes_== Tuple3(Some(name), Some(name), None)
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

  def line = 8.gen(tryp.Random.double())

  def data = Line(List(line, line, line))

  val op =
    for {
      _ <- viz.deleteAll
      v <- viz.findOrCreate("line", name)
      res <- viz.update(v, data)
      _ <- viz.delete(res.id)
    } yield res

  def test = op computes matchA[Visualization].description(beSome(name))
}
