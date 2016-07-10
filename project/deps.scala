package tryp

import sbt._
import Keys._

object MiDeps
extends tryp.Deps
{
  val specsV = "3.8"

  def testScoped = ids(
    "org.specs2" %% "specs2-cats" % specsV,
    "org.specs2" %% "specs2-matcher-extra" % specsV
  )

  override def deps = super.deps ++ Map(
    "macro-console" -> (macroConsole ++ core ++ viz)
  )

  def pulsar(pro: String) = {
    dd("tryp" %% s"pulsar-$pro" % "0.1-SNAPSHOT", "tek/pulsar", pro)
  }

  lazy val imp = ids(
    "org.spire-math" %% "imp" % "0.+" % "provided"
  )

  lazy val core = ids(
    pulsar("jvm"),
    "org.scalanlp" %% "breeze-natives" % "0.+",
    "org.scalanlp" %% "breeze-viz" % "0.+",
    "org.spire-math" %% "spire" % "0.+",
    "org.tpolecat" %% "atto-core" % "0.5.0-SNAPSHOT",
    "org.tpolecat" %% "atto-compat-cats04" % "0.5.0-SNAPSHOT"
  ) ++ imp

  lazy val viz = testScoped ++ ids(
    "org.http4s" %% "http4s-blaze-client" % "0.14.0a-SNAPSHOT",
    "org.http4s" %% "http4s-circe" % "0.14.0a-SNAPSHOT",
    "io.circe" %% "circe-core" % "0.+",
    "io.circe" %% "circe-generic" % "0.+",
    "io.circe" %% "circe-parser" % "0.+",
    pulsar("unit-core"),
    "com.github.wookietreiber" %% "scala-chart" % "0.+"
  )

  lazy val unit = ids(
    "org.specs2" %% "specs2-scalacheck" % specsV,
    pulsar("unit-core")
  )

  lazy val macroConsole = ids(
    pulsar("core")
  )
}
