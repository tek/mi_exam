package tryp
import sbt._
import Keys._

object MiDeps
extends tryp.Deps
{
  override def deps = super.deps ++ Map(
    "core" → core,
    "macro-console" → (macroConsole ++ core)
  )

  lazy val imp = ids(
    "org.spire-math" %% "imp" % "+" % "provided"
  )

  lazy val core = ids(
    dd("tryp" %% "pulsar-jvm" % "+", "tek/pulsar", "jvm"),
    "org.scalanlp" %% "breeze-natives" % "+",
    "org.scalanlp" %% "breeze-viz" % "+",
    "org.spire-math" %% "spire" % "+",
    "org.tpolecat" %% "atto-stream"  % "+"
  ) ++ imp

  override lazy val unit = super.unit ++ ids(
    "org.specs2" %% "specs2-scalacheck" % "+",
    dd("tryp" %% "pulsar-unit-core" % "+", "tek/pulsar", "unit-core")
  ) ++ imp

  def macroConsole = ids(
    dd("tryp" %% "pulsar-core" % "+", "tek/pulsar", "core")
  )
}
