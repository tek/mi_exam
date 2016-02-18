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

  lazy val core = ids(
    dd("tryp" %% "pulsar-jvm" % "+", "tek/pulsar", "jvm"),
    "org.scalanlp" %% "breeze-natives" % "+",
    "org.scalanlp" %% "breeze-viz" % "+",
    "org.spire-math" %% "spire" % "+",
    "org.spire-math" %% "imp" % "+" % "provided",
    "org.tpolecat" %% "atto-stream"  % "+"
  )

  override lazy val unit = super.unit ++ ids(
    dd("tryp" %% "pulsar-unit-core" % "+", "tek/pulsar", "unit-core")
  )

  def macroConsole = ids(
    dd("tryp" %% "pulsar-core" % "+", "tek/pulsar", "core")
  )
}
