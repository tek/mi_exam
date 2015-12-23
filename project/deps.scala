package tryp
import sbt._
import Keys._

object MiDeps
extends tryp.Deps
{
  override def deps = super.deps ++ Map(
    "core" → core
  )

  lazy val core = ids(
    dd("tryp" %% "pulsar-jvm" % "+", "tek/pulsar", "jvm"),
    "org.scalanlp" %% "breeze-natives" % "+",
    "org.scalanlp" %% "breeze-viz" % "+",
    "org.spire-math" %% "spire" % "+"
  )

  override lazy val unit = super.unit ++ ids(
    dd("tryp" %% "pulsar-unit" % "+", "tek/pulsar", "unit")
  )
}
