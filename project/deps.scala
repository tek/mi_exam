package tryp
import sbt._
import Keys._

object MiDeps
extends tryp.Deps
{
  override def deps = super.deps ++ Map(
    "core" → core,
    "viz" → viz,
    "macro-console" → (macroConsole ++ core ++ viz)
  )

  lazy val imp = ids(
    "org.spire-math" %% "imp" % "+" % "provided"
  )

  lazy val core = ids(
    dd("tryp" %% "pulsar-jvm" % "+", "tek/pulsar", "jvm"),
    "org.scalanlp" %% "breeze-natives" % "+",
    "org.scalanlp" %% "breeze-viz" % "+",
    "org.spire-math" %% "spire" % "+",
    "org.tpolecat" %% "atto-stream"  % "+",
    "org.python" % "jython" % "+"
  ) ++ imp

  lazy val viz = commonTestIdsScoped ++ ids(
    "org.http4s" %% "http4s-blaze-client" % "0.14.0a-SNAPSHOT",
    "org.http4s" %% "http4s-circe" % "0.14.0a-SNAPSHOT",
    "io.circe" %% "circe-core" % "+",
    "io.circe" %% "circe-generic" % "+",
    "io.circe" %% "circe-parser" % "+",
    dd("tryp" %% "pulsar-unit-core" % "+" % "test", "tek/pulsar", "unit-core"),
    "com.github.wookietreiber" %% "scala-chart" % "latest.integration"
  )

  override val specsV = "3.7"

  override lazy val unit = super.unit ++ ids(
    dd("tryp" %% "pulsar-unit-core" % "+", "tek/pulsar", "unit-core")
  ) ++ imp

  def macroConsole = ids(
    dd("tryp" %% "pulsar-core" % "+", "tek/pulsar", "core")
  )
}
