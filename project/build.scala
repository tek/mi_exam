package tryp

import sbt._, Keys._

object Build
extends MultiBuild("mi_exam", deps = MiDeps)
{
  lazy val core = tdp("core")

  lazy val mlp = "mlp" << core

  lazy val rbf = "rbf" << core

  lazy val unit = ("unit" << mlp << rbf)
    .logback("tag" -> "mi")
    .settingsV(
      fork := true,
      javaOptions += {
        val datadir = (baseDirectory in ThisBuild).value / "data"
        s"-Ddatadir=$datadir"
      }
    )

  override def consoleImports = """
  import cats._, data._, syntax.all._, std.all._
  """
}
