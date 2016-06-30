package tryp

import sbt._, Keys._

object Build
extends MultiBuild("mi_exam", deps = MiDeps)
{
  lazy val core = tdp("core")
    .logback("tag" -> "mi")

  lazy val mlp = "mlp" << core

  lazy val rbf = "rbf" << core

  lazy val svm = "svm" << core

  lazy val viz = "viz" << core

  def blas =
    "-Dcom.github.fommil.netlib.BLAS=com.github.fommil.netlib.NativeRefBLAS"

  lazy val unit = ("unit" << mlp << rbf << viz << svm)
    .settingsV(
      fork := true,
      javaOptions += {
        val datadir = (baseDirectory in ThisBuild).value / "data"
        s"-Ddatadir=$datadir"
      },
      javaOptions += blas
    )

  override def consoleImports = """
  import cats._, data._, syntax.all._, std.all._
  import breeze._, linalg._
  import tryp._
  """
}
