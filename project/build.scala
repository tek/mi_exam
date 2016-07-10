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

  lazy val pca = "pca" << core

  lazy val viz = "viz" << core

  def blas =
    "-Dcom.github.fommil.netlib.BLAS=com.github.fommil.netlib.NativeRefBLAS"

  def test = List(
      fork := true,
      javaOptions += blas
    )

  lazy val unit = ("unit" << mlp << rbf << viz << svm << pca)
    .settings(test)
    .settingsV(
      javaOptions += {
        val datadir = (baseDirectory in ThisBuild).value / "data"
        s"-Ddatadir=$datadir"
      }
    )

  lazy val rand = ("rand" << rbf << svm << pca << unit)
    .settings(test)

  override def consoleImports = """
  import cats._, data._, syntax.all._, std.all._
  import breeze._, linalg._, numerics._
  import tryp._, mi._
  """
}
