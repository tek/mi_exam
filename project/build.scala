package tryp

import sbt._, Keys._

object Build
extends MultiBuild("mi_exam", deps = MiDeps)
{
  def blas =
    "-Dcom.github.fommil.netlib.BLAS=com.github.fommil.netlib.NativeRefBLAS"

  override def defaultBuilder =
    super.defaultBuilder(_).settingsV(javaOptions += blas, fork := true)

  lazy val core = "core".logback("tag" -> "mi")

  lazy val mlp = "mlp" << core

  lazy val rbf = "rbf" << core

  lazy val svm = "svm" << core

  lazy val pca = "pca" << core

  lazy val viz = "viz" << core

  lazy val unit = ("unit" << mlp << rbf << viz << svm << pca)
    .settingsV(
      javaOptions += {
        val datadir = (baseDirectory in ThisBuild).value / "data"
        s"-Ddatadir=$datadir"
      }
    )

  lazy val rand = ("rand" << rbf << svm << pca << unit)

  override def consoleImports = """
  import cats._, data._, syntax.all._, std.all._
  import breeze._, linalg._, numerics._
  import tryp._, mi._
  """
}
