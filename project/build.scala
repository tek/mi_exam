package tryp

import sbt._, Keys._

object Build
extends MultiBuild("mi_exam", deps = MiDeps)
{
  lazy val core = tdp("core")

  lazy val unit = "unit" << core
}
