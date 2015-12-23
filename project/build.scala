package tryp

import sbt._
import Keys._

object MiExamProject
extends MultiBuild("mi-exam", deps = MiDeps)
{
  lazy val core = "core" / "core"

  lazy val unit = "unit" << core
}
