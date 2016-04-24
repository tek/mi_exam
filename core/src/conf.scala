package tryp
package mi

object LearnConf
{
  sealed trait LearnMode

  case object Batch
  extends LearnMode

  case object Online
  extends LearnMode
}
