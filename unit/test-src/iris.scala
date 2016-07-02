package tryp
package mi

import viz._

trait IrisSpecBase[P, O]
extends MSVSpecBase[Iris, P, O]
{
  implicit def modelClasses = Iris.instance_ModelClasses_Iris

  lazy val data = Iris.loadNel
}
