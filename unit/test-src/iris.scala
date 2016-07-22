package tryp
package mi

import viz._

trait IrisSpecBase[P, V]
extends MSVSpecBase[Iris, P, V]
{
  implicit def modelClasses = Iris.instance_ModelClasses_Iris

  lazy val data = Iris.loadNel
}
