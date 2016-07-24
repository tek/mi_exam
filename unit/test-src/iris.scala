package tryp
package mi

import viz._

trait IrisSpecBase[P, M, V]
extends MSVSpecBase[Iris, P, M, V]
{
  implicit def modelClasses = Iris.instance_ModelClasses_Iris

  lazy val data = Iris.loadNel
}
