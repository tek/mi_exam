package tryp
package mi
package unit

import breeze.numerics._
import breeze.linalg._

import pca._

class MiscSpec
extends CheckSpec[PCAData]
{
  import GenBase._

  override def numTests = 1

  lazy val dataGen = PCAGen.rings(2, 2, Range(4, 5), LinearKernel)

  def mkCheck(cd: CheckData[PCAData]) =
    new Check[PCAData](cd) {
      def result = {
        cd.conf.classes.map { g =>
          g.gen() map { v =>
          }
        }
        1 must_== 1
      }
    }
}
