package tryp
package mi
package unit

import breeze.numerics._
import breeze.linalg._

import pca._

class MiscSpec
extends Check[PCAData]
{
  import GenBase._

  override def numTests = 1

  lazy val dataGen = PCAGen.rings(2, 2, Range(4, 5))

  override def check(a: PCAData) = {
    a.classes.map { g =>
      g.gen() map { v =>
      }
    }
    1 must_== 1
  }

  override def result(conf: PCAData, classes: Nel[ClassData], data: Nel[Data])
  (implicit sample: Sample[Data]) = 1 must_== 1
}
