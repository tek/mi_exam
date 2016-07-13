package tryp
package mi
package rbf

import breeze.linalg._
import breeze.numerics._
import breeze.linalg.functions.euclideanDistance
import breeze.stats.distributions.MultivariateGaussian

import org.specs2.scalacheck._

class RandomSpec
extends Check[RBFData]
with MSVSpecBase[Data, RBFs[GaussBF], Double]
{
  import GenBase._

  lazy val dataGen = RBFGen.rbf(5, 5, Range(folds * 5, folds * 10))

  override val trials = Some(1)

  override def epsilon = 1e-15d

  def result(classData: RBFData, classes: Nel[ClassData], data: Nel[Data])
  (implicit sample: Sample[Data]) = {
    val margin = 1e-5d * (trials | data.length)
    val lconf =
      RBFLearnConf.default[GaussBF, Data](rbfs = classes.length, eta = 1d)
    val msv = RBF.msv(data.shuffle, lconf, sconf)
    train(msv, margin)
  }
}
