package tryp
package mi
package rbf

import breeze.linalg._
import breeze.numerics._
import breeze.linalg.functions.euclideanDistance
import breeze.stats.distributions.MultivariateGaussian

import org.specs2.scalacheck._

class RandomSpec
extends SimpleCheck[RBFData, RBFs[GaussBF]]
{
  import GenBase._

  lazy val dataGen = RBFGen.rbf(5, 5, Range(folds * 5, folds * 10))

  override val trials = Some(1)

  override def epsilon = 1e-15d

  def margin(sd: RBFData) = 1e-5d

  def msv(classes: Nel[ClassData], data: Nel[Data])
  (implicit mc: MC[Data], sample: Sample[Data]) = {
    val lconf =
      RBFLearnConf.default[GaussBF, Data](rbfs = classes.length, eta = 1d)
    RBF.msv(data.shuffle, lconf, sconf)
  }
}
