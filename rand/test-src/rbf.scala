package tryp
package mi
package rbf

import breeze.linalg._
import breeze.numerics._
import breeze.linalg.functions.euclideanDistance
import breeze.stats.distributions.MultivariateGaussian

import org.specs2.scalacheck._

class RandomSpec
extends SimpleCheckSpec[RBFData, RBFs[GaussBF]]
{
  import GenBase._

  override def epsilon = 1e-15d

  lazy val dataGen = RBFGen.rbf(5, 5, Range(folds * 5, folds * 10))

  override val trials = Some(1)
}
