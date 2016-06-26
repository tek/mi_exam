package tryp
package mi
package rbf

import breeze.linalg._
import breeze.numerics._
import breeze.linalg.functions.euclideanDistance
import breeze.stats.distributions.MultivariateGaussian

import org.specs2.ScalaCheck
import org.specs2.scalacheck._
import org.scalacheck._
import Prop._
import Arbitrary.arbitrary

class RandomSpec
extends Check
{
  implicit lazy val params = Parameters(minTestsOk = 1)

  def is = s2"""
  randomly generated gaussian clusters ${forAllNoShrink(dataGen)(check)}
  """

  import RBFGen._

  val folds = 10

  lazy val dataGen: Gen[Nel[ClassConf]] =
    rbf(5, 5, Range(folds * 5, folds * 10))

  def mkSample(d: Nel[DataClass]) =
    new DataSample {
      def data = d.unwrap
      def featureCount = d.head.conf.features
    override def range = 2 * RBFGen.range
    }

  val trials = Some(1)

  val sconf = ModelSelectionConf.default(folds = folds, trials = trials,
    epsilon = 1e-15d)

  def check(conf: Nel[ClassConf]) = {
    val classes = conf.map(createClass)
    val data = classes.flatMap(_.data)
    implicit val sample: Sample[Data] = mkSample(classes)
    val lconf =
      RBFLearnConf.default[GaussBF, Data](rbfs = classes.length, eta = 0.5d)
    val result = RBF.msv(data.shuffle, lconf, sconf)
    result.printer.short()
    val margin = 1e-5d * (trials | data.length)
    result.unsafeValidation.totalError must be_<=(margin)
  }
}
