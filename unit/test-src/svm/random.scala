package tryp
package mi
package svm
package unit

import breeze.linalg._
import breeze.numerics._
import breeze.linalg.functions.euclideanDistance
import breeze.stats.distributions.MultivariateGaussian

class RandomSpec
extends Check[SVMData]
with MSVSpecBase[Data, SVM, Double]
{
  import GenBase._
  import SVMGen._

  lazy val dataGen = svm(10, Range(folds * 5, folds * 10))

  def range = SVMGen.range

  def result(classData: SVMData, classes: Nel[ClassData], data: Nel[Data])
  (implicit sample: Sample[Data]) = {
    val lconf = SVMLearnConf.default(lambda = 0.5d)
    val msv = SVM.msv(data.shuffle, lconf, sconf)
    val margin = 2d * classData.rank * (trials | data.length)
    train(msv, margin)
  }
}

class PlottedRandomSpec
extends PlottedCheck[SVMData, Data, SVM, Double]
{
  import GenBase._
  import SVMGen._

  lazy val dataGen = svm(10, Range(folds * 5, folds * 10))

  def range = SVMGen.range

  def result(classData: SVMData, classes: Nel[ClassData], data: Nel[Data])
  (implicit sample: Sample[Data]) = {
    implicit val sp = mkSamplePlotting(classData, range)
    val lconf = SVMLearnConf.default(lambda = 0.5d)
    val msv: MSV = SVM.msv(data.shuffle, lconf, sconf)
    val margin = 2d * classData.rank * (trials | data.length)
    trainPms(mkPms(msv), margin)
  }
}
