package tryp
package mi
package svm
package unit

import org.specs2.matcher.MatchResult

import viz.Shape

trait SVMRandomSpecBase
extends Check[SVMData]
with MSVSpecBase[Data, SVM, Double]
{
  val kernel: KernelFunc = LinearKernel

  def trainSvm(classData: SVMData, msv: MSV, margin: Double)
  (implicit sample: Sample[Data]): MatchResult[_]

  def result(classData: SVMData, classes: Nel[ClassData], data: Nel[Data])
  (implicit sample: Sample[Data]) = {
    val lconf = SVMLearnConf.default(lambda = 0.5d, kernel = kernel)
    val msv = SVM.msv(data.shuffle, lconf, sconf)
    val margin = 0.2d * classData.rank * (trials | data.length)
    trainSvm(classData, msv, margin)
  }
}

class LinearRandomSpec
extends SVMRandomSpecBase
{
  // override def numTests = 1

  // override def trials = 3.some

  lazy val dataGen = SVMGen.linearSvm(10, Range(folds * 5, folds * 10))

  // override val kernel: KernelFunc = PolyKernel(2d, 1d)

  def trainSvm(classData: SVMData, msv: MSV, margin: Double)
  (implicit sample: Sample[Data]) =
    train(msv, margin)
}

class PlottedRandomSpec
extends PlottedCheck[SVMData, Data, SVM, Double]
with SVMRandomSpecBase
{

  override def estimationShape: Shape = Shape.Line

  override def numTests = 1

  override def trials = 1.some

  def lambda = 0.0005d

  // lazy val dataGen = SVMGen.linearSvm(10, Range(folds * 5, folds * 10))
  lazy val dataGen = SVMGen.linearSvm(2, Range(4, 5))

  def trainSvm(classData: SVMData, msv: MSV, margin: Double)
  (implicit sample: Sample[Data]) = {
    implicit val sp = mkSamplePlotting(classData)
    trainPms(mkPms(msv), margin)
  }
}
