package tryp
package mi
package svm
package unit

import viz.Shape

trait SVMRandomSpecBase
extends SimpleCheckSpec[SVMData, SVM, SVMLearnConf]

class LinearRandomSpec
extends SVMRandomSpecBase
{
  // override def numTests = 1

  // override def trials = 3.some

  lazy val dataGen = SVMGen.linearSvm(10, Range(folds * 5, folds * 10))

  // override val kernel: KernelFunc = PolyKernel(2d, 1d)
}

class PolyRandomSpec
extends SVMRandomSpecBase
{
  def lambda = 1d

  override def numTests = 1

  val kernel: KernelFunc = PolyKernel(7d, 1d)
  // override val kernel: KernelFunc = RBFKernel(1d)

  override lazy val dataGen =
    SVMGen.threeClusterPolySvm(3, Range(folds * 5, folds * 10), kernel)
}

class PlottedRandomSpec
extends PlottedCheckSpec[SVMData, SVM, SVM, Double, SVMLearnConf]
{
  override def estimationShape: Shape = Shape.Line

  override def numTests = 1

  override def trials = 2.some

  def lambda = 0.00005d

  lazy val dataGen = SVMGen.linearSvm(10, Range(folds * 5, folds * 10))
}
