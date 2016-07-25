package tryp
package mi
package pca
package unit

import org.specs2.matcher.MatchResult

import viz.Shape

trait PCARandomSpecBase
extends SimpleCheckSpec[PCAData, PCA, PCALearnConf]
{
  val kernel: KernelFunc = LinearKernel
}

class LinearRandomSpec
extends PCARandomSpecBase
{
  // override def numTests = 1

  // override def trials = 3.some

  lazy val dataGen = PCAGen.pca(2, Range(folds * 5, folds * 10), kernel)
}

class RBFRandomSpec
extends LinearRandomSpec
{
  override val kernel: KernelFunc = RBFKernel(2d)

  override lazy val dataGen = PCAGen.rings(2, 2, Range(4, 5), kernel)
}

class PlottedRandomSpec
extends PlottedCheckSpec[PCAData, PCA, PCA, Double, PCALearnConf]
{
  val kernel: KernelFunc = LinearKernel

  override def estimationShape: Shape = Shape.Line

  override def numTests = 1

  // override def trials = 1.some

  def lambda = 0.00005d

  lazy val dataGen = PCAGen.pca(10, Range(folds * 5, folds * 10), kernel)
}
