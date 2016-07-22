package tryp
package mi
package pca
package unit

import org.specs2.matcher.MatchResult

import viz.Shape

trait PCARandomSpecBase
extends MSVCheck[PCAData, PCA, PCA, Double]
{
  val kernel: KernelFunc = LinearKernel

  def margin(sd: PCAData) = 0.2 * sd.rank

  def msv(classes: Nel[ClassData], data: Nel[Data])
  (implicit mc: MC[Data], sample: Sample[Data]) = {
    val lconf = PCALearnConf.default(kernel = kernel)
    PCA.msv(data.shuffle, lconf, sconf)
  }
}

class LinearRandomSpec
extends PCARandomSpecBase
{
  // override def numTests = 1

  // override def trials = 3.some

  lazy val dataGen = PCAGen.pca(2, Range(folds * 5, folds * 10))

  def trainPca(classData: PCAData, msv: MSV, margin: Double)
  (implicit sample: Sample[Data]) =
    train(msv, margin)
}

class RBFRandomSpec
extends LinearRandomSpec
{
  override val kernel: KernelFunc = RBFKernel(2d)

  override lazy val dataGen = PCAGen.rings(2, 2, Range(4, 5))
}

class PlottedRandomSpec
extends PlottedCheck[PCAData, PCA, PCA, Double]
with PCARandomSpecBase
{
  override def estimationShape: Shape = Shape.Line

  override def numTests = 1

  // override def trials = 1.some

  def lambda = 0.00005d

  lazy val dataGen = PCAGen.pca(10, Range(folds * 5, folds * 10))
}
