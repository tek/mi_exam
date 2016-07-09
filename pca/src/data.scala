package tryp
package mi
package pca

case class PCALearnConf(cost: Func2, kernel: KernelFunc)

object PCALearnConf
{
  def default(
    cost: Func2 = QuadraticError,
    kernel: KernelFunc = LinearKernel
  ) = {
    PCALearnConf(cost, kernel)
  }
}

case class PCA(basis: Mat)

object PCA
{
  def msv[S: Sample]
  (data: Nel[S], conf: PCALearnConf, sconf: ModelSelectionConf) = {
    lazy val validator = CrossValidator[S, PCA, PCA, Double](data,
      sconf, PCAEstimator[S](_, conf), _ => IdModelCreator[PCA](),
      PCAValidator[S](_, conf))
    PCAModelSelectionValidator(validator, conf.cost)
  }
}
