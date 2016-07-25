package tryp
package mi
package pca

case class PCALearnConf(cost: Func2, kernel: KernelFunc)
{
  def cov: PCAEval = kernel match {
    case LinearKernel => LinearEval
    case _ => KernelEval(kernel)
  }
}

object PCALearnConf
{
  def default(
    cost: Func2 = QuadraticError,
    kernel: KernelFunc = LinearKernel
  ) = {
    PCALearnConf(cost, kernel)
  }
}

case class PCA(basis: List[Col], µ: Col, λ: List[Double])
{
  def scaledBasis = basis zip λ map { case (a, b) => a * b }
}

object PCA
{
  implicit def instance_CreateEstimator_PCA[S: Sample: MC]
  : CreateEstimator[S, PCA, PCALearnConf] =
    new CreateEstimator[S, PCA, PCALearnConf] {
      def apply(data: Nel[S])
      (implicit conf: PCALearnConf, sconf: MSConf)
      : Estimator[PCA] = PCAEstimator(data, conf)
    }

  implicit def instance_CreateModelCreator_PCA[S] =
    CreateModelCreator.id[S, PCA, PCALearnConf]

  implicit def instance_CreateValidator_PCA[S: Sample: MC]
  : CreateValidator[S, PCA, PCALearnConf] =
    new CreateValidator[S, PCA, PCALearnConf] {
      def apply(data: Nel[S])
      (implicit conf: PCALearnConf, sconf: MSConf)
      : Validator[PCA] = PCAValidator(data, conf)
    }
}
