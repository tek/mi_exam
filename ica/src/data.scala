package tryp
package mi
package ica

case class ICALearnConf(cost: Func2, kernel: KernelFunc)

object ICALearnConf
{
  def default(
    cost: Func2 = QuadraticError,
    kernel: KernelFunc = LinearKernel
  ) = {
    ICALearnConf(cost, kernel)
  }
}

case class ICA(unmix: Mat)

object ICA
{
  implicit def instance_CreateEstimator_ICA[S: Sample: MC]
  : CreateEstimator[S, ICA, ICALearnConf] =
    new CreateEstimator[S, ICA, ICALearnConf] {
      def apply(data: Nel[S])
      (implicit conf: ICALearnConf, sconf: MSConf)
      : Estimator[ICA] = ICAEstimator(data, conf)
    }

  implicit def instance_CreateModelCreator_ICA[S] =
    CreateModelCreator.id[S, ICA, ICALearnConf]

  implicit def instance_CreateValidator_ICA[S: Sample: MC]
  : CreateValidator[S, ICA, ICALearnConf] =
    new CreateValidator[S, ICA, ICALearnConf] {
      def apply(data: Nel[S])
      (implicit conf: ICALearnConf, sconf: MSConf)
      : Validator[ICA] = ICAValidator(data, conf)
    }
}
