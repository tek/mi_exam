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
  def msv[S: Sample]
  (data: Nel[S], conf: ICALearnConf, sconf: ModelSelectionConf)
  (implicit mc: MC[S])  
  = {
    lazy val validator = CrossValidator[S, ICA, ICA, Double](data,
      sconf, ICAEstimator[S](_, conf), _ => IdModelCreator[ICA](),
      ICAValidator[S](_, conf))
    ICAModelSelectionValidator(validator, conf.cost)
  }
}
