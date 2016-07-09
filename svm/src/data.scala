package tryp
package mi
package svm

case class SVMLearnConf(lambda: Double, cost: Func2, kernel: KernelFunc)
{
  def eval(svm: SVM): SVMEval = kernel match {
    case LinearKernel => LinearEval(svm)
    case _ => KernelEval(svm, kernel)
  }
}

object SVMLearnConf
{
  def default(
    lambda: Double = 2.0,
    cost: Func2 = QuadraticError,
    kernel: KernelFunc = LinearKernel
  ) = {
    SVMLearnConf(lambda, cost, kernel)
  }
}

case class SVM(normal: Col, offset: Double, support: List[Col], cy: Col)

object SVM
{
  def msv[S: Sample]
  (data: Nel[S], conf: SVMLearnConf, sconf: ModelSelectionConf) = {
    lazy val validator = CrossValidator[S, SVM, SVM, Double](data,
      sconf, SVMEstimator[S](_, conf), _ => IdModelCreator[SVM](),
      SVMValidator[S](_, conf))
    SVMModelSelectionValidator(validator, conf.cost)
  }
}
