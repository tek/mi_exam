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
  implicit def instance_CreateEstimator_SVM[S: Sample: MC]
  : CreateEstimator[S, SVM, SVMLearnConf] =
    new CreateEstimator[S, SVM, SVMLearnConf] {
      def apply(data: Nel[S])
      (implicit conf: SVMLearnConf, sconf: MSConf)
      : Estimator[SVM] =
        SVMEstimator(data, conf)
    }

  implicit def instance_CreateModelCreator_SVM[S] =
    CreateModelCreator.id[S, SVM, SVMLearnConf]

  implicit def instance_CreateValidator_SVM[S: Sample: MC]
  : CreateValidator[S, SVM, SVMLearnConf] =
    new CreateValidator[S, SVM, SVMLearnConf] {
      def apply(data: Nel[S])
      (implicit conf: SVMLearnConf, sconf: MSConf)
      : Validator[SVM] = SVMValidator(data, conf)
    }
}
