package tryp
package mi
package kmeans

import breeze._
import linalg._
import numerics._
import operators._

case class KMeansLearnConf(cost: Func2, kernel: KernelFunc,
  stop: StopCriterion[KMeans])

object KMeansLearnConf
{
  def default(
    cost: Func2 = QuadraticError,
    kernel: KernelFunc = LinearKernel,
    stop: StopCriterion[KMeans] = StepCountStopCriterion(1)
  ) = {
    KMeansLearnConf(cost, kernel, stop)
  }

  def ee(
    sconf: MSConf,
    cost: Func2 = QuadraticError,
    kernel: KernelFunc = LinearKernel
  ) = {
    val stop = EmpiricalErrorStopCriterion[KMeans](sconf.steps, sconf.epsilon)
    KMeansLearnConf(cost, kernel, stop)
  }
}

case class KMeans(centers: List[Col], assignment: List[Mat]
  )
{
  lazy val diffs = centers zip assignment map { case (a, b) => b(*, ::) - a }

  lazy val variances = diffs map (_.rowCols) map KMeans.setVariance

  def rank = centers.headOption map (_.length)
}

object KMeans
{
  def setVariance(set: List[Col]) =
    set.map(a => a dot a).sum / set.length

  implicit def instance_CreateEstimator_KMeans[S: Sample: MC]
  : CreateEstimator[S, KMeans, KMeansLearnConf] =
    new CreateEstimator[S, KMeans, KMeansLearnConf] {
      def apply(data: Nel[S])
      (implicit conf: KMeansLearnConf, sconf: MSConf)
      : Estimator[KMeans] = {
        KMeansEstimator(data, conf, conf.stop)
      }
    }

  implicit def instance_CreateModelCreator_KMeans[S] =
    CreateModelCreator.id[S, KMeans, KMeansLearnConf]

  implicit def instance_CreateValidator_KMeans[S: Sample: MC]
  : CreateValidator[S, KMeans, KMeansLearnConf] =
    new CreateValidator[S, KMeans, KMeansLearnConf] {
      def apply(data: Nel[S])
      (implicit conf: KMeansLearnConf, sconf: MSConf)
      : Validator[KMeans] = KMeansValidator(data, conf)
    }

  implicit def instance_EmpiricalError_KMeans: EmpiricalError[KMeans] =
    new EmpiricalError[KMeans] {
      def calc(rank: Int, model: KMeans) = {
        val count = model.variances.length
        sum(model.variances) / (rank * count)
      }

      def value(model: KMeans) = {
        (model.rank map (calc(_, model))) | pinf
      }
    }
}
