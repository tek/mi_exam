package tryp
package mi
package kmeans

import breeze._
import linalg._
import numerics._
import operators._

case class KMeansLearnConf(cost: Func2, kernel: KernelFunc)

object KMeansLearnConf
{
  def default(
    cost: Func2 = QuadraticError,
    kernel: KernelFunc = LinearKernel
  ) = {
    KMeansLearnConf(cost, kernel)
  }
}

case class KMeans(centers: List[Col], assignment: List[Mat])
{
  lazy val diffs = centers zip assignment map { case (a, b) => b(*, ::) - a }

  lazy val variances = diffs map { cluster =>
    cluster.rowCols.map(a => a dot a).sum / cluster.rows
  }
}

object KMeans
{
  def msv[S: Sample]
  (data: Nel[S], conf: KMeansLearnConf, sconf: ModelSelectionConf)
  (implicit mc: MC[S])
  : MSV[S, KMeans, KMeans, Col]
  = {
    val stop = EmpiricalErrorStopCriterion[KMeans](sconf.steps, sconf.epsilon)
    lazy val validator = CrossValidator[S, KMeans, KMeans, Col](data,
      sconf, KMeansEstimator[S](_, conf, stop), _ => IdModelCreator[KMeans](),
      KMeansValidator[S](_, conf))
    KMeansModelSelectionValidator[S](validator, conf.cost)
  }

  implicit def instance_EmpiricalError_KMeans: EmpiricalError[KMeans] =
    new EmpiricalError[KMeans] {
      def value(a: KMeans) = {
        sum(a.variances) / a.variances.length
      }
    }
}
