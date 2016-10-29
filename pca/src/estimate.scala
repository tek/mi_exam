package tryp
package mi
package pca

import cats.data.Validated._

import breeze.linalg._
import breeze.numerics._

trait PCAEval
{
  def apply(x: Mat, n: Double): Mat
}

object LinearEval
extends PCAEval
{
  def apply(x: Mat, n: Double): Mat = (x.t * x) / (n - 1)
}

case class KernelEval(kernel: KernelFunc)
extends PCAEval
{
  def kerneled(x: Mat) = {
    val rank = x.rows
    val feat = x.rowCols
    Mat.create(rank, rank, feat.map2(feat)(kernel.apply).toArray)
  }

  def apply(x: Mat, n: Double): Mat = {
    val k = kerneled(x)
    val i = Mat.fill(x.rows, x.rows)(1d / n)
    k - (i * k) - (k * i) + (i * k * i)
  }
}

case class PCAEstimator[S: Sample]
(data: Nel[S], config: PCALearnConf)
extends SimpleEstimator[PCA]
{
  lazy val n = data.length.toDouble

  lazy val x = Mat(data.map(_.feature).tail: _*)

  lazy val µ = (sum(x(::, *)) / n).t

  lazy val centered = x(*, ::) - µ

  lazy val covariance = config.cov(centered, n)

  lazy val eigen = {
    val eigSym.EigSym(l, v) = eigSym(covariance)
    val eigenSorted = l.data.toList zip v.rowCols sortBy (- _._1)
    eigenSorted.unzip
  }

  lazy val allEigenvalues = eigen._1

  lazy val allEigenvectors = eigen._2

  lazy val energies = accumulate(allEigenvalues.toCol).data.toList

  lazy val lowestEnergy = energies.lastOption | NaN

  lazy val energyRatios = energies map (_ / lowestEnergy)

  lazy val cutoff = energyRatios.indexWhere(_ >= 0.9d) + 1

  lazy val λ = allEigenvalues take cutoff

  lazy val pcs = allEigenvectors take cutoff

  lazy val go = Valid(PCA(pcs, µ, λ))
}
