package tryp
package mi
package pca

import cats.data.Validated._

import breeze.linalg._
import breeze.numerics._

case class PCAEstimator[S: Sample]
(data: Nel[S], config: PCALearnConf)
extends SimpleEstimator[PCA]
{
  lazy val n = data.length.toDouble

  lazy val x = Mat(data.map(_.feature).unwrap: _*)

  lazy val xm = (sum(x(::, *)) / n).t

  lazy val centered = x(*, ::) - xm

  lazy val covariance = (centered.t * centered) / (n - 1)

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

  lazy val go = Valid(PCA(Mat(1d)))
}
