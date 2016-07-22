package tryp
package mi
package kmeans

import scala.collection.immutable.SortedMap

import cats.data.Validated._

import breeze._
import linalg._
import numerics._
import math._
import stats._
import util._
import functions._

import IntImplToDouble._

case class AddCenterStep(data: Mat)
extends EstimationStep[KMeans]
{
  def rank = data.cols

  def dataMean: Col = mean(data(::, *)).t

  def displacement = Col.rand(rank) * 1e-3

  def findCenter(param: KMeans) = {
    val i = param.variances.argmax
    param.centers.lift(i).toValidatedNel("no centers to choose")
      .map(_ + displacement)
  }

  def apply(param: KMeans) = {
    val newCenter =
      if (param.centers.isEmpty) dataMean.validNel[String]
      else findCenter(param)
    newCenter map (a => KMeans(a :: param.centers, param.assignment))
  }
}

case class AssignCentersStep(data: List[Col])
extends EstimationStep[KMeans]
{
  def apply(param: KMeans) = {
    val assMap = data.foldLeft(SortedMap[Int, List[Col]]()) {
      (z, x) =>
        val i = param.centers.map(euclideanDistance(_, x)).argmin
        val old = z.getOrElse(i, Nil)
        z + (i -> (x :: old))
    }
    val assignment = assMap.valuesIterator.map(a => Mat(a: _*)).toList
    KMeans(param.centers, assignment).validNel[String]
  }
}

case class MoveCentersStep(data: List[Col])
extends EstimationStep[KMeans]
{
  def apply(param: KMeans) = {
    val centers = param.assignment map (a => sum(a(::, *)).t / a.rows)
    param.copy(centers = centers).validNel[String]
  }
}

case class KMeansEstimator[S: Sample]
(data: Nel[S], config: KMeansLearnConf, stop: StopCriterion[KMeans])
extends IterativeEstimator[KMeans]
{
  lazy val cols = data.map(_.feature).unwrap

  lazy val mat = Mat(cols: _*)

  lazy val initialParams = KMeans(Nil, List(mat))

  lazy val addCenter = AddCenterStep(mat)

  lazy val assign = AssignCentersStep(cols)

  lazy val move = MoveCentersStep(cols)

  lazy val steps =
    Nel[EstimationStep[KMeans]](addCenter, assign, move)
}

case class KMeansModelCreator[S: Sample]
(data: Nel[S], config: KMeansLearnConf)
extends ModelCreator[KMeans, KMeans]
{
  lazy val predict = KMeansPredictor(config)

  def run(est: Est[KMeans]): KMeans = {
    est.params
  }
}
