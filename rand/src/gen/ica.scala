package tryp
package mi
package ica

import breeze.numerics._
import breeze.linalg._
import functions.euclideanDistance

import org.scalacheck.Gen

case class ICAData(rank: Int, classes: Nel[ClassCluster[_]])

object ICAGen
extends GenBase[ICAData]
{
  import Gen._
  import GenBase._

  import genData._

  def signals(range: Double, samples: Double) = {
    val space = Col(0d until samples by 1d: _*)
    val domain = 8 * Math.PI * space / samples
    val sine = sin(domain)
    val square = signum(sine) * .75d
    val saw = domain map (a => ((a % 4d) / 2d) - 1d)
    Mat(space, sine, square, saw).t.rowCols
  }
}

object ICAData
extends ICADataInstances
{
  lazy val genData = GenData[ICAData]
}

trait ICADataInstances
{
  implicit lazy val instance_GenData_ICAData: GenData[ICAData] =
    new GenData[ICAData] {
      def rank(a: ICAData) = a.rank

      def classes(a: ICAData) = a.classes

      def sampleRange: Double = 10d

      def domainRange = 10d
    }
}
