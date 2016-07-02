package tryp
package mi
package svm
package unit

import viz._

object SVMIrisInstances
{
  implicit def instance_ModelClasses_Iris
  (cls1: ModelClass[Iris], cls2: ModelClass[Iris]): ModelClasses[Iris] =
    new ModelClasses[Iris] {
      import Iris._
      def classes = Nel(cls1, cls2)

      def value(a: ModelClass[Iris]) =
        a match {
          case `cls1` => Validated.valid(-1d)
          case `cls2` => Validated.valid(1d)
          case _ => Validated.invalid(s"no class for $a")
        }
    }
}

trait IrisSpec
extends IrisSpecBase[SVM, Double]
{
  def title = "Support Vector Machine"

  val lambda = 2d

  // override def trials = Some(1)

  lazy val conf = SVMLearnConf.default(lambda)

  override implicit def modelClasses =
    SVMIrisInstances.instance_ModelClasses_Iris(Iris.Setosa, Iris.Versicolor)

  lazy val msv = SVM.msv(data, conf, sconf)
}

class NormalIrisSpec
extends IrisSpec
with MSVSpec[Iris, SVM, Double]

class PlottedIrisSpec
extends PlottedIrisSpecBase[SVM, Double]
with IrisSpec
{
  override def estimationShape: Shape = Shape.Line
}
