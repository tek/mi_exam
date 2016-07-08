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

  val lambda = 0.1d

  // override def trials = Some(3)

  override def folds = 20

  val kernel: KernelFunc = LinearKernel

  lazy val conf = SVMLearnConf.default(lambda, kernel = kernel)

  override implicit def modelClasses =
    SVMIrisInstances.instance_ModelClasses_Iris(Iris.Setosa, Iris.Versicolor)

  lazy val msv = SVM.msv(data, conf, sconf)
}

class NormalIrisSpec
extends IrisSpec
with MSVSpec[Iris, SVM, Double]
{
  override val kernel = RBFKernel(.5d)
  // override val kernel = PolyKernel(2d, 1d)
}

class PlottedIrisSpec
extends PlottedIrisSpecBase[SVM, Double]
with IrisSpec
{
  override def estimationShape: Shape = Shape.Line
}
