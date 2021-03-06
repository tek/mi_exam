package tryp
package mi
package svm
package unit

import viz._

object SVMIrisInstances
{
  implicit def instance_ModelClasses_Iris
  (cls1: ModelClass[Iris], cls2: ModelClass[Iris]): ModelClasses[Iris, Double] =
    new ModelClasses[Iris, Double] {
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
extends IrisSpecBase[SVM, SVM, Double]
{
  def title = "Support Vector Machine"

  val lambda = 0.1d

  // override def trials = Some(3)

  override def folds = 20

  val kernel: KernelFunc = LinearKernel

  lazy val conf = SVMLearnConf.default(lambda, kernel = kernel)

  override implicit def modelClasses =
    SVMIrisInstances.instance_ModelClasses_Iris(Iris.Setosa, Iris.Versicolor)
}

class NormalIrisSpec
extends SimpleMSVSpec[Iris, SVM, SVMLearnConf]
with IrisSpec
{
  override val kernel = RBFKernel(.5d)
  // override val kernel = PolyKernel(2d, 1d)
}

class PlottedIrisSpec
extends PlottedIrisSpecBase[SVM, SVM, Double, SVMLearnConf]
with IrisSpec
{
  override def estimationShape: Shape = Shape.Line
}
