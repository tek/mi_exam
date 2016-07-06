package tryp
package mi
package svm

import simulacrum._

import scalaz.std.vector.{vectorInstance => zVectorInstance}
import scalaz.syntax.zip._

import cats.data.Validated._

import breeze.linalg._
import breeze.linalg.functions.euclideanDistance
import breeze.linalg.operators.OpMulInner
import breeze.numerics._
import breeze.generic.UFunc
import breeze.optimize.proximal.{QuadraticMinimizer, ProjectBox, ProjectPos}

import LearnConf._

trait KernelFunc
{
  def apply(v1: Col, v2: Col): Double
}

object LinearKernel
extends KernelFunc
{
  def apply(v1: Col, v2: Col): Double = v1 dot v2
}

case class PolyKernel(degree: Double, bias: Double)
extends KernelFunc
{
  def apply(v1: Col, v2: Col): Double = pow(v1 dot v2 + bias, degree)
}

case class RBFKernel(sigma: Double)
extends KernelFunc
{
  val gamma = - 1 / (2 * sigma)

  def apply(v1: Col, v2: Col): Double = {
    val diff = v1 - v2
    exp(gamma * (diff dot diff))
  }
}

case class SVM(normal: Col, offset: Double, support: List[Col], cy: Col)

trait SVMEval
{
  def svm: SVM

  def apply(x: Col): Double

  def classify(x: Col): Double = signum(apply(x) - svm.offset)
}

case class LinearEval(svm: SVM)
extends SVMEval
{
  def apply(x: Col) = LinearEval.eval(svm.normal, x)
}

object LinearEval
{
  def eval(w: Col, x: Col) = w dot x
}

case class KernelEval(svm: SVM, k: KernelFunc)
extends SVMEval
{
  def apply(x: Col) = KernelEval.eval(k, svm.support, svm.cy, x)
}

object KernelEval
{
  def eval(k: KernelFunc, support: List[Col], cy: Col, x: Col) =
    cy dot support.map(k(_, x)).toCol
}

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

case class SVMPredictor(config: SVMLearnConf)
extends Predictor[SVM, Double]
{
  def apply[S: Sample](sample: S, model: SVM)
  : Prediction[S, SVM, Double] = {
    val pred = config.eval(model).classify(sample.feature)
    Prediction(sample, model, pred, Sample[S].predictedClass(pred))
  }
}

case class SVMEstimator[S: Sample]
(data: Nel[S], config: SVMLearnConf)
extends SimpleEstimator[SVM]
{
  lazy val x = Mat(data.map(_.feature).toList: _*)

  lazy val y = data.map(_.valueOrNaN).unwrap.toCol

  lazy val rank = data.length

  lazy val aeq = Mat(y)

  lazy val beq = Col(0d)

  lazy val lbv = Col.zeros[Double](rank)

  lazy val ub = 1d / (2d * config.lambda * rank)

  lazy val ubv = Col.fill(rank, ub)

  lazy val qm = new QuadraticMinimizer(rank, ProjectBox(lbv, ubv), aeq, beq)

  lazy val feat = data.toList.map(_.feature)

  def gramX =
    Mat.create(rank, rank, feat.map2(feat)(config.kernel.apply).toArray)

  lazy val spanY = y * y.t

  lazy val form = gramX :* spanY

  lazy val q = Col.ones[Double](rank)

  lazy val c = qm.minimize(form, -q)

  lazy val cy = c :* y

  lazy val w = sum(x(::, *) :* cy, Axis._0).t

  lazy val supportIndexes =
    c.toArray.toList.zipWithIndex.filter(_._1 != 0).map(_._2)

  lazy val supports = supportIndexes flatMap data.lift

  lazy val supportCols = supports.map(_.feature)

  lazy val support =
    Validated.fromOption(supports.headOption, "no support vectors found")

  lazy val supportCy = supportIndexes.map(cy(_)).toCol

  def eval(x: Col) = {
    config.kernel match {
      case LinearKernel => LinearEval.eval(w, x)
      case k => supportCy dot supportCols.map(k(_, x)).toCol
    }
  }

  lazy val offset =
    support flatMap (s => s.value.map(eval(s.feature) - _))

  def go =
    offset.map(b => SVM(w, b, supports.toList map (_.feature), supportCy))
      .toValidatedNel
}

case class SVMValidator[S: Sample]
(data: Nel[S], config: SVMLearnConf)
extends Validator[S, SVM, Double]
{
  lazy val predict = SVMPredictor(config)

  def verify(model: SVM)(sample: S): SampleValidation[S, Double] = {
    val pred = predict(sample, model)
    SV(sample, pred.value)
  }

  def run(model: SVM) = {
    val pred = data map(verify(model))
    Validation(pred)
  }
}

case class SVMModelSelectionValidator[S, P]
(cross: CrossValidator[S, SVM, SVM, Double], cost: Func2)
extends ModelSelectionValidator[S, SVM, Double]

object SVM
{
  def msv[S: Sample]
  (data: Nel[S], conf: SVMLearnConf, sconf: ModelSelectionConf) = {
    lazy val validator = CrossValidator[S, SVM, SVM, Double](data,
      sconf, SVMEstimator[S](_, conf), _ => IdModelCreator[SVM](),
      SVMValidator[S](_, conf))
    SVMModelSelectionValidator(validator, conf.cost)
  }
}
