package tryp
package mi
package svm

import breeze.linalg._
import breeze.numerics._
import breeze.optimize.proximal.{QuadraticMinimizer, ProjectBox, ProjectPos}

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

case class SVMEstimator[S: Sample]
(data: Nel[S], config: SVMLearnConf)
(implicit mc: MC[S])
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

  lazy val go: Vali[SVM] =
    offset.map(b => SVM(w, b, supports.toList map (_.feature), supportCy))
      .toValidatedNel
}
