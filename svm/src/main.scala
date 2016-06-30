package tryp
package mi
package svm

import simulacrum._

import scalaz.std.vector.{vectorInstance => zVectorInstance}
import scalaz.syntax.zip._

import breeze.linalg.{sum, squaredDistance, pinv, *, Axis}
import breeze.linalg.functions.euclideanDistance
import breeze.numerics.{abs, signum}
import breeze.generic.UFunc
import breeze.optimize.proximal.{QuadraticMinimizer, ProjectBox, ProjectPos}

import LearnConf._

case class SVM(normal: Col, support: List[Col], offset: Double)
{
  def classify(x: Col) = signum(normal dot x - offset)
}

case class SVMLearnConf(lambda: Double, cost: Func2)

object SVMLearnConf
{
  def default(
    lambda: Double = 2.0,
    cost: Func2 = QuadraticError
  ) = {
    SVMLearnConf(lambda, cost)
  }
}

case class SVMPredictor(config: SVMLearnConf)
extends Predictor[SVM, Double]
{
  def apply[S: Sample](sample: S, model: SVM)
  : Prediction[S, SVM, Double] = {
    Prediction(sample, model, model.classify(sample.feature))
  }
}

case class SVMEstimator[S: Sample]
(data: Nel[S], config: SVMLearnConf)
extends SimpleEstimator[SVM]
{
  lazy val x = Mat(data.map(_.feature).toList: _*)

  lazy val y = Col(data.map(_.valueOrNaN).toList: _*)

  lazy val rank = data.length

  lazy val aeq = Mat(y)

  lazy val beq = Col(0d)

  lazy val lbv = Col.zeros[Double](rank)

  lazy val ub = 1d / (2d * config.lambda * rank)

  lazy val ubv = Col.fill(rank, ub)

  lazy val qm = new QuadraticMinimizer(rank, ProjectBox(lbv, ubv), aeq, beq)

  lazy val feat = data.toList.map(_.feature)

  lazy val xdot = Mat.create(rank, rank, feat.map2(feat)(_ dot _).toArray)

  lazy val yg = y * y.t

  lazy val gram = xdot :* yg

  lazy val q = Col.ones[Double](rank)

  lazy val c = qm.minimize(gram, -q)

  lazy val cy = c :* y

  lazy val w = sum(x(::, *) :* cy, Axis._0).t

  lazy val supportIndexes = c.toArray.zipWithIndex.filter(_._1 != 0).map(_._2)

  lazy val supports = supportIndexes.flatMap(data.lift)

  lazy val support =
    Validated.fromOption(supports.headOption, "no support vectors found")
      .toValidatedNel

  lazy val offset = support map { s =>
    s.valueOrNaN - (w dot s.feature)
  }

  def go = offset map (b => SVM(w, supports.toList map (_.feature), b))
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
