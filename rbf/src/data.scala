package tryp
package mi
package rbf

import scalaz.std.vector.{vectorInstance => zVectorInstance}

import breeze._
import linalg._
import numerics._
import functions.euclideanDistance

case class RBFs[P: BasisFunction](bf: Nev[P])
{
  def centers = bf map(_.center)

  def count = bf.length
}

object RBFs
{
  implicit def instance_ParamDiff_RBFNet[P]: ParamDiff[RBFs[P]] =
    new ParamDiff[RBFs[P]] {
      def diff(a: RBFs[P], b: RBFs[P]): Double = {
        val dist = a.centers.fzip(b.centers)
          .map { case (a, b) => euclideanDistance(a, b) }
          .tail
        sum(dist) / a.count
      }
    }
}

case class RBFNet[P: BasisFunction](rbfs: RBFs[P], weights: Col)
{
  def bf = rbfs.bf
}

object RBFNet
{
  implicit def instance_ModelState_RBFNet[P]: ModelState[RBFNet[P]] =
    new ModelState[RBFNet[P]] {
      def output(a: RBFNet[P]) = a.output
    }
}

object RBF
{
  implicit def instance_CreateEstimator_RBF
  [S: Sample: MC, P: BasisFunction: Initializer: UpdateParams]
  : CreateEstimator[S, RBFs[P], RBFLearnConf[P]] =
    new CreateEstimator[S, RBFs[P], RBFLearnConf[P]] {
      def apply(data: Nel[S])
      (implicit conf: RBFLearnConf[P], sconf: MSConf)
      : Estimator[RBFs[P]] = {
        val stop = ParamDiffStopCriterion[RBFs[P]](sconf.steps, sconf.epsilon)
        RBFEstimator(data, conf, stop)
      }
    }

  implicit def instance_CreateModelCreator_RBF
  [S: Sample: MC, P: BasisFunction] =
    new CreateModelCreator[S, RBFs[P], RBFNet[P], RBFLearnConf[P]] {
      def apply(data: Nel[S])
      (implicit conf: RBFLearnConf[P], sconf: MSConf) =
        RBFModelCreator(data, conf)
    }

  implicit def instance_CreateValidator_RBF[S: Sample: MC, P: BasisFunction]
  : CreateValidator[S, RBFNet[P], RBFLearnConf[P]] =
    new CreateValidator[S, RBFNet[P], RBFLearnConf[P]] {
      def apply(data: Nel[S])
      (implicit conf: RBFLearnConf[P], sconf: MSConf)
      : Validator[RBFNet[P]] =
        RBFValidator(data, conf)
    }
}

@tc abstract class UpdateParams[P: BasisFunction]
extends AnyRef
{
  def updateCenter(index: Int, diff: Col): RBFs[P] => RBFs[P]

  def updateParams(a: P)(all: Nev[P], lambda: Double): P
}

case class RBFLearnConf[P: BasisFunction](rbfs: Int,
  eta: Double, lambda: Double, initialization: Initialization[P], cost: Func2)
  {
    lazy val bf = BasisFunction[P]

    def initialParams(featureCount: Int) =
      initialization.create(featureCount, rbfs)
  }

object RBFLearnConf
{
  def default[P: BasisFunction: Initializer, S: Sample](
    rbfs: Int = 3,
    eta: Double = 0.3,
    lambda: Double = 2.0,
    initialization: Option[Initialization[P]] = None,
    cost: Func2 = QuadraticError
  ) = {
    val init = initialization | new RandomInitialization[P, S]
    RBFLearnConf[P](rbfs, eta, lambda, init, cost)
  }
}
