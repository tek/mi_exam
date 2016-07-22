package tryp
package mi
package rbf

import simulacrum._

import scalaz.std.vector.{vectorInstance => zVectorInstance}
import scalaz.syntax.zip._

import monocle.macros.{GenLens, Lenses}
import monocle.function._
import monocle.syntax.apply._

import spire.math.exp
import spire.algebra._
import spire.implicits._
import spire.random._

import breeze.linalg.{sum, squaredDistance, pinv}
import breeze.linalg.functions.euclideanDistance
import breeze.numerics.abs
import breeze.generic.UFunc

import LearnConf._

import BasisFunction.ops._

@typeclass trait BasisFunction[P]
{
  def center(p: P): Col
  def output(p: P)(input: Col): Double
}

@typeclass abstract class Initializer[P]
extends AnyRef
{
  def init[S: Sample](features: Int, rbfs: Int): RBFs[P]
}

abstract class Initialization[P]
{
  def create(features: Int, rbfs: Int): RBFs[P]
}

class RandomInitialization[P: Initializer, S: Sample]
extends Initialization[P]
{
  def create(features: Int, rbfs: Int) =
    Initializer[P].init[S](features, rbfs)
}

class ManualInitialization[P: Initializer]
(manual: RBFs[P])
extends Initialization[P]
{
  def create(features: Int, rbfs: Int) = manual
}

@Lenses
case class GaussBF(center: Col, variance: Double)
extends UFunc
{
  lazy val varianceSquared = variance * variance

  implicit val colImpl = new Impl[Col, Double] {
    def apply(a: Col) = GaussBF.main(a, center, varianceSquared)
  }
}

trait GaussBFInstances
{
  implicit lazy val instance_Initializer_GaussBF
  : Initializer[GaussBF] =
    new Initializer[GaussBF] {
      def init[S: Sample](features: Int, rbfs: Int) = {
        RBFs(GaussBF.rand[S](features, rbfs))
      }
    }

  implicit lazy val instance_UpdateParams_GaussBF
  : UpdateParams[GaussBF] =
    new UpdateParams[GaussBF] {
      def update(model: RBFNet[GaussBF], pred: RBFNet[GaussBF],
        eta: Double) = {
          model
      }

      def updateCenter(index: Int, diff: Col) =
        (GenLens[RBFs[GaussBF]](_.bf) ^|-?
          nevIndex[GaussBF].index(index) ^|-> GaussBF.center)
            .modify(_ + diff)

      def updateParams(bf: GaussBF)(all: Nev[GaussBF], lambda: Double)
      : GaussBF = {
        val dist = all.map {
          case a if a != bf => euclideanDistance(bf.center, a.center)
          case _ => Double.MaxValue
        }
        val newSigma = dist.unwrap.min * lambda
        (bf &|-> GaussBF.variance).set(newSigma)
      }
    }

  implicit lazy val instance_BasisFunction_GaussBF
  : BasisFunction[GaussBF] =
    new BasisFunction[GaussBF] {
      def center(a: GaussBF) = a.center

      def output(a: GaussBF)(input: Col): Double = {
        import a._
        a(input)
      }
    }
}

object GaussBF
extends GaussBFInstances
{
  def rand[S: Sample](features: Int, rbfs: Int): Nev[GaussBF] = {
    def inst = GaussBF(Col.rand(features) * Sample[S].range,
      tryp.Random.double())
    rbfs gen inst match {
      case head :: tail => Nev(head, tail: _*)
      case a => Nev(inst)
    }
  }

  def main(sample: Col, center: Col, variance: Double) = {
    exp(-squaredDistance(sample, center) / variance)
  }
}
