package tryp
package mi

import shapeless._
import poly._

import spire.math._
import spire.algebra.{AdditiveMonoid, MultiplicativeGroup}
import spire.implicits._

import breeze.math._
import breeze.linalg.DenseVector

trait Transfer
extends Poly1
{
  // def simple[A]: A ⇒ A

  implicit def caseNum[A: Numeric] = at[A](x ⇒ 1)
}

class Logistic[A: Numeric](beta: A)
extends Transfer
{
}

object Logistic

trait Node

abstract class Neuron[A: Numeric]
extends Node
{
  def output: A
}

case class N[A: Numeric](output: A)
extends Neuron[A]

abstract class Input[A: Numeric]
extends Neuron[A]

case class I[A: Numeric](output: A)
extends Input[A]

trait Edge

case class Weight[A: Numeric](w: A)
extends Edge

case class CN[A, B]
(inputs: List[Neuron[A]], weights: List[Weight[A]])
(implicit num: Numeric[A], vs: VectorSpace[B, A], add: AdditiveMonoid[A],
  mult: MultiplicativeGroup[A])
extends Neuron[A]
{
  def output: A = {
    add.sum(inputs.map(_.output)) / num.fromInt(inputs.length)
  }
}

object CN
{
  def randomInputs(num: Long) = {
    for(_ ← 1L to num) yield I(Random.double())
  }

  def weights(num: Long) = {
    val w = 1.0 / num
    for(_ ← 1L to num) yield Weight(w)
  }

  def default = {
    val num = 256
    val x = randomInputs(num).toList
    CN[Double, DenseVector[Double]](x, weights(num).toList)
  }

  def run() = {
    p(classOf[Numeric[Double]])
    val cn = default
    p(cn.output)
  }
}
