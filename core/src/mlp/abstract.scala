package tryp
package mi

import shapeless._
import poly._

import spire.math._
import spire.algebra.{AdditiveMonoid, MultiplicativeGroup}
import spire.implicits._
import spire.random._

import breeze.math._
import breeze.linalg.Transpose

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

case class Layer[A: Numeric: AdditiveMonoid: MultiplicativeGroup, B]
(nodes: List[CN[A, B]])
(implicit vs: VectorSpace[B, A])
