package tryp
package mi

import breeze.numerics._

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
