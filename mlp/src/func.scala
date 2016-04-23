package tryp
package mi
package mlp

import breeze.numerics.exp

object Logistic
{
  def main(a: Double, beta: Double) = 1.0 / (1.0 + exp(-beta * a))
}

case class Logistic(beta: Double)
extends DFunc[LogisticDeriv]
{
  implicit val doubleImpl = new DI {
    def apply(a: Double) = Logistic.main(a, beta)
  }

  lazy val deriv = new LogisticDeriv(beta)
}

class LogisticDeriv(beta: Double)
extends DFunc[NullFuncBase]
{
  implicit val doubleImpl = new DI {
    def apply(a: Double) = {
      val fx = Logistic.main(a, beta)
      fx * (1 - fx)
    }
  }

  def deriv = NullFunc
}
