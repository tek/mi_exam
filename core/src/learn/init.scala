package tryp
package mi

import simulacrum._

trait Params

trait Config[C]
{
  type Params <: mi.Params
}

abstract class Initializer[C: Config]
extends AnyRef
{
  val config = implicitly[Config[C]]
  def init[A: Sample](c: C): config.Params
}

object Initializer
{
  def apply[C: Config](implicit instance: Initializer[C]) = instance
}
