package tryp
package mi

import cats._, free._, Free._, data._

object MLPFree
{
  type MLPData = (Col, Nel[Mat])

  sealed trait MLPA[A]

  case class Set(s: MLPData)
  extends MLPA[Unit]

  case object Get
  extends MLPA[MLPData]

  type MLP[A] = Free[MLPA, A]
  type MLPState[A] = State[MLPData, A]

  def set(s: MLPData): MLP[Unit] =
    liftF[MLPA, Unit](Set(s))

  def get: MLP[MLPData] =
    liftF[MLPA, MLPData](Get)

  val pureCompiler: MLPA ~> MLPState = new (MLPA ~> MLPState) {
    def apply[A](fa: MLPA[A]): MLPState[A] =
      fa match {
        case Set(s) ⇒ State.set(s)
        case Get ⇒ State.get
      }
  }
}
