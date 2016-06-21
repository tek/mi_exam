package tryp
package mi

trait ModelCreator[P, M]
{
  def run(est: Est[P]): M
}

case class IdModelCreator[P]()
extends ModelCreator[P, P]
{
  def run(est: Est[P]): P = est.params
}
