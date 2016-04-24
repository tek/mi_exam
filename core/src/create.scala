package tryp
package mi

trait ModelCreator[P, M]
{
  def run(est: Estimation[P]): M
}

case class IdModelCreator[P]()
extends ModelCreator[P, P]
{
  def run(est: Estimation[P]): P = est.params
}
