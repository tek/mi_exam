package tryp
package mi
package unit

class BasicSpec
extends SpecBase
{
  def is = s2"""
  run $doit
  """

  def doit = {
    CN.run()
    1 === 1
  }
}
