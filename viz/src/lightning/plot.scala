package tryp
package mi
package viz

@tc trait Plot[A]
{
  def data(a: A): List[List[Double]]
}

case class Line(data: List[List[Double]])

object Line
{
  implicit lazy val instance_Plot_Line: Plot[Line] =
    new Plot[Line] {
      def data(a: Line) = a.data
    }

}
