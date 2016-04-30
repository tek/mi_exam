package tryp
package mi
package viz

trait Linked extends Base {

  /**
   * Linked scatter and line plot
   */
  def scatterLine(x: List[Double],
                  y: List[Double],
                  series: List[List[Double]],
                  label: List[Int] = List[Int](),
                  size: List[Double] = List[Double](),
                  alpha: List[Double] = List[Double]()): Visualization = {

    val points = Utils.getPoints(x, y)
    val data = Map("series" -> series.toList, "points" -> points.toList)

    val settings = new Settings()
      .append(List(Label(label), Size(size), Alpha(alpha)))

    plot("scatter-line", data ++ settings.toMap)

  }

}
