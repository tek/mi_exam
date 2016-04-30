package tryp
package mi
package viz

trait Three extends Base {

  /**
   * Three-dimensional data as spheres.
   */
  def scatter3(x: List[Double],
               y: List[Double],
               z: List[Double],
               label: List[Int] = List[Int](),
               size: List[Double] = List[Double]()): Visualization = {

    val points = (x, y, z).zipped.map((x, y, z) => List(x, y, z)).toList
    val data = Map("points" -> points)

    val settings = new Settings()
      .append(List(Label(label), Size(size)))

    plot("scatter-3", data ++ settings.toMap)
  }

}
