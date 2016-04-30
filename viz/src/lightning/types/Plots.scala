package tryp
package mi
package viz

trait Plots extends Base {

  /**
   * One or more one-dimensional series data as lines.
   */
  def line(series: List[List[Double]],
           label: List[Int] = List[Int](),
           size: List[Double] = List[Double](),
           alpha: List[Double] = List[Double](),
           xaxis: String = "",
           yaxis: String = ""): Visualization = {

    val data = Map("series" -> series.toList)

    val settings = new Settings()
      .append(List(Label(label), Size(size), Alpha(alpha)))
      .append(List(Axis(xaxis, "xaxis"), Axis(yaxis, "yaxis")))

    plot("line", data ++ settings.toMap)

  }

  /**
   * One or more one-dimensional series data streamed as lines.
   */
  def lineStreaming(series: List[List[Double]],
                    size: List[Double] = List[Double](),
                    color: List[List[Double]] = List[List[Double]](),
                    alpha: List[Double] = List[Double] (),
                    label: List[Int] = List[Int](),
                    xaxis: String = "",
                    yaxis: String = "",
                    viz: Visualization = null): Visualization = {

    val data = Map("series" -> series.toList, "color" -> color.toList)

    val settings = new Settings()
      .append(List(Label(label), Size(size), Alpha(alpha)))
      .append(List(Axis(xaxis, "xaxis"), Axis(yaxis, "yaxis")))

    val payload = data ++ settings.toMap

    if (viz == null) {
      plot("line-streaming", payload)

    } else {
      // viz.append(payload)
      viz
    }

  }

  /**
   * Force-directed network from connectivity.
   */
  def force(conn: List[List[Double]],
            label: List[Int] = List[Int](),
            value: List[Double] = List[Double](),
            colormap: String = "",
            size: List[Double] = List[Double]()): Visualization = {

    val links = Utils.getLinks(conn)
    val nodes = Utils.getNodes(conn)

    val data = Map("links" -> links.toList, "nodes" -> nodes.toList)

    val settings = new Settings()
      .append(List(Label(label), Value(value), Colormap(colormap), Size(size)))

    plot("force", data ++ settings.toMap)

  }

  /**
   *  Two-dimensional data as points.
   */
  def scatter(x: List[Double],
              y: List[Double],
              label: List[Int] = List[Int](),
              value: List[Double] = List[Double](),
              colormap: String = "",
              size: List[Double] = List[Double](),
              alpha: List[Double] = List[Double](),
              xaxis: String = "",
              yaxis: String = ""): Visualization = {

    val points = Utils.getPoints(x, y)
    val data = Map("points" -> points.toList)

    val settings = new Settings()
      .append(List(Label(label), Value(value), Colormap(colormap), Size(size), Alpha(alpha)))
      .append(List(Axis(xaxis, "xaxis"), Axis(yaxis, "yaxis")))

    plot("scatter", data ++ settings.toMap)
  }

  /**
   * Dense matrix or a table as a heat map.
   */
  def matrix(matrix: List[List[Double]],
             colormap: String = "",
             rowLabels: List[String] = List[String](),
             colLabels: List[String] = List[String]()): Visualization = {

    val data = Map("matrix" -> matrix.toList, "rowLabels" -> rowLabels.toList,"columnLabels" -> colLabels.toList)

    val settings = new Settings()
      .append(Colormap(colormap))

    plot("matrix", data ++ settings.toMap)
  }

  /**
   * Sparse adjacency matrix with labels from connectivity.
   */
  def adjacency(conn: List[List[Double]],
                label: List[Int] = List[Int](),
                labels: List[String] = List[String]()): Visualization = {

    val links = Utils.getLinks(conn)
    val nodes = Utils.getNodes(conn)

    val data = Map("links" -> links.toList, "nodes" -> nodes.toList, "labels" -> labels.toList)

    val settings = new Settings()
      .append(Label(label))

    plot("adjacency", data ++ settings.toMap)

  }

  /**
   * Chloropleth map of the world or united states.
   */
  def map(regions: List[String],
          values: List[Double],
          colormap: String = ""): Visualization = {

    if (!(regions.forall(s => s.length == 2) | regions.forall(s => s.length == 3))) {
      throw new IllegalArgumentException("Region names must have 2 or 3 characters")
    }

    val data = Map("regions" -> regions.toList, "values" -> values.toList)

    val settings = new Settings()
      .append(Colormap(colormap))

    plot("map", data ++ settings.toMap)

  }

  /**
   * Node-link graph from spatial points and connectivity.
   */
  def graph(x: List[Double],
            y: List[Double],
            conn: List[List[Double]],
            label: List[Int] = List[Int](),
            value: List[Double] = List[Double](),
            colormap: String = "",
            size: List[Double] = List[Double]()): Visualization = {

    val links = Utils.getLinks(conn)
    val nodes = Utils.getPoints(x, y)
    val data = Map("links" -> links, "nodes" -> nodes.toList)

    val settings = new Settings()
      .append(List(Label(label), Value(value), Colormap(colormap), Size(size)))

    plot("graph", data ++ settings.toMap)

  }

  /**
   * Node-link graph with bundled edges from spatial points and connectivity.
   */
  def graphBundled(x: List[Double],
                   y: List[Double],
                   conn: List[List[Double]],
                   label: List[Int] = List[Int](),
                   value: List[Double] = List[Double](),
                   colormap: String = "",
                   size: List[Double] = List[Double]()): Visualization = {

    val links = Utils.getLinks(conn)
    val nodes = Utils.getPoints(x, y)
    val data = Map("links" -> links, "nodes" -> nodes.toList)

    val settings = new Settings()
      .append(List(Label(label), Value(value), Colormap(colormap), Size(size)))

    plot("graph-bundled", data ++ settings.toMap)

  }

}
