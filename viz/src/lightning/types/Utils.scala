package tryp
package mi
package viz

object Utils {

  def getLinks(conn: List[List[Double]]): List[List[Double]] = {

    if (conn.length == conn(0).length) {

      conn.zipWithIndex
        .flatMap{case (row, i) => row.zipWithIndex
        .filter{case (x, j) => x != 0}.map{case (x, j) => List(i, j, x)}}

    } else {

      conn(0).length match {
        case 2 => conn
        case 3 => conn.map(l => List(l(0), l(1), 1.0))
        case _ => throw new IllegalArgumentException("Elements per link must be 2 or 3")
      }

    }
  }

  def getNodes(conn: List[List[Double]]): List[Int] = {

    if (conn.length == conn(0).length) {
      Range(0, conn.length).toList
    } else {
      val n = conn.map(l => l.max).max.toInt + 1
      Range(0, n).toList
    }

  }

  def getPoints(x: List[Double], y: List[Double]): List[List[Double]] = {
    (x, y).zipped.map((x, y) => List(x, y))
  }

}
