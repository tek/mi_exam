package tryp
package mi
package viz

import scalax.chart._, api._

import scala.swing._

case class FigureConf(name: String, rows: Int, width: Int, height: Int)

object FigureConf
{
  def default(
    name: String,
    rows: Int = 1,
    width: Int = 800,
    height: Int = 600
  ) =
      FigureConf(name, rows, width, height)
}

case class FigureState(frame: Frame)

class Figure(config: FigureConf, plots: List[Chart])
{
  lazy val rows = math.min(plots.length, config.rows)

  lazy val cols = math.ceil(plots.length.toDouble / rows).toInt

  val transparent = new Color(255, 255, 255, 0)

  def show(): VisibleFigure = {
    val panel = new GridPanel(rows, cols) {
      contents ++= plots.map(_.toComponent())
    }
    panel.contents.foreach { a =>
      a.background = transparent
    }
    val frame = new MainFrame {
      contents = panel
    }
    frame.size = new Dimension(config.width, config.height)
    frame.visible = true
    new VisibleFigure(this, FigureState(frame))
  }
}

object Figure
{
  def apply(config: FigureConf) =
    new Figure(config, List.empty)
}

class VisibleFigure(figure: Figure, state: FigureState)
