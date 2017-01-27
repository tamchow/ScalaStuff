/**
  * Generates 3D scatter plots
  */

import java.awt.Color
import javax.swing.{JFrame, JPanel}

import org.math.plot._

object Scatter3D {
  def loadDataFromCSV(csvPath: String): JPanel = {
    val data = scala.io.Source.fromFile(csvPath).getLines().map(_.split(",").map(_.trim.toDouble).toSeq)
    val (x, y, z) = (data map {
      case Seq(x, y, z) => (x, y, z)
    }).toArray.unzip3
    val colors = z map (x => new Color(Color.HSBtoRGB(x.toFloat, 1.0f, 0.5f)))
    val plot = new Plot3DPanel()
    plot.addScatterPlot("Fractal in 3D", colors, x, y, z)
    plot
  }

  def main(args: Array[String]): Unit = {
    val plot = loadDataFromCSV("D:/Programming/Projects/FractalGenerator/output.csv")
    val frame = new JFrame("Fractal3D")
    frame.setSize(900,900)
    frame.setContentPane(plot)
    frame.setVisible(true)
  }
}
