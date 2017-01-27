import scala.annotation.tailrec
import scala.util.Either
import scalax.chart.api._

object Interpolation {

  object ExtrapolationTypes extends Enumeration {
    val Linear, Constant, None = Value
    type ExtrapolationType = Value
  }

  import ExtrapolationTypes._

  def createInterpolant(xs: Seq[Double], ys: Seq[Double], extrapolationType: ExtrapolationType): (Double => Double) = {
    xs.length match {
      case length if length != ys.length => throw new IllegalArgumentException(
        s"length of xs (${xs.length}) != length of ys (${ys.length})")
      case 0 => (_: Double) => 0
      case 1 => (_: Double) => ys.head
      case length =>
        val indices = (0 until length) sortWith ((a, b) => xs(a) < xs(b))

        def indexMap(ls: Seq[Double], limit: Int, f: (Int => Double)) =
          (0 until limit) map f

        def inIndexOrder(ls: Seq[Double]) = indexMap(ls, length, i => ls(indices(i)))

        val (newXs, newYs) = (inIndexOrder(xs), inIndexOrder(ys))
        //Get consecutive differences and Slopes
        val dxs = indexMap(newXs, length - 1, i => newXs(i + 1) - newXs(i))
        val dys = indexMap(newYs, length - 1, i => newYs(i + 1) - newYs(i))
        val ms = (dxs zip dys) map (dxdy => dxdy._2 / dxdy._1)
        //Get degree-1 coefficients
        val c1s = IndexedSeq(ms(0)) ++ dxs.indices.map { i =>
          val Limit = ms.length - 1
          i match {
            case Limit => ms(Limit)
            case idx =>
              val (m, mNext) = (ms(idx), ms(idx + 1))
              if (m * mNext <= 0) 0 else {
                val (dx, dxNext) = (dxs(idx), dxs(idx + 1))
                val common = dx + dxNext
                3 * common / ((common + dxNext) / m + (common + dx) / mNext)
              }
          }
        }
        //Get degree-2 and degree-3 coefficients
        val (c2s, c3s) = (0 until (c1s.length - 1)).map { i =>
          val (c1, m, invDx) = (c1s(i), ms(i), 1 / dxs(i))
          val common = c1 + c1s(i + 1) - 2 * m
          ((m - c1 - common) * invDx, common * invDx * invDx)
        }.unzip

        def interpolator(x: Double) = {
          val end = length - 1
          if (x == newXs(end)) newYs(end) else {
            @tailrec
            def intervalSearch(low: Int, high: Int): Either[(Int, Int), Double] = {
              if (low > high) Left((low, high)) else {
                val mid = low + ((high - low) / 2)
                val xHere = newXs(mid)
                if (xHere < x) intervalSearch(mid + 1, high)
                else if (xHere > x) intervalSearch(low, mid - 1)
                else Right(newYs(mid))
              }
            }

            intervalSearch(0, c3s.length - 1) match {
              case Right(y) => y
              case Left((_, high)) =>
                val i = 0 max high
                val diff = x - newXs(i)
                val diffSq = diff * diff
                newYs(i) + c1s(i) * diff + c2s(i) * diffSq + c3s(i) * diff * diffSq
            }
          }
        }

        val (xMin, xMax, yMin, yMax) = (newXs.min, newXs.max, newYs.min, newYs.max)
        (x: Double) =>
          extrapolationType match {
            case None => interpolator(x)
            case other =>
              if (x < xMin || x > xMax) {
                other match {
                  case Linear => yMin + (x - xMin) / (xMax - xMin) * yMax
                  case Constant => if (x < xMin) yMin else yMax
                }
              } else {
                interpolator(x)
              }
          }
    }
  }

  def visualize(xs: Seq[Double], ys: Seq[Double], extrapolationType: ExtrapolationType)
               (start: Double, end: Double, step: Double): Unit = {
    val interpolator = createInterpolant(xs, ys, extrapolationType)
    val data = for (i <- start to end by step) yield (i, interpolator(i))
    val chart = XYLineChart(data)
    chart.labelGenerator = XYLabelGenerator((a, b) => s"($a,$b)")
    chart.show()
  }

  def main(args: Array[String]): Unit = {
    visualize(Seq(1, 2, 4, 3, 5), Seq(1, 4, 16, 9, 25), None)(-2.0, 6.0, 0.5)
  }
}