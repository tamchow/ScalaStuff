import java.io.File
import javafx.embed.swing.SwingFXUtils
import javafx.scene.image.{Image, WritableImage}
import javafx.scene.paint.Color

import scala.annotation.tailrec
import scala.language.implicitConversions

object MathUtils {
  val Epsilon: Double = 1E-8

  object Complex {
    type Complex = (Double, Double)
    val (_0, _1, i, e): (Complex, Complex, Complex, Complex) = ((0, 0), (1, 0), (0, 1), math.E)

    def apply(repr: String): Complex =
      if (repr.startsWith("i")) (0.0, repr.tail.toDouble)
      else if (repr.contains("i")) {
        val Array(r, i) = repr.split('i').map(_.toDouble)
        (r, i)
      } else (repr.toDouble, 0.0)

    def apply(x: Double): Complex = x.toComplex

    implicit class Double2Complex(val real: Double) extends AnyVal {
      def toComplex: Complex = (real, 0)
    }

    implicit def DoubleAsComplex(real: Double): Complex = real.toComplex

    implicit class ComplexOps(val base: Complex) extends AnyVal {
      @inline def isReal: Boolean = imag < Epsilon

      @inline def real: Double = base._1

      @inline def imag: Double = base._2

      @inline private[ComplexOps] def r = real

      @inline private[ComplexOps] def i = imag

      @inline def cabs: Double = r * r + i * i

      @inline def abs: Double = math.sqrt(cabs)

      @inline def arg: Double = math.atan2(i, r)

      @inline def +(x: Complex): Complex = (r + x.r, i + x.i)

      @inline def -(x: Complex): Complex = (r - x.r, i - x.i)

      @inline def unary_- : Complex = _0 - base

      @inline def *(x: Double): Complex = (x * r, x * i)

      @inline def *(x: Complex): Complex = (r * x.r - i * x.i, r * x.i + i * x.r)

      /**
        * Special-cases a few common exponents for faster execution
        *
        * @param x the exponent
        * @return base^x^
        */
      @inline def ^(x: Complex): Complex =
        x match {
          case `_0` => _1
          case `_1` => base
          case (2.0, 0.0) => base * base
          case _ =>
            if (base == _0) _0
            else {
              val b1 = x.real * arg + 0.5 * x.i * math.log(cabs)
              val b2 = math.pow(cabs, x.real / 2) * math.exp(-x.i * arg)
              (b2 * Math.cos(b1), b2 * Math.sin(b1))
            }
        }

      @inline def unary_~ : Complex = (r, -i)

      @inline def /(x: Complex): Complex =
        (r * x.r + i * x.i, i * x.r - r * x.i) * (1 / x.cabs)

      @inline def repr: String = s"${r}i$i"

      @inline def str: String = if (isReal) s"$r" else if (r < Epsilon) s"${i}i" else s"$r,${i}i"

      @inline def possibleZero_? : Boolean = base.cabs < Epsilon

      @inline def veryClose_?(x: Complex): Boolean = (base - x).possibleZero_?
    }

    trait ComplexIsNumeric extends Numeric[Complex] {
      def plus(x: Complex, y: Complex): Complex = ComplexOps(x) + y

      def minus(x: Complex, y: Complex): Complex = ComplexOps(x) - y

      def times(x: Complex, y: Complex): Complex = ComplexOps(x) * y

      def negate(x: Complex): Complex = -ComplexOps(x)

      def fromInt(x: Int): Complex = x.toComplex

      def toInt(x: Complex): Int = x.real.toInt

      def toLong(x: Complex): Long = x.real.toLong

      def toFloat(x: Complex): Float = x.real.toFloat

      def toDouble(x: Complex): Double = x.real
    }

    trait ComplexIsFractional extends ComplexIsNumeric with Fractional[Complex] {
      def div(x: Complex, y: Complex): Complex = ComplexOps(x) / y
    }

    /**
      * Defines Lexicographical order on the Complex numbers
      */
    trait ComplexOrdering extends Ordering[Complex] {
      def compare(x: Complex, y: Complex): Int =
        if (x.real < y.real || (x.real == y.real && x.imag < y.imag)) -1
        else if (x.real == y.real && x.imag < y.imag) 0
        else 1
    }

    implicit object ComplexIsFractional extends ComplexIsFractional with ComplexOrdering

  }

  import Complex._

  case class Polynomial(data: Map[Complex, Complex]) {
    if (data.isEmpty) throw new IllegalArgumentException("Empty Polynomial")

    private lazy val cleaned = data.filterNot {
                                                case (_, a) => a.possibleZero_?
                                              }.toSeq.sortBy(_._1)

    def apply(x: Complex): Complex =
      cleaned.foldLeft(_0) {
                             case (acc, (n, a)) => (if (n.possibleZero_?) a else a * (x ^ n)) + acc
                           }

    lazy val derivative: Polynomial =
      Polynomial(cleaned.collect {
                                   case (n, a) if !n.possibleZero_? => (n - _1, a * n)
                                 }.toMap)

    lazy val rootBound: Double =
      if (cleaned.isEmpty) throw new IllegalArgumentException("No upper bound of roots for empty polynomial")
      else {
        val sortedData = cleaned.map(_._2).map(_.cabs)
        val dataInit = sortedData.init
        1.0 + (if (dataInit.nonEmpty) math.sqrt(dataInit.max / sortedData.last) else 0.0)
      }

    lazy val numRoots: Int = cleaned.last._1.abs.toInt

    lazy val zeroIsValidRoot: Boolean = data.get(_0).exists(_ veryClose_? _0)

    override lazy val toString: String =
      cleaned.foldRight("") {
                              case ((n, a), acc) =>
                                if (a.isReal) {
                                  val c = a.real
                                  val abs_c = c.abs
                                  acc + (if (acc.isEmpty) "" else if (c < 0) " - " else " + ") +
                                  (if (n.possibleZero_?) abs_c else s"$abs_c * x^${n.str}")
                                } else {
                                  val str_a = a.str
                                  acc + (if (acc.isEmpty) "" else " + ") +
                                  (if (n.possibleZero_?) str_a else s"$str_a * x^${n.str}")
                                }
                            }

  }

  object Polynomial {
    def apply(coefficients: Seq[Complex]): Polynomial =
      Polynomial(coefficients.zipWithIndex
                   .map {
                          case (a, n) => (Complex(n), a)
                        }.toMap)

    def apply(coefficients: String): Polynomial = Polynomial(coefficients.split("\\s+").map(Complex(_)))
  }

  def defaultIfNaN(value: Double, default: Double = 0.0): Double = if (value.isNaN) default else value

  def clampHigh(max: Double)(value: Double): Double = value min max
}

object NewtonFractal {

  import MathUtils._
  import Complex._

  val PHI: Double = (1 + math.sqrt(5)) / 2

  def createColors(offset: Double = math.random)(n: Int): Seq[Color] =
    for (id <- 1 to n) yield {
      val hueRaw = id * PHI - math.floor(id * PHI)
      Color.hsb(((offset + hueRaw) % 1) * 360, 1.0, 1.0)
    }

  case class NewtonFractal(width: Int, height: Int, scaleFactor: Double = 0.2, center: Complex = _0)
                          (maxIterations: Int, smoothingBase: Option[Complex] = Some(e))
                          (p: Polynomial, R: Complex = _1, c: Option[Complex] = Some(_0)) {
    private lazy val colors: Seq[Color] = createColors()(p.numRoots)
    private lazy val radius = p.rootBound * scaleFactor
    println(p, p.rootBound)
    private lazy val dp = p.derivative
    private val roots = collection.mutable.Set[Complex]()

    private lazy val (radiusX, radiusY) = ((width * radius) / height, radius)
    private lazy val (xScale, yScale) = ((2 * radiusX) / width, (2 * radiusY) / height)
    private lazy val (screenCenterX, screenCenterY) = (width / 2.0, height / 2.0)

    private val logEpsilon = math.log(Epsilon)

    def convertScreenCoordinates(x: Int, y: Int): Complex =
      center + ((x - screenCenterX) * xScale, (screenCenterY - y) * yScale)

    private def getC(x: Int, y: Int): Complex = c.getOrElse(convertScreenCoordinates(x, y))

    private def root(zn: Complex) = roots.find(zn veryClose_? _)

    private def converged(zn: Complex) = root(zn).nonEmpty

    private val exponentialSmoothing = smoothingBase.isDefined
    private val base = smoothingBase.getOrElse(e)
    private val clampIterations = clampHigh(maxIterations) _

    private def evaluate(x: Int, y: Int) = {
      val (zn_0, zp_0, a) = (convertScreenCoordinates(x, y), _0, getC(x, y))

      def newtonForwardStep(zn: Complex) = zn - R * (p(zn) / dp(zn)) + a

      def stopIteration_?(iterations: Int, zn: Complex, zp: Complex) = iterations >= maxIterations || (zn.veryClose_?(zp) || converged(zn))

      @tailrec
      def iterateExponential(currentIteration: Int, zn: Complex, zp: Complex, expIterations: Complex):
      (Int, Complex, Complex, Complex) = {
        if (stopIteration_?(currentIteration, zn, zp))
          (currentIteration, zn, zp, expIterations)
        else
          iterateExponential(currentIteration + 1,
                             newtonForwardStep(zn),
                             zn,
                             expIterations + (base ^ (-zn.cabs - 0.5 / (zp - zn).cabs)))
      }

      @tailrec
      def iteratePlain(currentIteration: Int, zn: Complex, zp: Complex):
      (Int, Complex, Complex, Complex) = {
        if (stopIteration_?(currentIteration, zn, zp))
          (currentIteration, zn, zp, _0)
        else
          iteratePlain(currentIteration + 1,
                       newtonForwardStep(zn),
                       zn)
      }

      val (i, zn, zp, expIterations) = if (exponentialSmoothing)
                                         iterateExponential(0, zn_0, zp_0, _0)
                                       else
                                         iteratePlain(0, zn_0, zp_0)
      val rootZn = addAndGetRoot(i, zn)
      val finalExpIterations = expIterations.abs
      if (exponentialSmoothing) {
        (clampIterations(defaultIfNaN(finalExpIterations, i)), rootZn)
      } else {
        val rootZnOr0 = rootZn.getOrElse(_0)
        val logD0 = math.log((zp - rootZnOr0).cabs)
        val logDist = if (i < maxIterations) ((logEpsilon - logD0) / (math.log((zn - rootZnOr0).cabs) - logD0)).abs else 0.0
        (clampIterations(i + defaultIfNaN(logDist)), rootZn)
      }
    }

    private def addAndGetRoot(iterations: Int, zn: Complex) = {
      classOf[NewtonFractal].synchronized {
                                            if (iterations < maxIterations && !converged(zn) && roots.size < p.numRoots) {
                                              if (zn.veryClose_?(_0) && !p.zeroIsValidRoot) {
                                                root(zn)
                                              } else {
                                                roots += zn
                                                Some(zn)
                                              }
                                            } else {
                                              root(zn)
                                            }
                                          }

    }

    private type PixelDatum = (Double, Option[Complex])

    private def evaluateAll() = {
      val data = Array.ofDim[PixelDatum](width, height)
      var currentMaxIterations = 0.0
      (0 until height).par.foreach(y => (0 until width).foreach(x => {
        val (iterations, value) = evaluate(x, y)
        data(y)(x) = (iterations, value)
        currentMaxIterations = currentMaxIterations max iterations
      }))
      (data, currentMaxIterations)
    }

    private lazy val colorMap = {
      /*if (roots.size != p.numRoots)
        throw new IllegalArgumentException(s"Not all roots of polynomial p(x) = $p were found" +
                                           s" (roots found: $roots)," +
                                           s" try maxIterations > $maxIterations")
      else */ roots.zip(colors).toMap
    }

    private def colorize(rawData: Array[Array[PixelDatum]], currentMaxIterations: Double) = {
      val img = new WritableImage(width, height)
      val imgWriter = img.getPixelWriter
      val progress = new java.util.concurrent.atomic.AtomicLong(0)
      val progressMultiplier = 100.0 / (width * height)
      (0 until height).par.foreach(y => (0 until width).foreach(x => {
        val (iterations, value) = rawData(y)(x)
        val color = value.map(colorMap(_)).getOrElse(Color.BLACK)

        val shadedColor = color.deriveColor(0, 1.0, 1.0 - iterations.floor / currentMaxIterations, 1.0)
        val interpolationColor = color.deriveColor(0, 1.0, 1.0 - iterations.ceil / currentMaxIterations, 1.0)
        val finalColor = shadedColor.interpolate(interpolationColor, iterations % 1)
        if (iterations.isNaN) println(iterations, x, y)
        imgWriter.setColor(x, y, finalColor)
        if (progress.getAndIncrement % width == 0) println(f"${progress.get * progressMultiplier}%.4f%% completed")
      }))
      println(s"Max Iterations: $currentMaxIterations")
      println(s"${roots.size} roots:")
      roots.foreach(println)
      img
    }

    def generate(): Image = {
      val (data, maxIterations) = evaluateAll()
      colorize(data, maxIterations)
    }

    def generateAndSave(file: File): Image = {
      val img = generate()
      javax.imageio.ImageIO.write(SwingFXUtils.fromFXImage(img, null), "PNG", file)
      img
    }
  }

  def main(args: Array[String]): Unit = {
    val pol3 = Polynomial("1 0 0 1")
    val pol9 = Polynomial("0 315 0 -420 0 378 0 -180 0 35")
    val fractal = NewtonFractal(1000, 1000, 1.5)(64)(pol9)
    fractal.generateAndSave(new java.io.File("image.png"))
  }
}