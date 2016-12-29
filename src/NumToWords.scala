import scala.util.Random

/**
  * Converts an integral numeral to words - Size-unlimited!
  */
object NumToWords {

  //@formatter:off
  private val (negative, and, latinAnd, empty)= ("negative ", "and ", "et", "")

  implicit def NumberRepresentationToNumber(number: NumberRepresentation): BigInt = number.value

  implicit def NumberRepresentationToString(number: NumberRepresentation): String = number.name

  implicit def BigIntToInt(number: BigInt): Int = number.intValue()

  implicit def IntToBigInt(number: Int): BigInt = BigInt(number)
  private val ones = IndexedSeq("", "one ", "two ", "three ", "four ", "five ",
                                "six ", "seven ", "eight ", "nine ", "ten ",
                                "eleven ", "twelve ", "thirteen ", "fourteen ", "fifteen ",
                                "sixteen ", "seventeen ", "eighteen ", "nineteen ")
  private val tens = IndexedSeq("", "", "twenty ", "thirty ", "forty ", "fifty ",
                                "sixty ", "seventy ", "eighty ", "ninety ")
  private val (thousand, hundred, ten, nineteen, nine, three, two, one, zero) =
              (NumberRepresentation(1000,"thousand "), NumberRepresentation(100,"hundred "),
                NumberRepresentation(10, ones(10)), NumberRepresentation(19, ones(19)),
                NumberRepresentation(9, ones(9)), NumberRepresentation(3, ones(3)),
                NumberRepresentation(2, ones(2)), NumberRepresentation(1, ones(1)),
                NumberRepresentation(0, "zero"))
  private val latinSingles = IndexedSeq("", "m", "b", "tr", "quadr", "quin", "sext", "sept", "oct", "non")
  private val latinOnes = IndexedSeq("", "un", "duo", "tre", "quattor", "quin", "sex", "sept", "octo", "novem")
  private val latinTens = IndexedSeq("", "dec", "vigint", "trigint", "quadragint", "quinquagint", "sexagint",
                                      "septuagint", "octogint", "nonagint")
  private val latinHundred = NumberRepresentation(hundred, "cent")
  private val (internationalMultipleSuffix, thousandOffset, hundredOffset) = ("illion ", two, one)
  private val indianRoots = IndexedSeq("lakh ","crore ","arab ","kharab ","neel ","padma ","shankh ")
  private val indianMultiples = indianRoots.zipWithIndex map { case (root, index) =>
    NumberRepresentation(thousand * (hundred pow  BigInt(index) + hundredOffset), root)
  }

  def doIfPredicateMatches[A](predicate: A => Boolean, op: A => String, list: Seq[A]): (String, Boolean) = {
    (list filter predicate) lastOption match {
      case None => (empty, false)
      case Some(x) => (op(x), true)
    }
  }

  def testLatinRoots(rootsTill: BigInt): Unit = {
    for ((value: NumberRepresentation, idx: Int) <- generateInternationalMultiples(rootsTill).zipWithIndex) {
      println(s"${idx + 1}->$value")
    }
  }
  //@formatter:on

  private def generateInternationalMultiples(upperLimit: BigInt) = one to upperLimit map { index =>
    NumberRepresentation(thousand pow index + thousandOffset,
      numToLatinRoot(index) + internationalMultipleSuffix)
  }

  def numToLatinRoot(number: BigInt): String = {
    def convert(number: BigInt, suffix: String): String = {
      val out1 = if (number >= hundred) convert(number % hundred, empty) +
        (if (number % hundred > zero) latinAnd else empty) +
        (if (number / hundred > two) convert(number / hundred, latinHundred) else latinHundred) + suffix
      else empty

      out1 + (if (number < hundred && number >= ten) latinOnes(number % ten) +
        latinTens(number / ten) + suffix
      else if (number < ten) latinOnes(number) + suffix
      else empty)
    }

    if (number < ten) latinSingles(number) else convert(number, empty)
  }

  def numToWords(number: BigInt, useIndianNumerals: Boolean, internationalMultiplesTill: BigInt): String = {
    val multiples = if (useIndianNumerals) indianMultiples
    else
      generateInternationalMultiples(internationalMultiplesTill)

    def convert(number: BigInt, suffix: String): String = {
      val (out1, numberGTEMillion) = doIfPredicateMatches((value: NumberRepresentation) => number >= value,
        (value: NumberRepresentation) => convert(number / value, value) +
          convert(number % value, empty) + suffix, multiples)

      val out2 = out1 + (if (!numberGTEMillion && number >= thousand)
        convert(number / thousand, thousand) +
          convert(number % thousand, empty) + suffix
      else if (!numberGTEMillion && number >= hundred) convert(number / hundred, hundred) +
        (if (number % hundred > zero) and else empty) + convert(number % hundred, empty) + suffix
      else empty)

      out2 + (if (number < hundred && number >= nineteen) tens(number / ten) + ones(number % ten) + suffix
      else if (number < nineteen) ones(number) + suffix
      else empty)
    }

    //bad idea! invalidates transitivity property of '==' ! However, this looks better.
    if (zero == number) zero
    else if (number < zero) negative + convert(number.abs, empty)
    else convert(number, empty)
  }

  def createNDigitNumber(N: BigInt): BigInt = {
    assert(N >= 0, "Number of digits cannot be negative!")

    @annotation.tailrec
    def accumulateDigits(number: String, digitsAccumulator: BigInt): String = digitsAccumulator match {
      case N => number
      case _ => accumulateDigits(number + (Random nextInt nine.value), digitsAccumulator + one)
    }

    BigInt(accumulateDigits(empty, zero.value))
  }

  def countDigits(Number: BigInt): BigInt = {
    @annotation.tailrec
    def counter(num: BigInt, digitsAccumulator: BigInt): BigInt = num match {
      case zero.value => digitsAccumulator
      case _ => counter(num / ten, digitsAccumulator + one)
    }

    counter(Number.abs, zero)
  }

  def main(args: Array[String]): Unit = {
    def prompt() = {
      print(
        """Enter 1 to enter a decimal number,
          |or 2 to enter the number of digits of a randomly generated decimal number: > """.stripMargin)
    }

    def inputBigInt() = BigInt(io.StdIn readLine())

    def promptedReadFlags(): (Boolean, BigInt) = try {
      print("Use Indian numeral system? Y/N: > ")

      def readYNInput(): Boolean = io.StdIn readChar() match {
        case 'y' | 'Y' => true
        case 'n' | 'N' => false
        case _ => readYNInput()
      }

      val useIndianNumerals = readYNInput()
      if (!useIndianNumerals) {
        print("Enter the number till which to generate Latin roots: > ")
        (useIndianNumerals, BigInt(io.StdIn readLine()))
      } else (useIndianNumerals, zero)
    } catch {
      case nfe: NumberFormatException => promptedReadFlags()
    }

    def promptedReadInput(): BigInt = try {
      io.StdIn readInt() match {
        case 1 => print("Enter the number: > "); inputBigInt()
        case 2 => print("Enter the number of digits: > "); createNDigitNumber(inputBigInt())
        case _ => prompt(); promptedReadInput()
      }
    } catch {
      case nfe: NumberFormatException => prompt(); promptedReadInput()
    }

    @annotation.tailrec
    def retry(): Unit = {
      print("Try another? (Y/N): > ")
      io.StdIn readChar() match {
        case 'y' | 'Y' => main(args)
        case 'n' | 'N' => ()
        case _ => retry()
      }
    }

    val (useIndianNumerals, rootLimit) = promptedReadFlags()
    prompt()
    val num = promptedReadInput()
    println(s"Input in digits = $num")
    println(s"Number of digits = ${countDigits(num)}")
    println(s"Input in words = ${numToWords(num, useIndianNumerals, rootLimit)}")
    retry()
    //test(5000)
  }

  case class NumberRepresentation(value: BigInt, name: String) {
    override val toString: String = name

    /**
      * In equality of two number representations, we care only about the number,
      * not the name assigned to it.
      *
      * This may violate the transitivity property of '==',
      * but this is used for notational convenience.
      *
      * @param obj the object to compare for equality with this
      * @return whether `obj` and `this` are equal or not
      */
    override def equals(obj: Any): Boolean = obj match {
      case that: NumberRepresentation => value == that.value
      case that: BigInt => value == that
      case that: Int => value == that
      case _ => false
    }
  }

}
