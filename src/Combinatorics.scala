/**
  * A module containing implicit enhancements to [[scala.Seq]]
  */
object SeqHelpers {

  /**
    * An implicit conversion for [[scala.Seq]] which enhances the base type
    * with methods for removing an element at a specified index
    *
    * @param input the base [[scala.Seq]] to enhance
    */
  implicit final class RicherSeq[A](private val input: Seq[A]) extends AnyVal {
    /**
      * Operator alias of [[RicherSeq.removeAt]]
      *
      * @param index The index of the element to remove
      * @return A copy of this [[scala.Seq]] with the element at index `index` removed
      * @see [[RicherSeq.removeAt]]
      */
    def /-(index: Int): Seq[A] = removeAt(index)

    /**
      * Removes the element at the given index
      *
      * @param index The index of the element to remove
      * @return A copy of this [[scala.Seq]] with the element at index `index` removed
      */
    def removeAt(index: Int): Seq[A] =
      if (index < 0 || index >= input.length)
        throw new IndexOutOfBoundsException(s"$index of ${input.length}")
      else
        input match {
          // Optimize for collections which have a `remove` method already
          case mutable: collection.mutable.Buffer[A] => mutable.remove(index); mutable
          case _ => input.indices.filter(_ != index).map(input(_))
        }

    /**
      * Operator alias of [[RicherSeq.removeAt]]
      *
      * @param element The element to remove
      * @return A copy of this [[scala.Seq]] with the element `element` removed
      * @see [[RicherSeq.removeAt]]
      */
    def -(element: A): Seq[A] = remove(element)

    /**
      * Removes the element `element`
      *
      * @param element The element to remove
      * @return A copy of this [[scala.Seq]] with the element `element` removed
      */
    def remove(element: A): Seq[A] =
      input.filter(_ != element)

    /**
      * Compares this [[scala.Seq]] with another for equality disregarding order
      *
      * @param other The [[scala.Seq]] to compare with this [[scala.Seq]] for order-independent equality
      * @tparam B The type of elements in `other`
      * @return `true` if this [[scala.Seq]] contains all the elements of `other` and they are equal in length, `false` otherwise
      */
    def equalsIgnoreOrder[B >: A](other: Seq[B]): Boolean =
      input.length == other.length &&
      other.forall(input contains _)
  }

  def toPlainString(input: Seq[Any]): String =
    (for (item <- input) yield item match {
      case subSeq: Seq[Any] => toPlainString(subSeq)
      case other => other.toString
    }).mkString("(", ", ", ")")
}

/**
  * A module containing combinatorics utilities
  */
object Combinatorics {

  import SeqHelpers._

  private val lengthOutOfRangeErrorMessage: String = "Length out of range"

  /**
    * Produces all possible length `length` rearrangements of the elements of the `input` [[scala.Seq]].
    * Individual element counts may change.
    *
    * @param input              The [[scala.Seq]] whose rearrangements are to be produced
    * @param length             The length of the rearrangements
    * @param onlyPermutations   Specifies if only permutations, i.e., reorderings are wanted
    * @param padIfNotLongEnough Specifies if the `input` [[scala.Seq]] should be padded if `input.length`&lt;`length`
    * @tparam A The type of elements of `input`
    * @return A list of all possible distinct rearrangements of the elements of `input`
    */
  def rearrange[A](input: Seq[A], length: Int,
                   onlyPermutations: Boolean,
                   padIfNotLongEnough: Boolean): Seq[Seq[A]] =
    if (length < 0 || (!padIfNotLongEnough && length > input.length))
      throw new IllegalArgumentException(lengthOutOfRangeErrorMessage)
    else if (padIfNotLongEnough && length > input.length)
           rearrange(pad(input, length), length, onlyPermutations, padIfNotLongEnough = false)
    else if (length == 0)
           IndexedSeq(IndexedSeq())
    else
      (for (i <- input.indices) yield {
        val head = input(i)
        for (tail <- rearrange(if (onlyPermutations) input /- i else input, length - 1, onlyPermutations, padIfNotLongEnough)) yield head +: tail
      }).flatten.distinct

  // Legacy code - keep it for nostalgia
  /*input match {
    case Seq() => Set(Seq())
    case Seq(x) => Set(Seq(x))
    case Seq(x, y) => Set(Seq(x, y), Seq(y, x))
    case other =>
      val allStarts = for(i <- 0 until other.length) yield (other(i) +: (if (onlyPermutations) (other - i) else other))
      (for(started <- allStarts) yield {
        started match {
          case head +: tail => (for(subTail <- rearrange(tail)) yield head +: subTail).toSet
        }
      }).flatten.distinct
    }*/

   private implicit class LongWithFactorial(val base: Long) extends AnyVal {
    def ! : Long = factorial

    def factorial: Long = {
      if (base < 0) throw new IllegalArgumentException(s"Factorial of a negative integer $base is not defined")

      def factorialAcc(current: Long, acc: Long): Long = current match {
        case 0 => acc
        case _ => acc * factorialAcc(current - 1, current)
      }

      factorialAcc(base, 1)
    }
  }

  def numberOfPermutations(total: Long, selected: Long): Long = if (total >= selected) (total !) / ((total - selected) !) else 0

  def numberOfCombinations(total: Long, selected: Long): Long = numberOfPermutations(total, selected) / (selected !)

  private def constrainedLength(input: Seq[_], padded: Boolean, length: Int) = if (padded) length else input.length

  def permutations[A](input: Seq[A], length: Int,
                      padIfNotLongEnough: Boolean): Seq[Seq[A]] = {
    val result = rearrange(input, length, onlyPermutations = true, padIfNotLongEnough = padIfNotLongEnough)
    require(result.length == numberOfPermutations(constrainedLength(input, padIfNotLongEnough, length), length))
    result
  }

  def permutations[A](input: Seq[A],
                      padIfNotLongEnough: Boolean): Seq[Seq[A]] =
    permutations(input, input.length, padIfNotLongEnough)

  def combinations[A](input: Seq[A], length: Int,
                      padIfNotLongEnough: Boolean): Seq[Seq[A]] = {
    val result = rearrange(input, length, onlyPermutations = true, padIfNotLongEnough = padIfNotLongEnough).map(_.toSet).distinct.map(_.toIndexedSeq)
    require(result.length == numberOfCombinations(constrainedLength(input, padIfNotLongEnough, length), length))
    result
  }

  def combinations[A](input: Seq[A],
                      padIfNotLongEnough: Boolean): Seq[Seq[A]] =
    combinations(input, input.length, padIfNotLongEnough)

  /**
    * Produces all possible rearrangements of the elements of the `input` [[scala.Seq]] of length equal to `input.length`.
    * Individual element counts may change.
    *
    * @param input              The [[scala.Seq]] whose rearrangements are to be produced
    * @param onlyPermutations   Specifies if only permutations, i.e., reorderings are wanted. Default: `false`
    * @param padIfNotLongEnough Specifies if the `input` [[scala.Seq]] should be padded if `input.length`&lt;`length`. Default: `false`
    * @tparam A The type of elements of `input`
    * @return A list of all possible distinct rearrangements of the elements of `input`
    */
  def rearrange[A](input: Seq[A],
                   onlyPermutations: Boolean = false,
                   padIfNotLongEnough: Boolean = false): Seq[Seq[A]] =
    rearrange(input, input.length, onlyPermutations, padIfNotLongEnough)

  /**
    * Pads the `input` [[scala.Seq]] to length `length` by repeating its contents in order.
    *
    * @param input  The [[scala.Seq]] to be padded to length `length`
    * @param length The length to pad (or trim) `input` to
    * @tparam A The type of elements of `input`
    * @return `input` padded to length `length`
    */
  def pad[A](input: Seq[A], length: Int): Seq[A] =
    if (length < 0)
      throw new IllegalArgumentException(lengthOutOfRangeErrorMessage)
    else if (length <= input.length)
           input take length
    else
      input ++: pad(input, length - input.length)

  def cartesianProduct[S](vectors: Traversable[S]*): Traversable[Seq[_]] =
    cartesianProductMultiType(vectors)

  def cartesianProductMultiType(vectors: Traversable[Traversable[_]]): Traversable[Seq[_]] = {
    def cartesianProduct2[A, B](leftVector: IndexedSeq[IndexedSeq[A]], vector2: Traversable[B]): IndexedSeq[IndexedSeq[_]] =
      for {
        superSequence <- leftVector
        element <- vector2
      } yield superSequence :+ element

    vectors.foldLeft(IndexedSeq(IndexedSeq[Any]()))(cartesianProduct2(_, _)).distinct
  }

  def rearrangementsUpTo[A](input: Seq[A], maxLength: Int = -1,
                            onlyPermutations: Boolean = false,
                            padIfNotLongEnough: Boolean): Seq[Seq[A]] = {
    upTo(rearrange(_: Seq[A], _: Int, onlyPermutations, padIfNotLongEnough))(input, maxLength)
  }

  def consecutiveSubSequencesUpTo[A](input: Seq[A], maxLength: Int = -1): Seq[Seq[A]] =
    upTo(consecutiveSubSequences(_: Seq[A], _: Int))(input, maxLength)

  private def upTo[A](producer: (Seq[A], Int) => Seq[Seq[A]])(input: Seq[A], maxLength: Int): Seq[Seq[A]] = {
    val safeMaxLength = if (maxLength < 0) input.length else maxLength
    (0 to safeMaxLength).flatMap(producer(input, _)).distinct
  }

  def consecutiveSubSequences[A](input: Seq[A], length: Int): Seq[Seq[A]] =
    if (length < 0 || length > input.length)
      throw new IllegalArgumentException(lengthOutOfRangeErrorMessage)
    else if (length == 0)
           Seq(Seq())
    else
      (for (i <- input.indices) yield input.slice(i, i + length)).distinct

  def commonConsecutiveSubSequences[A](seq1: Seq[A], seq2: Seq[A]): Seq[Seq[A]] =
    consecutiveSubSequencesUpTo(seq1).intersect(consecutiveSubSequencesUpTo(seq2))

  def longestCommonConsecutiveSubSequence[A](seq1: Seq[A], seq2: Seq[A]): Seq[A] =
    commonConsecutiveSubSequences(seq1, seq2).maxBy(_.length)

  def longestCommonSubSequence(str1: String, str2: String): String =
    longestCommonSubSequence[Char](str1, str2).mkString("")

  import collection.GenSeq

  def longestCommonSubSequence[A](seq1: GenSeq[A], seq2: GenSeq[A]): GenSeq[A] =
    if (seq1.isEmpty || seq2.isEmpty) seq1.companion.empty[A]
    else {
      val (i, j) = (seq1.size - 1, seq2.size - 1)
      if (seq1(i) == seq2(j))
        longestCommonSubSequence(seq1.slice(0, i), seq2.slice(0, j)) :+ seq1(i)
      else
        GenSeq(longestCommonSubSequence(seq1.slice(0, i + 1), seq2.slice(0, j)),
               longestCommonSubSequence(seq1.slice(0, i), seq2.slice(0, j + 1))).maxBy(_.length)
    }
}