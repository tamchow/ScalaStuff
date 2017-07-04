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
    def -(index: Int): Seq[A] = removeAt(index)

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
          case _ => input.indices.filter(_ != index).map(i => input(i))
        }

    /**
      * Compares this [[scala.Seq]] with another for equality disregarding order
      *
      * @param other The [[scala.Seq]] to compare with this [[scala.Seq]] for order-independent equality
      * @tparam B The type of elements in `other`
      * @return `true` if this [[scala.Seq]] contains all the elements of `other` and they are equal in length, `false` otherwise
      */
    def equalsIgnoreOrder[B >: A](other: Seq[B]): Boolean =
      input.length == other.length &&
      other.forall(element => input contains element)
  }

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
           Seq(Seq())
    else
      (for (i <- input.indices) yield {
        val head = input(i)
        for (tail <- rearrange(if (onlyPermutations) input - i else input, length - 1, onlyPermutations, padIfNotLongEnough)) yield head +: tail
      }).flatten.distinct

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

  // Legacy code - keep it for nostalgia
  /*input match {
    case Seq() => Set(Seq())
    case Seq(x) => Set(Seq(x))
    case Seq(x, y) => Set(Seq(x, y), Seq(y, x))
    case other =>
      val allStarts = for(i <- 0 until other.length) yield (other(i) +: (if (onlyPermutations) (other - i) else other))
      (for(started <- allStarts) yield {
        started match {
          case head +: tail => (for(subtail <- rearrange(tail)) yield head +: subtail).toSet
        }
      }).flatten.distinct
    }*/

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

  def subSequencesUpTo[A](input: Seq[A], maxLength: Int = -1): Seq[Seq[A]] =
    upTo(subSequences(_: Seq[A], _: Int))(input, maxLength)

  private def upTo[A](producer: (Seq[A], Int) => Seq[Seq[A]])(input: Seq[A], maxLength: Int): Seq[Seq[A]] = {
    val safeMaxLength = if (maxLength < 0) input.length else maxLength
    (0 to safeMaxLength).flatMap(producer(input, _)).distinct
  }

  def subSequences[A](input: Seq[A], length: Int): Seq[Seq[A]] =
    if (length < 0 || length > input.length)
      throw new IllegalArgumentException(lengthOutOfRangeErrorMessage)
    else if (length == 0)
           Seq(Seq())
    else
      (for (i <- input.indices) yield input.slice(i, i + length)).distinct

  def commonSubSequences[A](seq1: Seq[A], seq2: Seq[A]): Seq[Seq[A]] =
    subSequencesUpTo(seq1).union(subSequencesUpTo(seq2)

  def longestCommonSubSequence[A](seq1: Seq[A], seq2: Seq[A]): Seq[A] =
    commonSubSequences(seq1, seq2).maxBy(_.length)
}
