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
    def equalsIgnoreOrder[B >: A](other: Seq[B]): Boolean = input.length == other.length && other.forall(element => input contains element)

    def flattenAnyTo2D: Seq[_] = {
      val acc = collection.mutable.ListBuffer[Any]()
      for (item <- input) item match {
        case seq: Seq[_] =>
          val subAcc = collection.mutable.ListBuffer[Any]()
          for (seqItem <- seq) seqItem match {
            case subSeq: Seq[_] =>
              for (subSeqItem <- subSeq) {
                subAcc += subSeqItem
              }
            case subSingle => subAcc += subSingle
          }
          acc += subAcc.toList
        case single =>
          acc += single
      }
      acc.toList
    }
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

  // I HATE MYSELF! WHY! OH WHY!
  def cartesianProduct(vectors: Seq[Seq[_]]): Seq[Seq[_]] = {
    def cartesianProduct2(leftVector: Seq[_], vector2: Seq[_]): Seq[Seq[_]] = {
      val wrapItemsInSeq = (_: Seq[_]).map(IndexedSeq(_))
      if (leftVector.isEmpty && vector2.isEmpty)
        Seq(IndexedSeq())
      else if (vector2.isEmpty)
             wrapItemsInSeq(leftVector)
      else if (leftVector.isEmpty)
             wrapItemsInSeq(vector2)
      else
        (for {
          element1 <- leftVector
          element2 <- vector2
        } yield IndexedSeq(element1, element2)).distinct
    }

    def cartesianProduct2Flatten(leftVector: Seq[_], vector2: Seq[_]): Seq[Seq[_]] =
      cartesianProduct2(leftVector.flattenAnyTo2D, vector2).flattenAnyTo2D.asInstanceOf[Seq[Seq[Any]]]

    val length = vectors.length
    if (vectors.isEmpty)
      Seq(IndexedSeq())
    else if (length == 1)
           cartesianProduct2(vectors.head, vectors.head)
    else if (length == 2)
           cartesianProduct2(vectors(0), vectors(1))
    else
      vectors.reduceLeft(cartesianProduct2Flatten(_, _)).distinct.asInstanceOf[Seq[Seq[Any]]]
  }

  def rearrangementsUpTo[A](input: Seq[A], maxLength: Int,
                            onlyPermutations: Boolean = false,
                            padIfNotLongEnough: Boolean): Seq[Seq[A]] =
    upTo(rearrange(_: Seq[A], _: Int, onlyPermutations, padIfNotLongEnough))(input, maxLength)

  def subSequencesUpTo[A](input: Seq[A], maxLength: Int): Seq[Seq[A]] =
    upTo(subSequences(_: Seq[A], _: Int))(input, maxLength)

  private def upTo[A](producer: (Seq[A], Int) => Seq[Seq[A]])(input: Seq[A], maxLength: Int): Seq[Seq[A]] =
    (0 to maxLength).flatMap(length => producer(input, length)).distinct

  def subSequences[A](input: Seq[A], length: Int): Seq[Seq[A]] =
    if (length < 0 || length > input.length)
      throw new IllegalArgumentException(lengthOutOfRangeErrorMessage)
    else if (length == 0)
           Seq(Seq())
    else
      (for (i <- input.indices) yield input.slice(i, i + length)).distinct
}
