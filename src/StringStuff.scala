import scala.annotation.tailrec

/**
  * Random [[java.lang.String]] things.
  */
object StringStuff {
  def occurrencesOf(target: Set[Char])(text: String): IndexedSeq[Int] =
    for ((character, index) <- text.zipWithIndex if target.contains(character)) yield index

  def groupFirstLast[T](list: Seq[T]): Map[T, T] = {
    @tailrec
    def group(list: Seq[T], accumulator: Map[T, T]): Map[T, T] = list match {
      case Seq() => accumulator
      case Seq(a) => accumulator ++ Map(a -> a)
      case _ => group(list.tail.init, accumulator + (list.head -> list.last) + (list.last -> list.head)) // We need this to go both ways
    }
    group(list, Map())
  }

  def bothCases(characters: Seq[Char]): Seq[Char] =
    (for (character <- characters) yield Seq(character.toLower, character.toUpper)).flatten

  def reverseTarget(target: Set[Char], text: String): String = {
    val matchedIndices = groupFirstLast(occurrencesOf(target)(text))
    (for ((character, index) <- text.zipWithIndex) yield {
      if (target.contains(character)) text(matchedIndices(index)) else character
    }).mkString("")
  }
}
