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

  def sortByCloseness(key: String, inputs: Seq[String]): Seq[String] =
    inputs.sortBy(editDistance(key, _))

  def editDistance(s1: String, s2: String): Int = damerauLevenshteinDistance(s1, s2)

  def damerauLevenshteinDistance(s1: String, s2: String): Int = {
    if (s1 == s2) return 0
    // INFinite distance is the max possible distance
    val inf = s1.length + s2.length
    // Create and initialize the character array indices
    val da = collection.mutable.HashMap[Char, Int]()
    da ++= s1.union(s2).map(_ -> 0)
    // Create the distance matrix H
    val h = Array.ofDim[Int](s1.length + 2, s2.length + 2)
    // initialize the left and top edges of H
    for (i <- 0 to s1.length) {
      h(i + 1)(0) = inf
      h(i + 1)(1) = i
    }
    for (j <- 0 to s2.length) {
      h(0)(j + 1) = inf
      h(1)(j + 1) = j
    }
    // fill in the distance matrix H
    // look at each character in s1
    for (i <- 1 to s1.length) {
      var db = 0
      // look at each character in b
      for (j <- 1 to s2.length) {
        val (i1, j1) = (da(s2(j - 1)), db)
        val cost =
          if (s1(i - 1) == s2(j - 1)) {
            db = j
            0
          } else 1
        h(i + 1)(j + 1) = Seq(h(i)(j) + cost, /*substitution*/
                              h(i + 1)(j) + 1, /*insertion*/
                              h(i)(j + 1) + 1, /*deletion*/
                              h(i1)(j1) + (i - i1) + (j - j1) - 1 /*transposition*/).min
      }
      da.put(s1.charAt(i - 1), i)
    }
    h(s1.length + 1)(s2.length + 1)
  }

}
