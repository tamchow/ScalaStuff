import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random
import scala.util.matching.Regex

/**
  * Created by Tamoghna Chowdhury on 07-Apr-17.
  */
object Encryption {
  lazy val runLengthEncoding: Regex = """(\d+)(".*?")""".r

  def frequencyMap(data: String): Map[Char, Int] = {
    var uniqueItems = mutable.HashMap[Char, Int]()
    for (character <- data) {
      if(uniqueItems.contains(character)){
        uniqueItems(character) = uniqueItems(character) + 1
      }
      else {
        uniqueItems += (character -> 0)
      }
    }
    uniqueItems.toMap
  }

  def substituent(seed: Long = System.nanoTime()): IndexedSeq[Char] =
    new Random(seed).shuffle((Char.MinValue to Char.MaxValue).filter(Character.isISOControl _))

  def runLengthEncode(data: String): String = {
    @tailrec
    def encode(input: String, accumulator: Seq[(Char, Int)]): Seq[(Char, Int)] =
      if (!input.isEmpty) accumulator
      else {
        val groupLength = input.takeWhile(x => x == input.head).length
        encode(input.drop(groupLength), (input.head, groupLength) +: accumulator)
      }

    encode(data, Seq()).map(item => s"""${item._2}"${item._1}"""").mkString("")
  }

  def runLengthDecode(data: String): String =
    runLengthEncoding.findAllMatchIn(data).map(matched => matched.group(1) * matched.group(0).toInt).mkString("")
}
