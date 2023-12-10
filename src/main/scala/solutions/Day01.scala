package solutions
import utils.Utils.*

// input is a collection of lines
class Day01(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  val alphMap = "zero one two three four five six seven eight nine"
    .split(" ")
    .zipWithIndex
    .toMap

  private def findMatch(s: String): Option[Int] =
    alphMap.keys
      .find(s.startsWith)
      .map(alphMap)
      .orElse(s.headOption.filter(_.isDigit).map(_ - '0'))

  override def run = // find 1st and last digit
    input
      .sumBy(s => (s("\\d".r)(0).toInt) * 10 + s("(\\d)\\D*?$".r)(0).toInt)
  override def run2 = // find 1st and last digit/num name
    input.sumBy: st =>
      val fst = st.tails
        .collect(findMatch.unlift)
        .next
      val snd = (1 to st.size)
        .map(i => st.sub(-i))
        .collect(findMatch.unlift)
        .head
      fst * 10 + snd
