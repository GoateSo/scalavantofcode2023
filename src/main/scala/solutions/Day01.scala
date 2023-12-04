package solutions
import utils.Utils.*

// input is a collection of lines
class Day01(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  val alphMap =
    List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight",
      "nine").zipWithIndex.groupMap(_._1)(_._2).mapValues(_.head)

  override def run = // find 1st and last digit
    input
      .map(s => (s("\\d".r)(0).toInt) * 10 + s("(\\d)\\D*?$".r)(0).toInt)
      .sum

  private def findMatch(s: String): Option[Int] =
    alphMap.keys
      .find(s.startsWith)
      .map(alphMap)
      .orElse(s.headOption.filter(_.isDigit).map(_ - '0'))
  override def run2 = // find 1st and last digit/num name
    input
      .map(st =>
        val fst = st.tails
          .collect(findMatch.unlift)
          .next
        val snd = (1 to st.size)
          .map(i => st.sub(-i))
          .collect(findMatch.unlift)
          .head
        fst * 10 + snd
      )
      .sum
