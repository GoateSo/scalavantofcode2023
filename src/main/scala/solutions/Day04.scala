package solutions
import utils.Utils.*
import math.*

class Day04(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  val process = (s: String) => s.trim.split("\\s+").map(_.trim.toInt)
  val lines =
    input.map(_(".+: (.+) \\| (.+)".r)).collect { case List(xs, ys) =>
      (process(xs).toList, process(ys).toSet)
    }
  override def run =
    (for (xs, ys) <- lines yield pow(2, xs.count(ys) - 1).toInt).sum

  override def run2 =
    val as = Array.fill[Int](lines.size)(0)
    for
      i <- 0 until input.size
      (xs, ys) = lines(i)
    do
      as(i) += 1
      for card <- 1 to math.min(xs.count(ys), input.size - i - 1)
      do as(i + card) += as(i)
    as.sum
