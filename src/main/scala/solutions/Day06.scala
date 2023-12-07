package solutions
import math.*
import utils.Utils.*

class Day06(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  val lines = input.map(_.split("\\s+").toSeq)
  val ts    = lines(0).drop(1).map(_.toLong)
  val ds    = lines(1).drop(1).map(_.toLong)

  def calc(d: Long, t: Long) =
    val sdisc = sqrt(1.0 * t * t - 4 * d)
    val r     = ((t + sdisc) / 2).toLong
    val l     = ((t - sdisc) / 2).ceil.toLong
    r - l + 1 - (l * (t - l) == d) - (r * (t - r) == d)

  def run = ds.zip(ts).map(calc).product

  def run2 = calc(ds.mkString.toLong, ts.mkString.toLong)
