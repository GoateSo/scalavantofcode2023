package solutions
import utils.Utils.lcm

class Day08(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  val (dir +: _ +: rest) = input: @unchecked
  val ps = rest.map { case s"$p = ($l, $r)" =>
    (p, Map('L' -> l, 'R' -> r))
  }.toMap

  def simulate(start: String, p2: Boolean): Long =
    var cur = start
    var i   = 0
    while (!p2 && cur != "ZZZ") || (p2 && cur(2) != 'Z') do
      cur = ps(cur)(dir(i % dir.size))
      i += 1
    i

  def run = simulate("AAA", false)

  def run2 =
    val as  = ps.keys.filter(_(2) == 'A').toArray
    val cnt = as.size
    // relies on path from A->Z being the same as paths Z->Z which somehow holds true although is not mentioned
    val ocrs = for i <- 0 until cnt yield simulate(as(i), true)
    ocrs.reduce(lcm)
