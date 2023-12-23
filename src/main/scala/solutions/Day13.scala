package solutions
import math.*
import utils.Utils.*
class Day13(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  val grids = input.splitBy("")
  // evaluating differences
  def diffs(g1: Seq[String], g2: Seq[String]) =
    for
      i <- 0 until g1.size
      j <- 0 until g1(i).size
      if g1(i)(j) != g2(i)(j)
    yield (i, j)

  var sum, sum2 = 0 // unsmuged and smudged resp.
  // fing reflections and sum up prev row/col * mult
  def findRefl(grid: Seq[String], mult: Int) =
    for i <- 1 until grid(0).size do
      val minLen = min(i, grid(0).size - i)
      var l      = grid.map(_.take(i).takeRight(minLen))
      var r      = grid.map(_.drop(i).take(minLen).reverse)
      val ss     = diffs(l, r)
      if l == r then sum += i * mult        // direct equality
      if ss.size == 1 then sum2 += i * mult // smudge equality

  for grid <- grids do
    findRefl(grid, 1) // vertical refls lines
    val flip = grid.chrCols
    findRefl(flip, 100) // transpose to find horizontal refl lines
  override def run: Any = sum

  override def run2: Any = sum2
