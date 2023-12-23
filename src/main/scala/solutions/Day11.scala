package solutions
import math.*
import utils.Utils.chrCols
class Day11(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  val transpose = input.chrCols
  // psums for distance scaling
  var vcnt, hcnt = 0
  val vert       = Array.fill(input.size)(0)
  val hori       = Array.fill(transpose.size)(0)
  for i <- 0 until input.size do
    if input(i).forall(_ == '.') then vcnt += 1
    vert(i) = vcnt
  for i <- 0 until transpose.size do
    if transpose(i).forall(_ == '.') then hcnt += 1
    hori(i) = hcnt
  // get galaxy indicies
  val galaxies = for
    i <- 0 until input.size
    j <- 0 until input(0).size
    if input(i)(j) == '#'
  yield (i, j)
  // get all distances between pairs for given scale factor
  def dists(expFactor: Long) = for
    i <- 0 until galaxies.size
    j <- i + 1 until galaxies.size
    (y1, x1) = galaxies(i)
    (y2, x2) = galaxies(j)
  yield
    val mult = expFactor - 1
    // distance + addn offset from expansion
    abs(y2 - y1) + abs(x2 - x1)
      + abs(vert(y2) - vert(y1)) * mult + abs(hori(x2) - hori(x1)) * mult
  override def run: Any = dists(2).sum

  override def run2: Any = dists(1_000_000).sum
