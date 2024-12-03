package solutions
import utils.Utils.*
import scala.collection.mutable.{Stack, HashMap}
import scala.util.boundary, boundary.break

class Day17(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  val rows = input.size
  val cols = input(0).size
  val grid = input.map(_.map(_ - '0').toArray).toArray

  def path(start: Int, end: Int) =
    val pq = MinPq[((Int, Int), Boolean)](((0, 0), true), 0)
    pq += (((0, 0), false), 0)
    // seperate dist for vertical and horizontal
    val hd = Array.fill[Int](rows, cols)(1_000_000)
    val vd = Array.fill[Int](rows, cols)(1_000_000)
    hd(0)(0) = 0
    vd(0)(0) = 0
    boundary: // alternative Hori-Vert dijkstra
      while pq.arr.length > 1 do
        val (((i, j), isHoriz), nDist) = pq.pop
        if (if isHoriz then hd else vd).apply(i)(j) < nDist then ()
        else if i == rows - 1 && j == cols - 1 then break(nDist.toInt)
        else
          // get direction of travel
          // add stuff to all 2 other directions
          //  go thru each of the possible lengths in ench of the directions
          for (a, b) <-
              if isHoriz
              then List((-1, 0), (1, 0))
              else List((0, -1), (0, 1))
          do
            var dsum = nDist // accum for distance so far
            for
              m <- 1 to end
              ni = i + a * m
              nj = j + b * m if ni >= 0 && nj >= 0 && ni < rows && nj < cols
            do // multiplier - get next elem, check if its in bounds
              dsum += grid(ni)(nj)
              if m >= start then
                val dmap = if a == 0 then hd else vd
                if dsum < dmap(ni)(nj) then
                  dmap(ni)(nj) = dsum.toInt
                  pq += (((ni, nj), !isHoriz), dsum)
      -1

  def run: Any = path(1, 3)

  def run2: Any = path(4, 10)
