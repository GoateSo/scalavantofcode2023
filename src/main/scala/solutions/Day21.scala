package solutions

import scala.collection.mutable.HashSet
import utils.Utils.*

class Day21(input: Seq[String], samp: Boolean) extends Solution(input, samp):

  val grid = input.map(_.toIndexedSeq).toIndexedSeq
  val n    = grid.size
  val m    = grid(0).size

  val queue = for
    i <- 0 until n
    j <- 0 until m if grid(i)(j) == 'S'
  yield (i, j)

  private def bfs(steps: Int) = go(steps, HashSet.from(queue))

  private def go(
      steps: Int,
      ps: HashSet[(Int, Int)]
  ): Int =
    if steps == 0 then ps.size
    else
      go(
        steps - 1,
        ps.flatMap { (a, b) =>
          List(
            ((a + 1) +% n, b),
            ((a - 1) +% n, b),
            (a, (b - 1) +% n),
            (a, (b + 1) +% n)
          ).filter((a, b) => grid(a)(b) != '#')
        }
      )

  def run: Any  = bfs(if samp then 6 else 64)
  def run2: Any = 1

end Day21
