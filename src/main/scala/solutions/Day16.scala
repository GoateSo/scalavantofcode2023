package solutions
import utils.Utils.*
import scala.collection.mutable.{Stack, HashSet}

class Day16(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  val rows = input.size
  val cols = input(0).size
  // simulate walks of light rays, memoize same dir same pos
  def sim(y: Int, x: Int, dy: Int, dx: Int): Int =
    val stk     = Stack((y, x, dy, dx))
    val memo    = HashSet[(Int, Int, Int, Int)]()
    val visited = Array.fill(input.size, input(0).size)(false)
    while stk.nonEmpty do
      val (y, x, dy, dx) = stk.pop
      if x >= 0 && y >= 0 && y < input.size && x < input(0).size
        && !memo(y, x, dy, dx) // bounded and path hasn't been trodden
      then
        visited(y)(x) = true
        memo.add(y, x, dy, dx)
        input(y)(x) match
          case '|' => // either split if perp. or pass thru
            if dy == 0 then stk ++= List((y + 1, x, 1, 0), (y - 1, x, -1, 0))
            else stk.push((y + dy, x + dx, dy, dx))
          case '-' =>
            if dx == 0 then stk ++= List((y, x + 1, 0, 1), (y, x - 1, 0, -1))
            else stk.push((y + dy, x + dx, dy, dx))
          case '/'  => stk.push((y - dx, x - dy, -dx, -dy)) // mirror
          case '\\' => stk.push((y + dx, x + dy, dx, dy))
          case _    => stk.push((y + dy, x + dx, dy, dx))   // let it be
    visited.sumBy(_.count(identity))
  def run = sim(0, 0, 0, 1) // top left going right

  def run2 = // try all edge configs
    // left going right, up going down, right going left, down going up
    val lefts = for i <- 0 until rows yield sim(i, 0, 0, 1)
    val ups   = for i <- 0 until cols yield sim(0, i, 1, 0)
    val right = for i <- 0 until rows yield sim(i, cols - 1, 0, -1)
    val downs = for i <- 0 until cols yield sim(rows - 1, i, -1, 0)
    List(lefts, ups, right, downs).flatten.max
