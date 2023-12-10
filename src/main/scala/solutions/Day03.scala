package solutions
import utils.Utils.*
// input is a collection of lines
class Day03(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  val a = input.length
  val b = input(0).length

  val parts = for
    i <- 0 until a
    j <- 0 until b if !input(i)(j).isDigit && input(i)(j) != '.'
  yield (i, j)

  val visited = Array.fill(a, b)(false)
  val mapping =
    for (i, j) <- parts yield
      val pnums = // get all adj numbers
        for
          (y, x) <- surrounding(i, j).bound(a, b)
          if !visited(y)(x) && input(y)(x).isDigit // is number & unvisited
        yield
          // get left, right bounds
          var l, r = x
          while l >= 0 && input(y)(l).isDigit do l -= 1
          while r < a && input(y)(r).isDigit do r += 1
          // update visited set
          for i <- l + 1 until r do visited(y)(i) = true
          // extract number from bounds
          input(y).mkString.sub(l + 1, r).toInt
      (input(i)(j), pnums)

  override def run =
    // sum of part numbers
    mapping.sumBy(_._2.sum)

  override def run2 =
    // sum of pnum1 * pnum2 next to gears
    mapping.filter(x => x._1 == '*' && x._2.size == 2).sumBy(_._2.product)
