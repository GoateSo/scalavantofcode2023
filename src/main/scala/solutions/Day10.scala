package solutions
import scala.collection.mutable.{HashSet, ListBuffer, ArraySeq}
import utils.Utils.*

class Day10(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  val L      = input.size
  val W      = input(0).size
  var si, sj = 0

  def safeGet(x: (Int, Int)): Option[Char] = x match
    case (a, b) =>
      if a < 0 || b < 0 || a >= L || b >= W then None
      else Some(input(a)(b))
  for
    i <- 0 until L
    j <- 0 until W if input(i)(j) == 'S'
  do
    si = i
    sj = j
  // start node processing
  val lb = ListBuffer[(Int, Int)]()
  if safeGet(si, sj - 1).exists("-FL".toSet) then lb.append((si, sj - 1))
  if safeGet(si, sj + 1).exists("-J7".toSet) then lb.append((si, sj + 1))
  if safeGet(si - 1, sj).exists("|F7".toSet) then lb.append((si - 1, sj))
  if safeGet(si + 1, sj).exists("|LJ".toSet) then lb.append((si + 1, sj))

  // redo bfs for part 1, and scale by up 2?
  // double the positions, only fill up intermediates for pipes
  val dirs = Map(
    '|' -> List((0 + 1, 0), (0 - 1, 0)),
    '-' -> List((0, 0 - 1), (0, 0 + 1)),
    'F' -> List((0 + 1, 0), (0, 0 + 1)),
    'J' -> List((0 - 1, 0), (0, 0 - 1)),
    '7' -> List((0 + 1, 0), (0, 0 - 1)),
    'L' -> List((0 - 1, 0), (0, 0 + 1))
  )

  var dist = -1 // added iter for start
  var q    = ListBuffer((si, sj))
  val vis  = HashSet[(Int, Int)]()
  val repr = Array.fill(L * 2, W * 2)(' ')
  while !q.isEmpty do
    dist += 1
    q.flatMapInPlace { (a, b) =>
      if vis((a, b)) then Nil
      else
        vis.add((a, b))
        var ret = (safeGet(a, b) match
          case Some('S') => lb.toList
          case Some(x) if dirs.contains(x) =>
            dirs(x).map((dx, dy) => (a + dx, b + dy))
          case _ => Nil
        ).filterNot(vis)
        if !(a < 0 || b < 0 || a >= L || b >= W) then
          repr(a * 2)(b * 2) = '#'
          // fill in intermediate
          for (x, y) <- ret do repr(a + x)(b + y) = '#'
        ret
    }
  repr(si * 2)(sj * 2) = '#'

  def run = dist
  def run2 =
    val cnts = for
      i <- 0 until L * 2
      j <- 0 until W * 2
      if repr(i)(j) != '+' && repr(i)(j) != '#'
    yield
      val stk = ListBuffer((i, j)) // floodfill
      repr(i)(j) = '+'
      var contained = true
      var lcnt      = 0
      while stk.nonEmpty do
        val (x, y) = stk.remove(0)
        val ns     = neighbors(x, y).bound(L * 2, W * 2)
        contained = contained && ns.size == 4
        for (a, b) <- ns if repr(a)(b) != '#' && repr(a)(b) != '+' do
          lcnt += math.min(1 - a % 2, 1 - b % 2) // only if both even
          repr(a)(b) = '+'
          stk.addOne((a, b))
      if contained then lcnt else 0
    cnts.sum
