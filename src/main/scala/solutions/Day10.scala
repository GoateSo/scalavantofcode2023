package solutions
import scala.collection.mutable.{HashSet, ListBuffer}
import utils.Utils.*
import math.*
import scala.util.boundary, boundary.break

class Day10(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  val len = input.size
  val wid = input(0).size
  val (si, sj) = boundary:
    for // get start index
      i <- 0 until len
      j <- 0 until wid if input(i)(j) == 'S'
    do break((i, j))
    (-1, -1)
  // start node processing
  def safeGet(x: (Int, Int)): Char = x match
    case (a, b) if a >= 0 && b >= 0 && a < len && b < wid => input(a)(b)
    case _                                                => ' '

  val lb = for // neighbors returns in U, L, R, D order
    ((a, b), set) <- neighbors(si, sj) zip Seq("|F7", "-FL", "-J7", "|LJ")
    if set(safeGet(a, b))
  yield (a - si, b - sj)
  // redo bfs for part 1, and scale by up 2: double the positions, only fill up intermediates for pipes
  val ns = Map( // neighbor offset mapping
    '|' -> Seq((1, 0), (-1, 0)),
    '-' -> Seq((0, 0 - 1), (0, 1)),
    'F' -> Seq((1, 0), (0, 1)),
    'J' -> Seq((-1, 0), (0, -1)),
    '7' -> Seq((1, 0), (0, -1)),
    'L' -> Seq((-1, 0), (0, 1)),
    'S' -> lb
  )

  var dist = -1 // added iteration for start
  val q    = ListBuffer((si, sj))
  val vis  = HashSet[(Int, Int)]()
  val repr = Array.fill(len * 2, wid * 2)(' ')
  while q.nonEmpty do
    dist += 1
    q.flatMapInPlace: (a, b) =>
      if vis(a, b) then Nil
      else // part 1: do bfs
        vis.add(a, b)
        var neighbors = safeGet(a, b) match
          case x if ns.contains(x) => // remove already seen node(s)
            ns(x) map (_.bimap(a + _, b + _)) filterNot vis
          case _ => Nil
        // part 2: fill in intermediates in repr (keeping scaling in mind)
        for (x, y) <- (a, b) +: neighbors do repr(a + x)(b + y) = '#'
        neighbors
  // count of spaces inside loop (1 entry per Conn. Comp. cnt = 0 if outside loop)
  val cnts = for
    i <- 0 until len * 2
    j <- 0 until wid * 2
    if !"+#" (repr(i)(j)) // #, + pipe or visited
  yield                   // floodfill for CC size
    val stk = ListBuffer((i, j))
    repr(i)(j) = '+'
    var contained  = true
    var spaceCount = 0
    while stk.nonEmpty do
      val (x, y) = stk remove 0
      val ns     = neighbors(x, y) bound (len * 2, wid * 2)
      contained &&= ns.size == 4 // <= 3 neighbors => at outside
      for (a, b) <- ns if repr(a)(b) != '#' && repr(a)(b) != '+' do
        spaceCount += 1 - max(a % 2, b % 2) // whole space only, not half space
        repr(a)(b) = '+'
        stk.addOne(a, b)
    if contained then spaceCount else 0

  def run  = dist
  def run2 = cnts.sum
