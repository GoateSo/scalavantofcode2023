package solutions
import utils.Utils.*

class Day18(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  val lines = input
    .map(_.split(" ") match
      case Array(a, b, c) => (a, b, c)
    )

  def area(verts: Seq[(Long, Long)]): Long =
    verts
      .sliding(2)
      .toList // love thy shoelace
      .sumBy { case Seq((y1, x1), (y2, x2)) =>
        x1 * y2 - y1 * x2
      } / 2
  def run: Any =
    val dmap = Map('L' -> (0, -1), 'R' -> (0, 1), 'U' -> (-1, 0), 'D' -> (1, 0))
    var i, j, p = 0L
    val verts = for
      (dir, amt, _) <- lines
      (di, dj) = dmap(dir(0))
    yield
      i += di * amt.toInt
      j += dj * amt.toInt
      p += amt.toInt
      (i, j)
    area(verts) + p / 2 + 1

  def run2: Any =
    // 0=R, 1=D, 2=L, 3=U
    val dmap    = Array((0, 1), (1, 0), (0, -1), (-1, 0))
    var i, j, p = 0L
    val verts = for
      (_, _, code) <- lines
      (amtStr :+ dir) = code.sub(2, -2).toSeq: @unchecked
      (di, dj)        = dmap(dir - '0')
    yield
      val amt = amtStr.mkString.toInt(16)
      i += di * amt
      j += dj * amt
      p += amt
      (i, j)
    area(verts) + p / 2 + 1
