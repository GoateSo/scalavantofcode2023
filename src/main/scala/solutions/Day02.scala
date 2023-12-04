package solutions
import utils.Utils.*
import scala.collection.mutable.Map
// input is a collection of lines
class Day02(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  val games = input.map(
    _("Game \\d+: (.+)".r)(0)            // decompose string
      .split(";")                        // split by subset lists
      .map(_.split(", ").map(_.strip))   // get each subset
      .flatMap(_.flatMap(_.split(", "))) // process into (cnt, color) pair
  )
  override def run =
    val maxCnt = Map('r' -> 12, 'g' -> 13, 'b' -> 14)
    val ids = for
      (game, ind) <- games.zipWithIndex
      if game.forall { case s"$cnt $color" => cnt.toInt <= maxCnt(color(0)) }
    yield ind + 1
    ids.sum

  override def run2 =
    val powers =
      for game <- games
      yield
        val maxs     = Map('r' -> 0, 'g' -> 0, 'b' -> 0)
        var r, g, bl = 0
        for
          case s"$n $color" <- game
          cnt = n.toInt
        do maxs(color(0)) = maxs(color(0)).max(cnt)
        maxs.values.product
    powers.sum
