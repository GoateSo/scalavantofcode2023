package solutions
import utils.Utils.*
import scala.collection.mutable.Map
// input is a collection of lines
class Day02(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  // break down into arrays of (count, color) pairs
  val games = input map: x =>
    for
      line  <- x(".+?: (.+)".r)(0).split("; ")
      query <- line.split(", ")
    yield query

  override def run =
    val maxCnt = Map('r' -> 12, 'g' -> 13, 'b' -> 14)
    val ids = for
      (game, ind) <- games.zipWithIndex
      if game.forall { case s"$cnt $color" => cnt.toInt <= maxCnt(color(0)) }
    yield ind + 1
    ids.sum

  override def run2 =
    val powers = for game <- games yield
      val maxs     = Map('r' -> 0, 'g' -> 0, 'b' -> 0)
      var r, g, bl = 0
      for case s"$n $color" <- game
      do maxs(color(0)) = maxs(color(0)).max(n.toInt)
      maxs.values.product
    powers.sum
