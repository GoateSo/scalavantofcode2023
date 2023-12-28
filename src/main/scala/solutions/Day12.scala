package solutions
import scala.collection.mutable.HashMap
import utils.Utils.*
class Day12(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  // monke top down soln
  val memo = HashMap[(List[Char], List[Int], Bool), Long]()
  def solns(str: List[Char], cnts: List[Int], instr: Bool): Long =
    memo.getOrElse(
      (str, cnts, instr), {
        val ret: Long = str match
          case Nil => if !cnts.forall(_ == 0) then 0 else 1
          case '#' :: rest => // subtract one from cnts(0), assert positivity
            cnts match
              case x :: rem if x > 0 => solns(rest, x - 1 :: rem, true)
              case _                 => 0
          case '.' :: rest => // assert first is empty and pop first
            cnts match
              case 0 :: rem => solns(rest, rem, false)
              case _        => if instr then 0 else solns(rest, cnts, false)
          case '?' :: rest => // try both sides
            solns('#' :: rest, cnts, instr) + solns('.' :: rest, cnts, instr)
          case _ => 0
        memo.addOne((str, cnts, instr), ret)
        ret
      }
    )

  val (l, r) = input.map(_.span(!_.isSpaceChar)).unzip
  // folded map
  val lens   = r.map(_.split(",").map(_.trim.toInt).toList)
  val onsens = l.map(_.toList)
  // unfolded map
  val five     = (1 to 5).toList
  val fullLens = lens.map(x => five.flatMap(_ => x))
  val fullOnsens = // join with ?
    onsens.map(x => five.map(_ => x).reduceRight(_ ::: '?' :: _))
  def run = onsens
    .zip(lens)
    .sumBy(solns(_, _, false))
  def run2 = fullOnsens
    .zip(fullLens)
    .sumBy: (a, b) =>
      memo.clear()
      solns(a, b, false)
