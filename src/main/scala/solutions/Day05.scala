package solutions
import scala.collection.mutable.ListBuffer
import utils.Utils.*
import math.*
import os.*

class Day05(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  val (ss +: rest) = input: @unchecked
  // 1st line, split by space, drop description, map to numbers
  val seeds = ss.split(" ").tail.map(_.toLong)
  val mappings = rest
    .splitBy(_.isEmpty)
    .tail // get rid of newline
    .map(
      _.tail                             // get rid of map desc
        .map(_.split(" ").map(_.toLong)) // convert into Array(a,b,c)
        .sortBy(_(1))                    // sort by b (starting pos)
    )
  // part 2, find minimum location treating inputs as ranges
  def locs(range: (Long, Long)) = // each step of the process
    mappings
      .foldLeft(List(range)) { case (xs, map) =>
        // for each range, find all mappings that overlap with it, and add those to the list
        xs.flatMap { (a, r) =>
          var l = a // mutable left ptr
          val nrangs = for // get overlapping maps
            Array(d, lb, len) <- map
            rb = lb + len - 1 if lb <= r && rb >= l // actual overlap
            sh = d - lb                             // shift amt
            (l1, r1) <- List( // get mapped ranges (and guaranteed unmapped)
              (max(l, lb) + sh, min(r, rb) + sh), // overlap portion
              (l, lb - 1)                         // left bit (might be empty)
            ) if l1 <= r1
          yield
            l = max(l, min(r, rb + 1)) // update left ptr
            (l1, r1)                   // yield processed range
          // in case no mappings, return same range
          if nrangs.nonEmpty then nrangs else List((a, r))
        }
      }
      .map(_._1)
  override def run =
    seeds.flatMap(x => locs(x, x)).min
  override def run2 = // minimum of starts (1st elem)
    seeds.grouped(2).flatMap { case Array(a, b) => locs(a, a + b - 1) }.min
