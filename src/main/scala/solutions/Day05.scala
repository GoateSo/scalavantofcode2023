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
  def minloc(range: (Long, Long)) = // each step of the process
    mappings
      .foldLeft(List(range)) { case (xs, map) =>
        // for each range, find all mappings that overlap with it, and add those to the list
        xs.flatMap { (a, right) =>
          var left = a // mutable left ptr 
          val nrangs = for // get overlapping maps
            case Array(d, lb, len) <- map if lb <= right
            rb    = lb + len - 1
            shift = d - lb
            (l1, r1) <- List(
              (max(left, lb) + shift, min(right, rb) + shift), // overlap portion
              (left, lb - 1) // left bit (might be empty)
            ) if l1 <= r1
          yield
            left = math.max(left, math.min(right, rb + 1))
            (l1, r1)
          if nrangs.isEmpty then List((a, right)) else nrangs // in case no mapping
        }
      }
      .map(_._1)
      .min

  override def run =
    seeds.map(x => minloc(x, x)).min

  override def run2 = // minimum of starts (1st elem)
    seeds.sliding(2, 2).map { case Array(a, b) => minloc(a, a + b - 1) }.min