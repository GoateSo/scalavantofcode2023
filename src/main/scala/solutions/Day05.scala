package solutions
import scala.collection.mutable.ListBuffer
import utils.Utils.*
import math.*
import os.*

class Day05(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  val (ss, rest) = input.splitAt(1)
  val seeds =
    ss(0) // 1st line, split by space
      .split(" ")
      .drop(1)       // drop description
      .map(_.toLong) // map to numbers
  val mappings = rest
    .splitBy(_.isEmpty())
    .drop(1) // get rid of newline
    .map(
      _.drop(1)                          // get rid of map desc
        .map(_.split(" ").map(_.toLong)) // convert into Array(a,b,c)
        .sortBy(_(1))                    // sort by b (starting pos)
    )

  // part 2, get pairs and turn to ranges
  val s2 = seeds.sliding(2, 2).map { case Array(a, b) => (a, a + b - 1) }

  // part 2, find minimum location treating inputs as ranges
  def minloc(range: (Long, Long)) =
    // each step of the process
    mappings
      .foldLeft(List(range)) { case (xs, map) =>
        // for each range, find all mappings that overlap with it, and add those to the list
        xs.flatMap { (a, r) =>
          var l = a // mutable copy
          val nrangs = map.flatMap { case Array(d, lb, len) =>
            val rb = lb + len - 1
            val shift = d - lb
            // find overlap, update l and r (maps should be sorted by s = l1)
            val ret =
              if r >= lb then
                List(
                  (max(l, lb) + shift, min(r, rb) + shift),
                  (l, lb - 1)
                ).filter(_ <= _)
              else Nil
            l = math.max(l, math.min(r, rb + 1)) // update left value
            ret
          }
          // in case no mapping
          if nrangs.isEmpty then List((a, r)) else nrangs
        }
      }
      .map(_._1)
      .min

  override def run =
    seeds.map(x => minloc(x,x)).min

  override def run2 =
    // minimum of starts (1st elem)
    s2.map(minloc).min
