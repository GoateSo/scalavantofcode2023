package solutions
import scala.collection.mutable.ListBuffer
import utils.Utils.*
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

  // part 1, find minimum location after applying all mappings
  def minloc(seed: Long) =
    mappings
      .foldLeft(List(seed)) { case (xs, map) =>
        // for each range, find all mappings that overlap with it, and add those to the list
        xs.flatMap { cur =>
          val ms =
            for Array(d, s, len) <- map if cur >= s && cur < s + len
            yield d + (cur - s)
          // in case no mapping
          if ms.isEmpty then List(cur) else ms
        }
      }
      .min
  // part 2, find minimum location treating inputs as ranges
  def minloc2(range: (Long, Long)) =
    // each step of the process
    mappings
      .foldLeft(List(range)) { case (xs, map) =>
        // for each range, find all mappings that overlap with it, and add those to the list
        xs.flatMap { (a, r) =>
          var l = a // mutable copy
          val nrangs = map.flatMap { case Array(d, s, len) =>
            val lb = s // left + right bounds
            val rb = s + len - 1
            // find overlap, update l and r (maps should be sorted by s = l1)
            val ret =
              if l < lb && r <= rb && r >= lb then
                // case1, on left side of range
                // side on left, never will be mapped
                List((d, r - lb + d), (l, lb - 1))
              else if lb <= l && rb < r && l <= rb then
                // case2, on right side of range, left overlap added as is
                // unmapped on right can possibly be mapped in future
                List((l - lb + d, rb - lb + d))
              else if lb <= l && r <= rb then
                // case3, totally contained
                List((l - lb + d, r - lb + d))
              else if l < lb && rb < r then
                // case4, both sides have unmapped
                // part on left never will be mapped
                List((d, rb - lb + d), (l, lb - 1))
              else Nil
            l = math.max(l, math.min(r + 1, rb + 1)) // update left value
            ret
          }
          // in case no mapping
          if nrangs.isEmpty then List((a, r)) else nrangs
        }
      }
      .map(_._1)
      .min

  override def run =
    seeds.map(minloc).min

  override def run2 =
    // minimum of starts (1st elem)
    s2.map(minloc2).min
