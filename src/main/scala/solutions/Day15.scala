package solutions

import math.*
import scala.util.boundary, boundary.break
import utils.Utils.*
import scala.collection.mutable.ArrayBuffer
class Day15(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  val strs              = input(0).split(",")
  def hash(str: String) = str.foldLeft(0)((i, c) => (i + c) * 17 % 256)

  def run = strs.toSeq.sumBy(hash)

  val buckets = IndexedSeq.fill(256)(ArrayBuffer[(String, Int)]())

  for arr <- strs.map(_.split("[=-]")) do
    val label  = arr(0)
    val bucket = buckets(hash(label))
    val ind    = bucket.indexWhere(_._1 == label)
    if arr.size == 1 then // LAB-
      if ind >= 0 then bucket.remove(ind)
    else // LAB=VAL
      val len = arr(1).toInt
      if ind >= 0 then bucket(ind) = (label, len)
      else bucket.addOne((label, len))

  def run2 =
    buckets.zipWithIndex.sumBy: (bucket, boxInd) =>
      (boxInd + 1) * bucket.toSeq.zipWithIndex
        .sumBy((a, b) => (b + 1) * a._2)
