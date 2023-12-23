package solutions

import math.*
import scala.util.boundary, boundary.break
import utils.Utils.*
import scala.collection.mutable.HashMap
class Day14(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  // helper to avoid creating too many unneccessary arrays
  def writeTo[T](to: Array[Array[T]], from: Array[Array[T]]) =
    for
      i <- 0 until to.size
      j <- 0 until to(0).size
    do to(i)(j) = from(i)(j)
  val hash  = HashMap[String, Int](input.mkString -> 0)
  val empty = Array.fill(input.size, input(0).size)('.')
  def tilt(arr: Array[Array[Char]], back: Boolean, trans: Boolean) =
    // too lazy to in-place transpose, extra array so be it
    val array = if trans then arr.transpose else arr
    val (start, end, dir) =
      if back
      then (array(0).size - 1, 0, -1)
      else (0, array(0).size - 1, 1)
    // generalized by transpose/backward, but here base version is left(W) fall
    // go row by row moving 'O' left until it hits another 'O' or '#'
    for i <- 0 until array.size do
      var ind = start
      for j <- start to end by dir do
        val chr = array(i)(j)
        if chr == '#' then ind = j + dir
        if chr == 'O' then
          array(i)(j) = '.'
          array(i)(ind) = 'O'
          ind += dir
    if trans then writeTo(arr, array.transpose)

  def cycle(arr: Array[Array[Char]]): Array[Array[Char]] =
    tilt(arr, false, true)  // north
    tilt(arr, false, false) // west
    tilt(arr, true, true)   // south
    tilt(arr, true, false)  // east
    arr

  def getLoad(arr: Array[Array[Char]]): Int =
    val strains = for
      i <- 0 until arr.size
      j <- 0 until arr(0).size if arr(i)(j) == 'O'
    yield input.size - i
    strains.sum

  def run = // just do one north
    val arr = input.map(_.toArray).toArray
    tilt(arr, false, true)
    getLoad(arr)

  def run2 = // do ONE BILLION NWSE cycles
    val (ind, cyStart) = boundary: // ret index and cycle start
      var arr = input.map(_.toArray).toArray
      var i   = 0
      while true do
        i += 1
        arr = cycle(arr)
        val flat = arr.flatten.mkString
        if hash.contains(flat) then break((i, hash(flat)))
        hash += ((flat, i))
      (-1, -1)
    var index = (1000000000 - cyStart) % (ind - cyStart) + cyStart
    val last  = hash.find(_._2 == index).get._1
    getLoad(last.grouped(input.size).map(_.toArray).toArray)
