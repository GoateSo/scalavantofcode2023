package solutions
import utils.Utils.*
import scala.collection.mutable.{Stack, HashMap, HashSet, ArrayBuffer}
import scala.util.boundary, boundary.break
import os.stat
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

class Day20(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  sealed trait Module { def process: Bool }
  case class Flip(var state: Bool) extends Module:
    def process = state
  case object Broad extends Module:
    def process = false
  case class Conj(val mem: HashMap[String, Bool]) extends Module:
    def process = !mem.valuesIterator.forall(identity)
  val mapping = HashMap[String, Array[String]]()
  val map2    = HashMap[String, Module]()

  for line <- input do // input graph proc
    val Array(l, r) = line.split(" -> ")
    val next        = r.split(", ")
    if l == "broadcaster" then
      mapping += ((l, next))
      map2 += ((l, Broad))
    else
      val name = l.sub(1)
      val typ  = l(0)
      mapping += ((name, next))
      map2 += ((
        name,
        typ match
          case '&' => Conj(HashMap.empty)
          case '%' => Flip(false)
      ))
  // 2nd pass to add in all inputs to conjunctions
  for case (name, Conj(map)) <- map2 do
    for (node, nexts) <- mapping do
      if nexts.contains(name) then map += ((node, false))

  def run =
    // var lows  = 0
    // var highs = 0
    // for i <- 1 to 1000 do
    //   lows += 1 // inital button hit
    //   val queue = Queue[String]("broadcaster")
    //   while queue.nonEmpty do
    //     val cur = queue.dequeue()
    //     for
    //       elem <- map2.get(cur)
    //       sig = elem.process
    //       next <- mapping.getOrElse(cur, Array.empty[String])
    //     do
    //       if sig then highs += 1 else lows += 1
    //       for v <- map2.get(next) do
    //         v match
    //           case x @ Flip(state) if sig == false =>
    //             x.state = !x.state
    //             queue.enqueue(next)
    //           case Conj(mem) =>
    //             mem += ((cur, sig))
    //             queue.enqueue(next)
    //           case _ => ()
    // lows * highs
    1
  def run2 =
    // for i <- 1 to 5 do

    val x    = "bn"
    var prev = 0
    println(x)
    boundary:
      if samp then break()
      // while true do
      for i <- 1 to 10_000 do
        // if n % 10_000_000 == 0 then println(n)
        // n += 1 // inital button hit
        val queue = Queue[String]("broadcaster")
        while queue.nonEmpty do
          val cur = queue.dequeue()
          for
            elem <- map2.get(cur)
            sig = elem.process
          do
            for
              next <- mapping.getOrElse(cur, Array.empty[String])
              v    <- map2.get(next)
            do
              v match
                case x @ Flip(state) if sig == false =>
                  x.state = !x.state
                  queue.enqueue(next)
                case Conj(mem) =>
                  mem += ((cur, sig))
                  queue.enqueue(next)
                case _ => ()
          val ma = map2("bn").asInstanceOf[Conj].mem
          if ma("pl") then
            println(i)
            println(map2("bn"))
            println(map2("bn").process)
            // break()
          // if map2("bn").asInstanceOf[Conj].mem.values.count(_ == true) == 2 then
          //   println(i)
          //   println(map2("bn"))
        // if sig && next == x then
        //   // println(map2.get(next))
        //   println((i, cur))
        //   // println(s"diff: ${i - prev}")
        //   prev = i
    println(map2.get("bn"))
    1
