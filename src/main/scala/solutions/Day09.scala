package solutions
import scala.collection.mutable.ArrayBuffer

class Day09(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  val xs = input.map(_.split(" ").map(_.toLong).to(ArrayBuffer))

  val pasc = for x <- xs yield
    val layers = ArrayBuffer(x)
    var cur    = x
    while cur.size >= 2 && cur.exists(_ != 0) do
      cur = cur.sliding(2).map(l => l(1) - l(0)).to(ArrayBuffer)
      layers.append(cur)
    layers

  for
    p <- pasc
    i <- p.size - 2 to 0 by -1
  do
    val c  = p(i)
    val pr = p(i + 1)
    c.prepend(c.head - pr.head)
    c.append(c.last + pr.last)

  def run =
    pasc.map(_(0).last).sum

  def run2 =
    pasc.map(_(0).head).sum
