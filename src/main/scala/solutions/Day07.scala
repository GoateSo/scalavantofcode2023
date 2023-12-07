package solutions

class Day07(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  val xs    = input map { case s"$a $b" => (a, b.toInt) }
  val hands = xs.map(_._1)
  val bids  = xs.map(_._2)
  def handType(hand: String) =
    val occ = hand.groupMapReduce(x => x)(_ => 1)(_ + _).values
    occ.size match
      case 1                             => 7 // XXXXX
      case 2 if occ.find(_ == 4) != None => 6 // XXXXY
      case 2 if occ.find(_ == 3) != None => 5 // XXXYY
      case 3 if occ.find(_ == 3) != None => 4 // XXXYZ
      case 3 if occ.count(_ == 2) == 2   => 3 // XXYYZ
      case 4                             => 2 // XYZWX
      case _                             => 1 // XYZWT
  def handType2(hand: String) =
    val occ  = hand.replaceAll("J", "").groupBy(x => x)
    val best = occ.maxByOption(_._2.size).map(_._1).getOrElse('A')
    handType(hand.replace('J', best))
  def value1(x: Char) = "23456789TJQKA".indexOf(x)
  def value2(x: Char) = "J23456789TQKA".indexOf(x)
  def getVal(hs: Seq[(String, Int)]) =
    hs.map(_._2).zipWithIndex.map((a, b) => a * (b + 1)).sum
  val pair = hands.zip(bids)
  def run =
    getVal(
      pair
        .sortWith { case ((h1, _), (h2, _)) =>
          if handType(h1) == handType(h2) then
            val (x, y) = h1.zip(h2).find(_ != _).get
            value1(x) < value1(y)
          else handType(h1) < handType(h2)
        }
    )
  def run2 =
    getVal(
      pair
        .sortWith { case ((h1, _), (h2, _)) =>
          if handType2(h1) == handType2(h2) then
            val (x, y) = h1.zip(h2).find(_ != _).get
            value2(x) < value2(y)
          else handType2(h1) < handType2(h2)
        }
    )
