package solutions
import utils.Utils.*
import scala.collection.mutable.HashMap

class Day19(input: Seq[String], samp: Boolean) extends Solution(input, samp):
  // useful classes and types
  type Part = Map[Char, Int]
  case class Rule(pred: State, nState: String)
  sealed trait State extends Function1[Part, Bool]
  case class Case(field: Char, lt: Bool, value: Int) extends State:
    def apply(p: Part) = if lt then p(field) < value else p(field) > value
  case object Base extends State { def apply(p: Part) = true }

  case class Possible(constraints: Map[Char, Ranges]):
    def addConstraint(field: Char, lt: Bool, value: Int) =
      // mask to remove condition is lt
      val mask = if lt then (value, 4000) else (1, value)
      this.copy(constraints + ((field, constraints(field) - mask)))
    def getCombos: Long = constraints.values.foldLeft(1L)(_ * _.count)
  val Seq(sec1, sec2) = input.splitBy("")
  // process part 1
  val line        = "([a-z]+)\\{(.+)\\}".r
  val basic       = "([AR]|[a-z]+)".r
  val conditional = "([xmas])([><])(\\d+):([AR]|[a-z]+)".r
  val rmap        = HashMap[String, List[Rule]]()
  for case line(name, rules) <- sec1 do
    val conditions =
      for cond <- rules.split(",")
      yield cond match
        case basic(next) => Rule(Base, next)
        case conditional(field, sign, num, next) =>
          Rule(Case(field(0), sign == "<", num.toInt), next)
    rmap(name) = conditions.toList

  def solveCnt(state: String, inputs: Possible): Long =
    state match
      case "A" => inputs.getCombos
      case "R" => 0L
      case _ =>
        var ret = 0L
        var cur = inputs
        for Rule(cond, next) <- rmap(state) do
          cond match
            case Case(field, lt, value) =>
              val use = cur.addConstraint(field, lt, value)
              ret += solveCnt(next, use)
              // get remainder
              val dif = if lt then -1 else 1
              cur = cur.addConstraint(field, !lt, value + dif)
            case Base => ret += solveCnt(next, cur)
        ret

  def run =
    val xs = for case s"{x=$x,m=$m,a=$a,s=$s}" <- sec2 yield
      val xi = x.toInt
      val mi = m.toInt
      val ai = a.toInt
      val si = s.toInt
      val v = Possible(
        Map(
          'x' -> Ranges((xi, xi)),
          'm' -> Ranges((mi, mi)),
          'a' -> Ranges((ai, ai)),
          's' -> Ranges((si, si))
        )
      )
      solveCnt("in", v) * (xi + mi + ai + si)
    xs.sum

  def run2 =
    solveCnt(
      "in",
      Possible(
        Map(
          'x' -> Ranges((1, 4000)),
          'm' -> Ranges((1, 4000)),
          'a' -> Ranges((1, 4000)),
          's' -> Ranges((1, 4000))
        )
      )
    )
