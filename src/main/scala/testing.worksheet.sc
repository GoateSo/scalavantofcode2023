import utils.Utils.*
// "abcde".sub(1)
// "abcde".sub(-2)
// "abcde".sub(0, -1)
val input = """ligma abc 124 a:4"""
  .split("\n")
  .toList

val xs = input.map { case s"$a $b 12$c $d" =>
  (a, b, c, d)
}
