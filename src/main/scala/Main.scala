import solutions.*
import scala.io.AnsiColor.*
import os.*
import utils.Utils
import fastparse.internal.Util

@main def main: Unit =
  val x               = println(pwd)
  val realInputs      = os.read.lines(pwd / "input.txt")
  val sampleInputs    = os.read.lines(pwd / "sample.txt")
  def emph(s: String) = s"$BOLD$UNDERLINED$BLUE$s$RESET"
  def ans(s: String)  = s"$RED$s$RESET"
  os.write.over(pwd / "POutput.txt", "")

  println(emph("[sample]"))
  Utils.write("[part 1]:")
  val s2 = Day02(sampleInputs, true)
  println(s"${GREEN}part 1: $RESET")
  println(ans(s2.run.toString))
  Utils.write("+".repeat(120))
  Utils.write("[part 2]:")
  println(s"${GREEN}part 2: $RESET")
  println(ans(s2.run2.toString))

  Utils.write("~".repeat(120))

  println(emph("[real]"))
  Utils.write("[part 1]:")
  val s = Day02(realInputs, false)
  println(s"${GREEN}part 1: $RESET")
  println(ans(s.run.toString))
  Utils.write("+".repeat(120))
  Utils.write("[part 2]:")
  println(s"${GREEN}part 2: $RESET")
  println(ans(s.run2.toString))
