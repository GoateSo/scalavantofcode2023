import solutions.*
import scala.io.AnsiColor.*
import os.*
import utils.Utils
import fastparse.internal.Util
import fansi.Color.{Green, Red, Blue}
import fansi.{Underlined, Bold}

def emph(s: String) =
  Blue(s).overlay(Underlined.On).overlay(Bold.On)

def output(soln: Solution) =
  println(Green("part 1:"))
  println(Red(soln.run.toString))
  Utils.write("+".repeat(120))
  Utils.write("[part 2]:")
  println(Green(s"part 2:"))
  println(Red(soln.run2.toString))

@main def main: Unit =
  os.write.over(pwd / "POutput.txt", "")
  println(emph("[sample]"))
  output(Day06(os.read.lines(pwd / "sample.txt"), true))
  Utils.write("~".repeat(120))
  println(emph("[real]"))
  output(Day06(os.read.lines(pwd / "input.txt"), false))
