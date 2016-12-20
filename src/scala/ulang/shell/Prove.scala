package ulang.shell

import scala.annotation.tailrec

import arse._

import ulang._
import ulang.expr._
import ulang.syntax._
import ulang.calculus._

case class Prove(thy: Thy) extends Shell {
  object parse extends ulang.calculus.Parsers(thy) {
    import ulang.source.Parsers._
    def parse_rule(line: String) = complete(line, rule)
    def parse_phi(line: String) = complete(line, formula)
  }

  val prompt = "prove> "
  val commands = Map(
    ":proof" -> cmd(out("some proof")))

  def prove(goal: Proof): Proof = goal match {
    case seq: Seq =>
      out("goal: " + seq)
      val line = input("rule> ")

      line match {
        case null | ":sorry" =>
          goal
        case _ => {
          val res = parse.parse_rule(line)
          prove(res apply seq)
        } or {
          out("in prove: rule " + line + " invalid or failed")
          prove(goal)
        }
      }

    case Step(ant, suc, rule) =>
      Step(ant map prove, suc, rule)
  }

  def read(line: String) {

    val phi = parse.parse_phi(line)
    val res = prove(Seq(List(Not(phi))))

    if (res.isClosed) out("finished.")
    else out("aborted.")

    out(res)
  }
}