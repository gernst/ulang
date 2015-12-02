package ulang.shell

import arse.control._
import ulang._
import ulang.syntax._
import ulang.calculus._
import scala.annotation.tailrec

case class Prove(context: Context) extends Shell {
  import ulang.source.Parsers._
  import ulang.calculus.Parsers._
  
  val parse_rule = rule(context) $
  val parse_phi = formula(context) $

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
          val res = parse_rule(tokenize(line))
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
    import ulang.source.Parsers._

    val phi = parse_phi(tokenize(line))
    val res = prove(Seq(Nil, phi))

    if (res.isClosed) out("finished.")
    else ("aborted.")

    out(res)
  }
}