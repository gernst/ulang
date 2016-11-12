package ulang.syntax

import arse._
import ulang.source.Grammar
import ulang.expr._
import ulang.typ._

class Parsers(thy: Thy) {
  def complete[A](line: String, p: Parser[List[String], A]): A = {
    ulang.source.Parsers.complete(line, p)
  }

  val expr_typ: Parser[List[String], (Expr, Type)] = ???

  val expr = expr_typ map (_._1)

  def formula = expr_typ map {
    case (e, `bool`) => e
    case (e, t) => sys.error("in parse: " + e + ": " + t + " + is not a formula")
  }
}