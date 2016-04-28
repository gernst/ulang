package ulang.syntax

import arse._
import arse._
import ulang.transform.Convert
import ulang.source.Grammar

class Parsers(thy: Thy) extends ulang.source.Parsers {
  def complete[A](line: String, p: Parser[Token, A]): A = {
    ulang.source.Parsers.complete(line, thy.sig.syntax, p)
  }

  val expr_typ = Grammar.exprs.parser map {
    Convert.expr_infer_top(thy.sig, thy.df, _, Nil)
  }

  val expr = expr_typ map (_._1)

  def formula = expr_typ map {
    case (e, Type.bool) => e
    case (e, t) => error("in parse: " + e + ": " + t + " + is not a formula")
  }
}