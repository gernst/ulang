package ulang

import arse._
import ulang.syntax._
import ulang.source.Parsers
import ulang.source.Grammar
import ulang.expr.FreeVar

case class Context(thy: Thy, free: List[FreeVar] = Nil) {
  object parse {
    def decl(line: String) = ??? // Context(Parsers.decl(line, thy), free)
    def expr(line: String) = ??? // Parsers.expr(line, thy, free)
    def typ(line: String) = ??? // Parsers.typ(line, thy)
  }
}

object Context {
  val empty = Context(Thy.empty)
  val default = Context(Thy.default)
}