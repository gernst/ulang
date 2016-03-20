package ulang

import arse._
import arse.control._
import ulang.syntax._
import ulang.source._
import scala.collection.mutable.ListBuffer
import ulang.source.Grammar

case class Context(thy: Thy, free: List[FreeVar] = Nil) {
  def +(decl: Decl) = Context(thy + decl, free)

  object parse {
    import Parsers._

    val grammar = Grammar(thy)
    def decl(line: String): Decl = single(grammar.decls.parser, tokenize(line))
    def expr(line: String): Expr = single(grammar.exprs.parser, tokenize(line))
  }
}

object Context {
  val empty = Context(Thy.empty)
  val default = Context(Thy.default)
}