package ulang

import arse._
import arse.control._
import ulang.syntax._
import ulang.source._
import scala.collection.mutable.ListBuffer

case class Context(thy: Thy, free: List[FreeVar] = Nil) {
  type Phase = Parser[String, Decl]
  
  import Load._
  import Parsers._

  def +(decl: Decl) = Context(thy + decl, free)

  def phase[A](p: Parser[String, A], source: List[List[String]]): List[A] = {
    val as = new ListBuffer[A]
    for (in <- source) {
      try { val (a, _) = p(in); as += a }
      catch { case _: Backtrack => }
    }
    as.toList
  }

  def phase(thy: Thy, fp: DeclParsers => Parser[String, Decl], source: List[List[String]]): Thy = {
    val decls = DeclParsers(thy)
    thy ++ phase(fp(decls), source)
  }

  def single[A](p: Parser[String, A], line: String) = {
    val q = p $;
    q(tokenize(line))
  }

  val _decls = DeclParsers(thy)
  def _imprt(d: DeclParsers) = d.imprt map { name => Import(parse.thy(name)) }

  object parse {
    def thy(name: String): Thy = {
      val source = load(name)
      var res = Thy.default

      res = phase(res, _imprt, source)
      res = phase(res, _.fixdecl, source)
      res = phase(res, _.typedecl, source)
      res = phase(res, _.typedef, source)
      res = phase(res, _.opdecl, source)
      res = phase(res, _.opdef, source)
      res
    }

    def expr(line: String): Expr = single(_decls.expr.parser, line)
    def decl(line: String): Decl = single(_imprt(_decls) | _decls.parser, line)
  }
}

object Context {
  val empty = Context(Thy.empty)
  val default = Context(Thy.default)
}