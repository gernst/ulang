package ulang

import arse._
import arse.control._
import ulang.syntax._
import ulang.source._
import scala.collection.mutable.ListBuffer

case class Context(thy: Thy, free: List[FreeVar] = Nil) {
  type pass = Parser[String, Decl]
  
  import Load._
  import Parsers._

  def flatten = Context(thy.flatten, free)
  def +(decl: Decl) = Context(thy + decl, free)

  def pass[A](p: Parser[String, A], source: List[List[String]]): List[A] = {
    val as = new ListBuffer[A]
    for (in <- source) {
      try { val (a, _) = p(in); as += a }
      catch { case _: Backtrack => }
    }
    as.toList
  }

  def pass(thy: Thy, fp: DeclParsers => Parser[String, Decl], source: List[List[String]]): Thy = {
    val decls = DeclParsers(thy)
    thy ++ pass(fp(decls), source)
  }

  def single[A](p: Parser[String, A], line: String) = {
    val q = p.$;
    q(tokenize(line))
  }

  val _decls = DeclParsers(thy)
  def _imprt(d: DeclParsers) = d.imprt map { name => Import(parse.thy(name)) }

  object parse {
    def thy(name: String): Thy = {
      val source = load(name)
      var res = Thy.default

      res = pass(res, _imprt, source)
      res = pass(res, _.fixdecl, source)
      res = pass(res, _.typedecl, source)
      res = pass(res, _.typedef, source)
      res = pass(res, _.opdecl, source)
      res = pass(res, _.opdef, source)
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