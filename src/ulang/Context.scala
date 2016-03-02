package ulang

import arse._
import arse.control._
import ulang.syntax._
import ulang.source._
import scala.collection.mutable.ListBuffer

case class Context(thy: Thy, free: List[FreeVar] = Nil) {
  def +(decl: Decl) = ??? // Context(thy + decl, free)

  /*object parse {
    import Load._
    import Parsers._

    def pass[A <: Decl](syntax: Syntax, sig: Sig, fp: DeclParsers => Parser[String, A], source: List[List[String]]): List[A] = {
      val decls = DeclParsers(syntax, sig)
      Parsers.pass(fp(decls), source)
    }

    def single[A](p: Parser[String, A], line: String) = {
      val q = p.$;
      q(tokenize(line))
    }

    val _decls = DeclParsers(thy.syntax, thy.sig)
    def _imprt(d: DeclParsers) = d.imprt map { name => Import(parse.module(name)) }

    // TODO: make constructors for Syntax, Sig, ... accept list of (any) decls
    //       then store decls in modules and just create stuff
    def module(name: String): Module = {
      val source = load(name)
      var syntax = Syntax.default
      var sig = Sig.default
      var defs = Defs.default

      val imports = pass(syntax, sig, _.fixdecl, source)

      val fixdecls = pass(syntax, sig, _.fixdecl, source)
      syntax ++= fixdecls

      val typedecls = pass(syntax, sig, _.typedecl, source)
      sig ++= typedecls

      val opdecls = pass(syntax, sig, _.opdecl, source)
      sig ++= opdecls

      val datadefs = pass(syntax, sig, _.datadef, source)
      defs ++= datadefs

      val typedefs = pass(syntax, sig, _.typedef, source)
      defs ++= typedefs

      val opdefs = pass(syntax, sig, _.opdef, source)
      defs ++= opdefs

      val decls = imports ::: fixdecls ::: typedecls ::: opdecls ::: datadefs ::: typedefs ::: opdefs
      Module(name, decls)
    }

    def expr(line: String): Expr = single(_decls.expr.parser, line)
    def decl(line: String): Decl = single(_decls.parser, line)
  }*/
}

object Context {
  val empty = Context(Thy.empty)
  val default = Context(Thy.default)
}