package ulang.newsyntax

import arse._
import arse.control._
import ulang.syntax._
import scala.collection.mutable.ListBuffer
import java.io.File

case class Context(syntax: Syntax, sig: Sig, free: List[FreeVar] = Nil) {
  type Phase = Parser[String, Decl]

  def phase[A](p: Parser[String, A], source: List[List[String]]): List[A] = {
    val as = new ListBuffer[A]
    for (in <- source) {
      try { val (a, _) = p(in); as += a }
      catch { case _: Backtrack => }
    }
    as.toList
  }

  def phase(thy: Thy, fp: DeclParsers => Parser[String, Decl], source: List[List[String]]): Thy = {
    val decls = DeclParsers(thy.syntax, thy.sig)
    thy ++ phase(fp(decls), source)
  }

  object parse {
    import Load._
    import Parsers._

    def thy(name: String): Thy = {
      val source = load(name)
      var res = Thy.default

      res = phase(res, _.fixdecl, source)
      res = phase(res, _.typedecl, source)
      res = phase(res, _.typedef, source)
      res = phase(res, _.opdecl, source)
      res = phase(res, _.opdef, source)
      res
    }
  }
}

object Context {
  val empty = Context(Syntax.empty, Sig.empty)
  val default = Context(Syntax.default, Sig.default)
}