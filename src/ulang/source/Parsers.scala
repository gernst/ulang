package ulang.source

import arse._
import arse.control._
import arse.Combinators.parse
import ulang.syntax._
import scala.collection.mutable.ListBuffer
import java.io.StringReader
import java.io.FileReader
import java.io.Reader
import java.io.File
import scala.language.implicitConversions

class Parsers extends Combinators with Primitives {
  type T = String

  def expect(s: String) = lit(s) ! "expected '" + s + "'"
  def parens[A](p: Parser[T, A]) = lit("(") ~> p <~ expect(")")

  val nonkw = string filterNot Parsers.keywords
}

object Parsers {
  implicit def toFileReader(file: File) = new FileReader(file)
  implicit def toStringReader(text: String) = new StringReader(text)

  val keywords = Set(
    "import", "data", "type",
    "prefix", "infix", "postfix", "binder",
    "(", ")", ";", "λ", ".")

  type Region = List[String]

  def tokenize(reader: Reader): List[String] = {
    val scan = new Scanner(reader)
    val buffer = new ListBuffer[String]
    var tok = scan.next
    while (tok != null) {
      buffer += tok
      tok = scan.next
    }
    buffer.toList
  }

  def regionize(reader: Reader): List[Region] = {
    val scan = new Scanner(reader)
    val buffer = new ListBuffer[Region]
    var run = new ListBuffer[String]
    var tok = scan.next

    while (tok != null) {
      if (tok == ";") {
        if (!run.isEmpty) {
          buffer += run.toList
          run.clear
        }
      } else {
        run += tok
      }
      tok = scan.next
    }
    buffer.toList
  }

  def pass[A](p: Parser[String, A], source: List[List[String]]): List[A] = {
    val as = new ListBuffer[A]
    for (in <- source) {
      try { val (a, _) = p(in); as += a }
      catch { case _: Backtrack => }
    }
    as.toList
  }

  def pass[A <: Decl](syntax: Syntax, sig: Sig, fp: DeclParsers => Parser[String, A], source: List[List[String]]): List[A] = {
    val decls = DeclParsers(syntax, sig)
    Parsers.pass(fp(decls), source)
  }

  def module(name: String): Module = Load.load(name) {
    source =>

      var syntax = Syntax.default
      var sig = Sig.default
      var defs = Defs.default

      val imports = pass(syntax, sig, _.imprt, source)
      syntax ++= imports
      sig ++= imports

      val fixdecls = pass(syntax, sig, _.fixdecl, source)
      syntax ++= fixdecls

      val typedecls = pass(syntax, sig, _.typedecl, source)
      sig ++= typedecls

      val opdecls = pass(syntax, sig, _.opdecl, source)
      sig ++= opdecls

      val datadefs = pass(syntax, sig, _.datadef, source)
      val typedefs = pass(syntax, sig, _.typedef, source)
      val opdefs = pass(syntax, sig, _.opdef, source)

      val decls = imports ::: fixdecls ::: typedecls ::: opdecls ::: datadefs ::: typedefs ::: opdefs
      Module(name, decls)
  }
}

trait SyntaxParsers {
  this: Parsers =>
  type Op

  def syntax: Syntax
  def op(name: String): Op
  def keywords: Set[String]

  def mixfix_op[A](m: Map[String, A], name: String) = {
    if (keywords contains name) fail
    else if (m contains name) (op(name), m(name))
    else fail
  }

  def mixfix_op(s: Set[String], name: String) = {
    if (keywords contains name) fail
    else if (s contains name) op(name)
    else fail
  }

  lazy val name = string filterNot keywords // keywords is initialized in subclass
  val prefix_op = next { mixfix_op(syntax.prefix_ops, _) }
  val postfix_op = next { mixfix_op(syntax.postfix_ops, _) }
  val infix_op = next { mixfix_op(syntax.infix_ops, _) }
  val bindfix_op = next { mixfix_op(syntax.bindfix_ops, _) }
}
