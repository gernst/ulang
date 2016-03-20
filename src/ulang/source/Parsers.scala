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
  val keywords = Set(
    "import", "data", "type",
    "prefix", "infix", "postfix", "binder",
    "(", ")", ";", "λ", ".")

  type Region = List[String]

  implicit def toFileReader(file: File) = new FileReader(file)
  implicit def toStringReader(string: String) = new StringReader(string)

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

  def single[A](p: Parser[String, A], source: List[String]) = {
    val q = p.$;
    q(source)
  }

  def pass[A](thy: Thy, gp: Grammar => Parser[String, A], source: List[List[String]]): List[A] = {
    val p = gp(new Grammar(thy))
    val as = new ListBuffer[A]
    for (in <- source) {
      try { val (a, _) = p(in); as += a }
      catch { case _: Backtrack => }
    }
    as.toList
  }

  def theory(name: String): Thy = Load.load(name) {
    reader =>
      val source = regionize(reader)
      var thy = Thy.default
      thy ++= pass(thy, _.decls.imprt, source)
      thy ++= pass(thy, _.decls.fixdecl, source)
      thy ++= pass(thy, _.decls.typedecl, source)
      thy ++= pass(thy, _.decls.opdecl, source)
      thy ++= pass(thy, _.decls.datadef, source)
      thy ++= pass(thy, _.decls.typedef, source)
      thy ++= pass(thy, _.decls.opdef, source)
      thy
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
