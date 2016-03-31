package ulang.source

import arse._
import arse.control._
import arse.Combinators.parse
import java.io.StringReader
import java.io.FileReader
import java.io.Reader
import java.io.File
import scala.collection.mutable.ListBuffer
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
    "(", ")", ";", "λ", ".", ":")

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

    def flush() = if (!run.isEmpty) {
      buffer += run.toList
      run.clear
    }

    while (tok != null) {
      if (tok == ";") flush()
      else run += tok
      tok = scan.next
    }
    flush() // flush non ";" terminated tokens
    buffer.toList
  }

  def mod(name: String): Module = Load.load(name) {
    reader =>
      val grammar = new Grammar(Syntax.default)
      val parse = grammar.decls.parser.$
      val source = regionize(reader)
      val decls = source map parse
      val syntax = grammar.syntax
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
