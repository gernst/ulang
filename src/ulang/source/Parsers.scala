package ulang.source

import arse._
import arse._
import arse.Combinators.parse
import java.io.StringReader
import java.io.FileReader
import java.io.Reader
import java.io.File
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import ulang.syntax
import ulang.transform.Convert

class Parsers extends Combinators with Primitives {
  type T = Token

  def expect(s: String) = lit(s) ! "expected '" + s + "'"
  def parens[A](p: Parser[T, A]) = lit("(") ~> p <~ expect(")")

  val nonkw = string filterNot Parsers.keywords

  def partial[A](syntax: Syntax, p: Parser[T, A]): Parser[T, A] = lift {
    in =>
      Grammar.context.withValue(syntax)(p(in))
  }
}

object Parsers {
  val keywords = Set(
    "import", "data", "type",
    "prefix", "infix", "postfix", "binder",
    "(", ")", ";", "λ", ".", ":")

  type Region = List[Token]

  implicit def toFileReader(file: File) = new FileReader(file)
  implicit def toStringReader(string: String) = new StringReader(string)

  def tokenize(reader: Reader): List[Token] = {
    val scan = new Scanner(reader)
    val buffer = new ListBuffer[Token]
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
    var run = new ListBuffer[Token]
    var tok = scan.next

    def flush() = if (!run.isEmpty) {
      buffer += run.toList
      run.clear
    }

    while (tok != null) {
      if (tok.toString == ";") flush()
      else run += tok
      tok = scan.next
    }
    flush() // flush non ";" terminated tokens
    buffer.toList
  }

  def complete[A](line: String, syntax: Syntax, p: Parser[Token, A]): A = {
    Grammar.context.withValue(syntax) {
      val parse = p.$
      val source = tokenize(line)
      parse(source)
    }
  }

  def thy(name: String): syntax.Thy = Load.load(name) {
    reader =>
      Grammar.context.withValue(Syntax.default) {
        val parse = Grammar.decls.parser.$
        val source = regionize(reader)
        val decls = source map parse
        val thy = Convert.decls(syntax.Thy.default, decls)
        thy rename name
      }
  }

  def decl(line: String, thy: syntax.Thy) = {
    val res = complete(line, thy.sig.syntax, Grammar.decls.parser)
    Convert.decl(thy, res)
  }

  def expr(line: String, thy: syntax.Thy, free: List[syntax.FreeVar]) = {
    val res = complete(line, thy.sig.syntax, Grammar.exprs.parser)
    Convert.expr_infer_top(thy.sig, thy.df, res, free)
  }

  def typ(line: String, thy: syntax.Thy) = {
    val res = complete(line, thy.sig.syntax, Grammar.types.parser)
    Convert.typ(thy.sig, res)
  }

  def formula(line: String, thy: syntax.Thy, free: List[syntax.FreeVar]) = {
    val (e, t) = expr(line, thy, free)
    if (!t.isBool)
      error("in parse: " + e + ": " + t + " is not a formula")
    e
  }
}

trait SyntaxParsers {
  this: Parsers =>
  type Op

  def syntax: Syntax
  def op(name: String): Op
  def keywords: Set[String]

  def mixfix_op[A](m: Map[String, A], tok: Token) = {
    val name = tok.text
    if (keywords contains name) fail
    else if (m contains name) (op(name), m(name))
    else fail
  }

  def mixfix_op(s: Set[String], tok: Token) = {
    val name = tok.text
    if (keywords contains name) fail
    else if (s contains name) op(name)
    else fail
  }

  lazy val name = string filterNot keywords // keywords is initialized in subclass
  lazy val nonmixfix = name filterNot { syntax.contains(_) } // defer evaluation of syntax

  lazy val id = parse(Id)(nonmixfix)
  lazy val ids = id.+

  val prefix_op = next { mixfix_op(syntax.prefix_ops, _) }
  val postfix_op = next { mixfix_op(syntax.postfix_ops, _) }
  val infix_op = next { mixfix_op(syntax.infix_ops, _) }
  val bindfix_op = next { mixfix_op(syntax.bindfix_ops, _) }
}
