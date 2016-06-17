package ulang.source

import java.io._
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

import arse._

import ulang.syntax._
import ulang.transform.Convert

object Parsers {
  val keywords = Set(
    "import", "data", "type",
    "prefix", "infix", "postfix", "binder",
    "(", ")", ";", "λ", ".", ":", "|", "=")

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
      if (tok.toString == ";") flush()
      else run += tok
      tok = scan.next
    }
    flush() // flush non ";" terminated tokens
    buffer.toList
  }

  def complete[A](line: String, p: Parser[String, A]): A = {
    val parse = p.$
    val source = tokenize(line)
    parse(source)
  }

  def thy(name: String): Thy = Load.load(name) {
    reader =>
      val parse = Grammar.decl.$
      val source = regionize(reader)
      val decls = source map parse
      val thy: Thy = ??? // Convert.decls(Thy.default, decls)
      thy rename name
  }

  def decl(line: String, thy: Thy): Thy = {
    val res = complete(line, Grammar.decl)
    ??? //Convert.decl(thy, res)
  }

  def expr(line: String, thy: Thy, free: List[FreeVar]): (Expr, Type) = {
    val res = complete(line, Grammar.expr)
    ??? //Convert.expr_infer_top(thy.sig, thy.df, res, free)
  }

  def typ(line: String, thy: Thy): Type = {
    val res = complete(line, Grammar.typ)
    ??? //Convert.typ(thy.sig, res)
  }

  def formula(line: String, thy: Thy, free: List[FreeVar]) = {
    val (e, t) = expr(line, thy, free)
    if (!t.isBool)
      error("in parse: " + e + ": " + t + " is not a formula")
    e
  }
}

