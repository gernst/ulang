package ulang

import arse._
import arse.control._
import java.io.StringReader
import java.io.FileReader
import arse.Primitives
import java.io.Reader
import java.io.File
import scala.collection.mutable.ListBuffer

class Parsers extends Combinators with Primitives {
  type T = String
  import Parsers.Region

  def expect(s: String) = lit(s) ! "expected '" + s + "'"
  def parens[A](p: Parser[T, A]) = lit("(") ~> p <~ expect(")")
}

object Parsers {
  implicit def toFileReader(file: File) = new FileReader(file)
  implicit def toStringReader(text: String) = new StringReader(text)

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
}