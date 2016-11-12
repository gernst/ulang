package ulang.source

import java.io.File
import java.io.Reader
import java.io.StringReader
import java.io.FileReader

import scala.language.implicitConversions
import scala.collection.mutable.ListBuffer

import arse._

object Parsers {
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
  
  def complete[A](line: String, p: Parser[List[String], A]): A = {
    val source = tokenize(line)
    val (a, rest) = p(source)
    if(!rest.isEmpty) sys.error("leftover input: " + rest)
    a
  }
}
