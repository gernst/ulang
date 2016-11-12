package ulang.test

import arse.Parser
import ulang.source.Module

object Test extends tst.Test {
  def parse(name: String) {
    ulang.source.Load.load(name) {
      reader =>
        val tokens = ulang.source.Parsers.tokenize(reader)
        val parser: Parser[List[String], Module] = ??? // ulang.source.Grammar.module
        val (res, out) = parser(tokens)
        println(res)
        out expect Nil
    }
  }

  def main(args: Array[String]): Unit = {
    // parse("src/ulang/hol")
    // parse("src/ulang/nat")
    import ulang.typ._

    val a = App("L", List(Free("a"), Free("c")))
    val b = App("L", List(Free("b"), App("K", List(Free("a")))))
    val Some(s) = a unify b
    println(a subst s)
    println(b subst s)
  }
}