package ulang.test

import ulang.source.Parsers
import ulang.transform.Convert
import ulang.syntax.Thy

object Test extends tst.Test {

  def main(args: Array[String]): Unit = {
    val thy = Parsers.thy("test")
    // val thy = Convert.decls(Thy.default, test.decls)
    println(thy)
  }
}