package ulang

import arse.control._
import java.io.File

object Load {
  import ulang.Parsers._

  val Extension = ".txt"

  var pending: Set[String] = Set.empty

  def load(name: String): Context = {
    try {
      if (pending contains name)
        error("in load: recursive dependency in " + name)
      pending += name

      val ctx = Context.default rename name
      val parse = ulang.source.Parsers.decls_ $
      val res = parse(tokenize(new File(name + Extension)))

      ctx ++ res
    } finally {
      pending -= name
    }
  }
}