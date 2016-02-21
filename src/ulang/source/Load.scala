package ulang.source

import arse.control._
import java.io.File

object Load {
  val Extension = ".txt"

  var pending: Set[String] = Set.empty

  def load(name: String): List[List[String]] = {
    import Parsers._

    try {
      if (pending contains name)
        error("in load: recursive dependency in " + name)
      pending += name

      regionize(new File(name + Extension))
    } finally {
      pending -= name
    }
  }
}