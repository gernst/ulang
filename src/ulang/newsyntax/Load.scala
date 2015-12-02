package ulang.newsyntax

import arse.control._
import java.io.File
import arse.Parser
import scala.collection.mutable.ListBuffer

object Load {
  import ulang.Parsers._

  val Extension = ".txt"

  var pending: Set[String] = Set.empty
  
  def load(name: String): List[List[String]] = {
    try {
      if (pending contains name)
        error("in load: recursive dependency in " + name)
      pending += name

      regionize(new File(name))
    } finally {
      pending -= name
    }
  }
}