package ulang.source

import arse.control._
import java.io.File

object Load {
  val Extension = ".txt"

  var cache: Map[String, Any] = Map.empty
  var pending: Set[String] = Set.empty

  def clear {
    cache = Map.empty
  }

  def load[A](name: String)(f: List[List[String]] => A): A = {
    import Parsers._

    try {
      if (pending contains name)
        error("in load: recursive dependency in " + name)
      pending += name

      if (cache contains name) {
        cache(name).asInstanceOf[A]
      } else {
        val a = f(regionize(new File(name + Extension)))
        cache += name -> a
        a
      }

    } finally {
      pending -= name
    }
  }
}