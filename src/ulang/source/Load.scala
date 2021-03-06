package ulang.source

import arse._
import java.io.File
import java.io.Reader

object Load {
  val Extension = ".txt"

  var pending: Set[String] = Set.empty

  def load[A](name: String)(f: Reader => A): A = {
    import Parsers._

    try {
      if (pending contains name)
        error("in load: recursive dependency in " + name)
      pending += name

      f(new File(name + Extension))
    } finally {
      pending -= name
    }
  }
}