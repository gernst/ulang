package ulang.source

import java.io.File
import java.io.Reader

object Load {
  val Extension = ".u"

  var pending: Set[String] = Set.empty

  def load[A](name: String)(f: Reader => A): A = {
    import Parsers._

    try {
      if (pending contains name)
        sys.error("in load: recursive dependency in " + name)
      pending += name

      f(new File(name + Extension))
    } finally {
      pending -= name
    }
  }
}