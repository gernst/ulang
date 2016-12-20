package ulang.syntax

import arse._

case class Thy(name: String, imports: List[Thy], defined: Sig, df: Defs) {
  lazy val imported: Sig = {
    imports.foldRight(Sig.empty)(_.exported ++ _)
  }

  lazy val sig = {
    imported ++ defined
  }

  lazy val exported = {
    Sig(sig.cons, defined.ops)
  }
  
  // def rename(name: String) = Thy(name, imports, defined, df)

  override def toString = {
    val ss = imports.map("import " + _.name) ++ List(defined, df)
    ss.mkString("\n")
  }
}

object Thy {
  val empty = Thy("empty", Nil, Sig.empty, Defs.empty)
  val default = Thy("default", Nil, Sig.default, Defs.default)
}