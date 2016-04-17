package ulang.syntax

import arse._
import ulang.source.Syntax

case class Thy(name: String, imports: List[Thy], defined: Sig, df: Defs) {
  lazy val imported: Sig = {
    imports.foldRight(Sig.empty)(_.exported ++ _)
  }

  lazy val sig = {
    imported ++ defined
  }

  lazy val exported = {
    Sig(sig.cons, defined.ops, sig.syntax)
  }
  
  def rename(name: String) = Thy(name, imports, defined, df)

  def +(thy: Thy) = copy(imports = imports :+ thy)
  def +(name: String, fixity: Fixity) = copy(defined = defined + (name, fixity))
  def +(con: Con) = copy(defined = defined + con)
  def +(op: Op) = copy(defined = defined + op)
  def +(con: Con, params: List[TypeParam], rhs: Type) = copy(df = df + (con, params, rhs))
  def +(con: Con, constrs: Set[Op]) = copy(df = df + (con, constrs))
  def +(ax: Expr) = copy(df = df + ax)

  /*
  def ++(that: Thy) = {
    Thy(name, this.syntax ++ that.syntax, this.sig ++ that.sig, this.df ++ that.df)
  }
  */

  override def toString = {
    val ss = imports.map("import " + _.name) ++ List(defined, df)
    ss.mkString("\n")
  }
}

object Thy {
  val empty = Thy("empty", Nil, Sig.empty, Defs.empty)
  val default = Thy("default", Nil, Sig.default, Defs.default)
}