package ulang.syntax

import arse._

case class Thy(name: String, imports: List[Thy], syntax: Syntax, sig: Sig, df: Defs) {
  def +(con: Con) = copy(sig = sig + con)
  def +(op: Op) = copy(sig = sig + op)
  def +(con: Con, constrs: Set[Op]) = copy(df = df + (con, constrs))
  def +(con: Con, params: List[TypeParam], rhs: Type) = copy(df = df + (con, params, rhs))
  def +(ax: Expr) = copy(df = df + ax)
  def +(thy: Thy) = copy(imports = imports :+ thy)
  def ++(that: Thy) = Thy(name, this.imports ++ that.imports, this.syntax ++ that.syntax, this.sig ++ that.sig, this.df ++ that.df)

  override def toString = sig + "\n" + df + "\n" + syntax
}

object Thy {
  val empty = Thy("empty", Nil, Syntax.default, Sig.empty, Defs.empty)
  val default = Thy("default", Nil, Syntax.default, Sig.default, Defs.default)
}