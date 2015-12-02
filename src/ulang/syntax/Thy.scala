package ulang.syntax

case class Thy(sig: Sig, df: Def) {
  def +(con: Con) = copy(sig = sig + con)
  def +(op: Op) = copy(sig = sig + op)
  def +(con: Con, constrs: Set[Op]) = copy(df = df + (con, constrs))
  def +(ax: Expr) = copy(df = df + ax)
  def ++(that: Thy) = Thy(this.sig ++ that.sig, this.df ++ that.df)

  override def toString = sig + "\n" + df
}

object Thy {
  val empty = Thy(Sig.empty, Def.empty)
  val default = Thy(Sig.default, Def.default)
}