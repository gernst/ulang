package ulang.syntax

import arse._
import ulang.source.Syntax

case class Thy(name: String, syntax: Syntax, sig: Sig, df: Defs) {
  def ++(that: Thy) = {
    Thy(name, this.syntax ++ that.syntax, this.sig ++ that.sig, this.df ++ that.df)
  }

  def +(decl: Decl): Thy = {
    Thy(name, syntax /* decl */ , sig + decl, df + decl)
  }

  def ++(decls: List[Decl]): Thy = {
    decls.foldLeft(this)(_ + _)
  }

  override def toString = sig + "\n" + df + "\n" + syntax
}

object Thy {
  def apply(name: String, decls: List[Decl] = Nil): Thy = {
    empty(name) ++ decls
  }

  def empty(name: String) = Thy(name, Syntax.empty, Sig.empty, Defs.empty)
  def default(name: String) = Thy(name, Syntax.default, Sig.default, Defs.default)
}