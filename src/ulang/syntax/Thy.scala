package ulang.syntax

import arse._
import ulang.source.Decl

case class Thy(name: String, syntax: Syntax, sig: Sig, df: Defs) {
  def ++(that: Thy) = {
    Thy(name, this.syntax ++ that.syntax, this.sig ++ that.sig, this.df ++ that.df)
  }

  def +(decl: Decl): Thy = {
    Thy(name, syntax + decl, sig + decl, df + decl)
  }

  def ++(decls: List[Decl]): Thy = {
    decls.foldLeft(this)(_ + _)
  }

  override def toString = sig + "\n" + df + "\n" + syntax
}

object Thy {
  def apply(decls: List[Decl]): Thy = empty ++ decls

  val empty = Thy("empty", Syntax.default, Sig.empty, Defs.empty)
  val default = Thy("default", Syntax.default, Sig.default, Defs.default)
}