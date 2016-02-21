package ulang.syntax

import arse._
import ulang.source._

case class Thy(name: String, imports: List[Thy], syntax: Syntax, sig: Sig, df: Defs) {
  def +(con: Con) = copy(sig = sig + con)
  def +(op: Op) = copy(sig = sig + op)
  def +(con: Con, constrs: Set[Op]) = copy(df = df + (con, constrs))
  def +(con: Con, params: List[TypeParam], rhs: Type) = copy(df = df + (con, params, rhs))
  def +(op: Op, rhs: Expr) = copy(df = df + (op, rhs))
  def +(name: String, fixity: Fixity) = copy(syntax = syntax + (name -> fixity))
  def +(thy: Thy) = copy(imports = imports :+ thy)
  def ++(that: Thy) = Thy(name, this.imports ++ that.imports, this.syntax ++ that.syntax, this.sig ++ that.sig, this.df ++ that.df)

  def flatten: Thy = {
    imports.foldLeft(this)(_ ++ _)
  }

  def ++(decls: List[Decl]): Thy = {
    decls.foldLeft(this)(_ + _)
  }

  def +(decl: Decl): Thy = decl match {
    case Import(thy) =>
      this + thy

    case FixDecl(fixity, name) =>
      this + (name, fixity)

    case TypeDecl(con) =>
      this + con

    case DataDef(schema, constrs) =>
      val con = schema.con
      val ops = constrs.map { case OpDecl(name, typ) => Op(name, typ) }.toSet
      this ++ constrs + (con, ops)

    case TypeDef(lhs, rhs) =>
      this + (lhs.con, lhs.args, rhs)

    case OpDecl(name, typ) =>
      this + Op(name, typ)

    case OpDef(name, args, rhs) =>
      // ???
      this
      // this + (name, Case(args, rhs))
  }

  override def toString = sig + "\n" + df + "\n" + syntax
}

object Thy {
  val empty = Thy("empty", Nil, Syntax.default, Sig.empty, Defs.empty)
  val default = Thy("default", Nil, Syntax.default, Sig.default, Defs.default)
}