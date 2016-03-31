package ulang.transform

import ulang.source
import ulang.syntax
import arse.control._

class Convert(var sig: syntax.Sig, var df: syntax.Defs) {
  def decls(decls: List[source.Decl]): List[syntax.Decl] = ???
  def defs(decls: List[source.Decl]): List[syntax.Decl] = ???

  def typ(t: source.Type): syntax.Type = t match {
    case source.Id(name) =>
      if (sig.cons contains name)
        syntax.TypeApp(name)
      else
        syntax.TypeParam(name)

    case source.TypeApp(name, args) if sig.cons contains name =>
      syntax.TypeApp(name, args map typ)

    case _ =>
      error("in convert: not a proper type " + t)
  }

  def expr(e: source.Expr): syntax.Expr = ???

  def con(t: source.Type): syntax.Con = t match {
    case source.Id(name) =>
      syntax.Con(name, 0)
    case source.TypeApp(name, args) =>
      syntax.Con(name, args.length)
    case _ =>
      error("in convert: not a type constructor " + t)
  }

  def schema(t: source.Type): syntax.Schema = typ(t) match {
    case syntax.TypeApp(name, args) if args forall (_.isInstanceOf[syntax.TypeParam]) =>
      syntax.Schema(name, args.asInstanceOf[List[syntax.TypeParam]])
    case _ =>
      error("in convert: not a type schema " + t)
  }

  def type_sig(decl: source.Decl): Unit = decl match {
    case source.Import(mod) =>
      mod.decls foreach type_sig
    case source.TypeDef(lhs, rhs) =>
      sig += con(lhs)
    case source.DataDef(lhs, constrs) =>
      sig += con(lhs)
    case _ =>
  }

  def op_sig(decl: source.Decl): Unit = decl match {
    case source.Import(mod) =>
      mod.decls foreach op_sig
    case source.OpDecl(name, rhs) =>
      sig += syntax.Op(name, typ(rhs))
    case source.DataDef(lhs, constrs) =>
      constrs foreach op_sig
    case _ =>
  }

  def type_def(decl: source.Decl): Unit = decl match {
    case source.Import(mod) =>
      mod.decls foreach type_def
    case source.TypeDef(lhs, rhs) =>
      val s = schema(lhs)
      df += (s.con, s.args, typ(rhs))
    case _ =>
  }

  def op_def(decl: source.Decl): Unit = decl match {
    case source.Import(mod) =>
      mod.decls foreach type_def
    case source.OpDef(axiom) =>
      ???
    case _ =>
  }
}