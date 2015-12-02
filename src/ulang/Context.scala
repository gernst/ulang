package ulang

import arse.Fixity
import ulang.syntax._
import ulang.source._
import ulang.transform._
import scala.util.DynamicVariable

case class Context(name: String, sig: Sig, df: Def, syntax: Syntax) {
  import Reorder._
  import Convert._
  import Infer._
  import Load._
  import semantics.Model

  def rename(name: String) = copy(name = name)

  lazy val model = Model(df, Model.default)

  def +(con: Con) = copy(sig = sig + con)
  def +(op: Op) = copy(sig = sig + op)
  def +(con: Con, constrs: Set[Op]) = copy(df = df + (con, constrs))
  def +(con: Con, params: List[TypeParam], rhs: Type) = copy(df = df + (con, params, rhs))
  def +(ax: Expr) = copy(df = df + ax)
  def +(name: String, fixity: Fixity) = copy(syntax = syntax + (name, fixity))
  def ++(that: Context) = Context(name, this.sig ++ that.sig, this.df ++ that.df, this.syntax ++ that.syntax)

  def ++(decls: List[Decl]): Context = {
    decls.foldLeft(this)(_ + _)
  }

  def +(decl: Decl): Context = decl match {
    case Import(name) =>
      this ++ load(name)

    case FixDecl(fixity, name) =>
      this + (name, fixity)

    case DataDef(_typ, constrs) =>
      val (schema, con) = toSchema(_typ)

      val (ctx, ops) = constrs.foldLeft((this + con, Set.empty[Op])) {
        case ((ctx, ops), OpDecl(name, _typ)) =>
          val typ = ctx toType _typ
          val op = Op(name, typ)
          (ctx + op, ops + op)
      }

      ctx + (con, ops)

    case TypeDef(_typ, _rhs) =>
      val (lhs, con) = toSchema(_typ)
      val ctx = this + con

      _rhs map toType match {
        case Some(rhs) =>
          ctx + (lhs.con, lhs.args, rhs)
        case None =>
          ctx
      }

    case OpDecl(name, _typ) =>
      val typ = toType(_typ)
      this + Op(name, typ)

    case OpDef(_expr) =>
      val (expr, typ) = toExpr(_expr)
      this + expr
    // out(expr + ": " + typ)

    case _ =>
      ???
  }

  def toExpr(_expr: Tree) = {
    val tree = syntax reorder _expr
    val preexpr = sig toExpr tree
    val i = new Infer(sig, df)
    i infer preexpr
  }

  def toType(_typ: Tree) = {
    val tree = syntax reorder _typ
    val typ = sig toType tree
    typ
  }

  def toSchema(_schema: Tree) = {
    val tree = syntax reorder _schema
    val schema = sig toSchema tree
    (schema, schema.con)
  }

  override def toString = sig + "\n" + df + "\n" + syntax
}

object Context {
  val empty = Context("empty", Sig.empty, Def.empty, Syntax.empty)
  val default = Context("default", Sig.default, Def.default, Syntax.default)
}