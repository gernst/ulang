package ulang.syntax

import arse.control._
import ulang.source._

case class Sig(cons: Map[String, List[Int]], ops: Map[String, List[Type]]) {
  def +(con: Con) = {
    val as = cons.getOrElse(con.name, Nil)
    copy(cons = cons + (con.name -> (as :+ con.arity)))
  }

  def +(op: Op) = op match {
    case Op(name, typ: TypeApp) =>
      val ts = ops.getOrElse(op.name, Nil)
      copy(ops = ops + (name -> (ts :+ typ)))
    case _ =>
      // this should be fatal?
      error("in sig: " + op + " has arbitrary type " + op.typ)
  }

  def ++(that: Sig) = {
    Sig(this.cons ++ that.cons, this.ops ++ that.ops)
  }

  def ++(decls: List[Decl]): Sig = {
    decls.foldLeft(this)(_ + _)
  }

  def +(decl: Decl): Sig = decl match {
    case Import(thy) => this ++ thy.sig
    case TypeDecl(con) => this + con
    case DataDef(_, constrs) => this ++ constrs
    case OpDecl(name, typ) => this + Op(name, typ)
    case _ => this
  }

  def contains(con: Con) = con match {
    case Con(name, arity) =>
      (cons contains name) && (cons(name) contains arity)
  }

  def contains(op: Op) = op match {
    case Op(name, typ) =>
      (ops contains name) && (ops(name) contains typ)
  }

  override def toString = {
    val cs = cons.flatMap { case (name, arities) => arities.map(name + "/" + _) }
    val os = ops.flatMap { case (name, types) => types.map(name + ": " + _) }
    (cs ++ os).mkString("\n")
  }
}

object Sig {
  def apply(decls: List[Decl]): Sig = empty ++ decls
  
  val empty = Sig(Map.empty[String, List[Int]], Map.empty[String, List[Type]])
  val default = Sig(List(Con.function, Con.bool), List(Op.equals, Op.if_then_else))

  def apply(cons: List[Con], ops: List[Op]): Sig = {
    var res = Sig.empty
    res = cons.foldLeft(res)(_ + _)
    res = ops.foldLeft(res)(_ + _)
    res
  }
}