package ulang.syntax

import arse.control._

case class Sig(cons: Map[String, Set[Int]], ops: Map[String, Set[Type]]) {
  def +(con: Con) = {
    val as = cons.getOrElse(con.name, Set.empty)
    copy(cons = cons + (con.name -> (as + con.arity)))
  }

  def +(op: Op) = op match {
    case Op(name, typ: TypeApp) =>
      val ts = ops.getOrElse(op.name, Set.empty)
      copy(ops = ops + (name -> (ts + typ)))
    case _ =>
      // this should be fatal?
      error("in sig: " + op + " has arbitrary type " + op.typ)
  }

  def ++(that: Sig) = {
    Sig(this.cons ++ that.cons, this.ops ++ that.ops)
  }

  def contains(con: Con) = con match {
    case Con(name, arity) =>
      (cons contains name) && (cons(name) contains arity)
  }

  def contains(op: Op) = op match {
    case Op(name, typ: TypeApp) =>
      (ops contains name) && (ops(name) contains typ)
    case _ =>
      false
  }

  override def toString = {
    val cs = cons.flatMap { case (name, arities) => arities.map(name + "/" + _) }
    val os = ops.flatMap { case (name, types) => types.map(name + ": " + _) }
    (cs ++ os).mkString("\n")
  }
}

object Sig {
  val empty = Sig(Map.empty[String, Set[Int]], Map.empty[String, Set[Type]])
  val default = Sig(List(Con.function, Con.bool), List(Op.equals, Op.if_then_else))
  
  def apply(cons: List[Con], ops: List[Op]): Sig = {
    var res = Sig.empty
    res = cons.foldLeft(res)(_ + _)
    res = ops.foldLeft(res)(_ + _)
    res
  }
}