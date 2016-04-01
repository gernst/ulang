package ulang.syntax

import arse.control._
import ulang.syntax._
import ulang.MultiMap._
import arse.Fixity
import ulang.source.Syntax

case class Sig(cons: List[Con], ops: List[Op], syntax: Syntax) {
  // assert(cons.distinct == cons)
  // assert(ops.distinct == ops)

  def +(con: Con) = copy(cons = cons :+ con)
  def +(op: Op) = copy(ops = ops :+ op)
  def +(name: String, fixity: Fixity) = copy(syntax = syntax + (name, fixity))

  def contains_con(name: String) = cons.exists(_.name == name)
  def contains_con(name: String, arity: Int) = cons contains Con(name, arity)

  def contains_op(name: String) = ops.exists(_.name == name)
  def contains_op(name: String, typ: Type) = ops contains Op(name, typ)

  def types(name: String) = ops collect {
    case Op(`name`, typ) => typ
  }

  def ++(that: Sig) = {
    Sig(this.cons ++ that.cons, this.ops ++ that.ops, this.syntax ++ that.syntax)
  }

  override def toString = {
    (cons ++ ops).mkString("\n")
  }
}

object Sig {
  val empty = Sig(Nil, Nil, Syntax.empty)
  val default = Sig(List(Con.function, Con.bool), List(Op.equals, Op.if_then_else), Syntax.default)
}