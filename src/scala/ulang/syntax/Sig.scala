package ulang.syntax

import arse._

import ulang.syntax._
import ulang.expr
import ulang.expr._
import ulang.typ._

case class Sig(cons: List[Con], ops: List[Op]) {
  // assert(cons.distinct == cons)
  // assert(ops.distinct == ops)

  def contains_con(name: String) = cons.exists(_.name == name)
  def contains_con(name: String, arity: Int) = cons contains Con(name, arity)

  def contains_op(name: String) = ops.exists(_.a._1 == name)
  def contains_op(name: String, typ: Type) = ops contains Op(name, typ)

  def types(name: String) = ops collect {
    case expr.Free((`name`, typ)) => typ
  }

  def ++(that: Sig) = {
    Sig(this.cons ++ that.cons, this.ops ++ that.ops)
  }

  override def toString = {
    (cons ++ ops).mkString("\n")
  }
}

object Sig {
  val empty = Sig(Nil, Nil)
  val default = Sig(List(Con.function, Con.bool), List(Eq.op, IfThenElse.op))
}