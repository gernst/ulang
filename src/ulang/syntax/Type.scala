package ulang.syntax

import arse._

case class Con(name: String, arity: Int) {
  override def toString = name + "/" + arity
}

object Con {
  val function = Con("→", 2)
  val bool = Con("Bool", 0)
}

case class Schema(name: String, args: List[TypeVar]) {
  def con = Con(name, args.length)
  override def toString = (name :: args).mkString("(", " ", ")")
}

// case class Poly(args: List[TypeVar], body: Type)

sealed trait Type {
  def →(that: Type): Type = TypeApp("→", List(this, that))

  def arity: Int = this match {
    case Function(t1, t2) =>
      1 + t2.arity
    case _ =>
      0
  }

  def subst(theta: Map[TypeVar, Type]): Type
  def isBool = (this == Type.bool)
}

case class TypeVar(name: String, index: Option[Int]) extends Type {
  def subst(theta: Map[TypeVar, Type]) = theta.getOrElse(this, this)
  override def toString = "$" + name
}

case class TypeApp(name: String, args: List[Type] = Nil) extends Type {
  def con = Con(name, args.length)
  def subst(theta: Map[TypeVar, Type]) = TypeApp(name, args map (_ subst theta))
  override def toString = (name :: args).mkString("(", " ", ")")
}

object Type {
  val bool = TypeApp("Bool")

  val alpha = TypeVar("α")
  val beta = TypeVar("β")
  val gamma = TypeVar("γ")
  
  val none = TypeVar("", None)
}

object TypeVar extends (String => TypeVar) {
  def apply(name: String) = TypeVar(name, None)
  
  var index = 0
  def fresh(name: String) = {
    index += 1
    TypeVar(name, Some(index))
  }
}