package ulang.syntax

import arse.control._

case class Con(name: String, arity: Int) {
  override def toString = name + "/" + arity
}

object Con {
  val function = Con("→", 2)
  val bool = Con("Bool", 0)
}

case class Schema(name: String, args: List[TypeParam]) {
  def con = Con(name, args.length)
  override def toString = (name :: args).mkString("(", " ", ")")
}

// case class Poly(args: List[TypeParam], body: Type)

sealed trait Type {
  def →(that: Type): Type = TypeApp("→", List(this, that))

  def arity: Int = this match {
    case predefined.pred.Function(t1, t2) =>
      1 + t2.arity
    case _ =>
      0
  }

  def subst(theta: Map[TypeParam, Type]): Type
  def isBool = (this == Type.bool)
}

case class TypeParam(name: String) extends Type {
  def subst(theta: Map[TypeParam, Type]) = theta.getOrElse(this, this)
  override def toString = "$" + name
}

case class TypeApp(name: String, args: List[Type] = Nil) extends Type {
  def con = Con(name, args.length)
  def subst(theta: Map[TypeParam, Type]) = TypeApp(name, args map (_ subst theta))
  override def toString = (name :: args).mkString("(", " ", ")")
}

object Type {
  val bool = TypeApp("Bool")

  val alpha = TypeParam("α")
  val beta = TypeParam("β")
  val gamma = TypeParam("γ")
}
