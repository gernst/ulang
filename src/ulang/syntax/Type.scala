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

  def generic: Type = {
    var ren: Map[String, TypeVar] = Map.empty

    def gen_rec(typ: Type): Type = typ match {
      case TypeParam(name) if ren contains name =>
        ren(name)
      case TypeParam(name) =>
        val alpha = TypeVar.fresh
        ren += name -> alpha
        alpha
      case TypeApp(name, args) =>
        new TypeApp(name, args map gen_rec)
      case _ =>
        fatal("in gen: " + typ + " unexpected")
    }

    TypeInst(this, gen_rec(this))
  }

  def arity: Int = this match {
    case predefined.pred.Function(t1, t2) =>
      1 + t2.arity
    case _ =>
      0
  }

  def isConcrete: Boolean = this match {
    case _: TypeVar | _: TypeInst => false
    case TypeApp(_, args) => args forall (_.isConcrete)
    case _ => true
  }

  def subst(theta: Map[TypeParam, Type]): Type
}

case class TypeVar(index: Int) extends Type {
  def subst(theta: Map[TypeParam, Type]) = this
  override def toString = "?" + index
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

case class TypeInst(orig: Type, gen: Type) extends Type {
  def subst(theta: Map[TypeParam, Type]) = fatal("in subst: type instance unexpected " + this)
  override def toString = gen.toString
}

object Type {
  val bool = TypeApp("Bool")

  val alpha = TypeParam("α")
  val beta = TypeParam("β")
  val gamma = TypeParam("γ")
}

object TypeVar {
  var index = 0
  def nextIndex = { index += 1; index }

  def fresh: TypeVar = TypeVar(nextIndex)
}