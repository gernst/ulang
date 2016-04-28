package ulang.source

import arse._

sealed trait Expr {
  val typ = TypeVar.fresh
  
  def op: Id
  def free: Set[Id]

  def defop = this match {
    case Eq(lhs, rhs) => lhs.op
    case _ => error("in expr: not a definition " + this)
  }
}

case class Id(name: String) extends Expr with Type {
  override def toString = name
  val orig = TypeVar.fresh
  def op = this
  def free = Set(this)
}

case class Typed(expr: Expr, orig: Type) extends Expr {
  override def toString = "(" + expr + ": " + orig + ")"
  def op = expr.op
  def free = expr.free
}

case class App(fun: Expr, arg: Expr) extends Expr {
  override def toString = "(" + fun + " " + arg + ")"
  def op = fun.op
  def free = fun.free ++ arg.free
}

case class Lambda(bound: Id, body: Expr) extends Expr {
  override def toString = "(λ " + bound + ". " + body + ")"
  def op = fatal("in expr: no top-level function symbol in " + this)
  def free = body.free - bound
}

object App {
  def apply(fun: Expr, args: List[Expr]): Expr = {
    args.foldLeft(fun)(App(_, _))
  }
}

object Lambda {
  def apply(bound: List[Id], body: Expr): Expr = {
    bound.foldRight(body)(Lambda(_, _))
  }
}

object Eq {
  def apply(lhs: Expr, rhs: Expr): Expr = {
    App(Id("="), List(lhs, rhs))
  }

  def unapply(expr: Expr): Option[(Expr, Expr)] = expr match {
    case App(App(Id("="), lhs), rhs) => Some((lhs, rhs))
    case _ => None
  }
}

sealed trait Type {
  def →(that: Type): Type = TypeApp("→", List(this, that))
}

case class TypeVar(index: Int) extends Type {
  override def toString = "?" + index
  def ~>(that: Type) = ulang.transform.Eq(this, that)
  def ~>(that: ulang.syntax.Type) = ulang.transform.Eq(this, TypeInst(that))
}

case class TypeApp(name: String, args: List[Type] = Nil) extends Type {
  override def toString = (name :: args).mkString("(", " ", ")")
}

case class TypeInst(orig: ulang.syntax.Type) extends Type {
  override def toString = orig.toString
}

object TypeVar {
  var index = 0
  def nextIndex = { index += 1; index }

  def fresh: TypeVar = TypeVar(nextIndex)
}
