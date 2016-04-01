package ulang.syntax

import arse.Fixity
import arse.control._

sealed trait Expr {
  def abs(x: FreeVar, index: Int): Expr
  def bind(stack: List[FreeVar]): Expr
  def free: Set[FreeVar]
  def vars: Set[FreeVar]
  def mapFree(f: FreeVar => Expr): Expr

  def replace(e1: Expr, e2: Expr): Expr = this match {
    case `e1` => e2
    case App(fun, arg) => App(fun replace (e1, e2), arg replace (e1, e2))
    case Lambda(bound, body) => fatal("in replace: cannot replace in " + this)
    case _ => this
  }
}

case class Op(name: String, typ: Type) extends Expr with ulang.semantics.Data {
  override def toString = name // + ":" + typ
  def abs(x: FreeVar, index: Int) = this
  def bind(stack: List[FreeVar]) = this
  def free = Set.empty[FreeVar]
  def vars = Set.empty[FreeVar]
  def mapFree(f: FreeVar => Expr) = this
}

case class BoundVar(index: Int) extends Expr {
  override def toString = "@" + index
  def abs(x: FreeVar, index: Int) = this
  def bind(stack: List[FreeVar]) = this
  def free = Set.empty[FreeVar]
  def vars = Set.empty[FreeVar]
  def mapFree(f: FreeVar => Expr) = this
}

case class FreeVar(name: String, typ: Type) extends Expr {
  override def toString = "$" + name // + ":" + typ
  def abs(x: FreeVar, index: Int) = if (x == this) BoundVar(index) else this

  def bind(stack: List[FreeVar]) = {
    val index = stack.indexOf(this)
    if (index >= 0) BoundVar(index)
    else this
  }

  def free = Set(this)
  def vars = Set(this)
  def mapFree(f: FreeVar => Expr) = f(this)
}

case class App(fun: Expr, arg: Expr) extends Expr {
  override def toString = this match {
    case FlatApp(op, args) =>
      "(" + op + " " + args.mkString(" ") + ")"
  }

  def abs(x: FreeVar, index: Int) = App(fun abs (x, index), arg abs (x, index))
  def bind(stack: List[FreeVar]) = App(fun bind stack, arg bind stack)
  def free = fun.free ++ arg.free
  def vars = fun.vars ++ arg.vars
  def mapFree(f: FreeVar => Expr) = App(fun mapFree f, arg mapFree f)
}

case class Lambda(bound: FreeVar, body: Expr) extends Expr {
  override def toString = "(λ " + bound + ". " + body + ")"
  def abs(x: FreeVar, index: Int) = Lambda(bound, body abs (x, index + 1))
  def bind(stack: List[FreeVar]) = Lambda(bound, body bind (bound :: stack))
  def free = body.free - bound
  def vars = body.free + bound
  def mapFree(f: FreeVar => Expr) = Lambda(bound, body mapFree f)
}

case class Case(args: List[Expr], body: Expr)

case class Match(cases: List[Case]) extends Expr {
  override def toString = {
    val ss = cases.map { case Case(args, rhs) => args.mkString(" ") + ". " + rhs }
    ss.mkString("(λ ", " | ", ")")
  }
  def abs(x: FreeVar, index: Int) = ???
  def bind(stack: List[FreeVar]) = ???
  def free = ???
  def vars = ???
  def mapFree(f: FreeVar => Expr) = ???
}

object Op {
  import Type._
  def equals = Op("=", alpha → (alpha → bool))
  val if_then_else = Op("if_then_else", bool → (alpha → (alpha → alpha)))
}

object App {
  def apply(fun: Expr, args: List[Expr]): Expr = {
    args.foldLeft(fun)(App(_, _))
  }
}

object Lambda {
  def apply(bound: List[FreeVar], body: Expr): Expr = {
    bound.foldRight(body)(Lambda(_, _))
  }
}