package ulang.source

import ulang.syntax.Con

sealed trait Expr

case class Id(name: String) extends Expr
case class App(fun: Expr, arg: Expr) extends Expr
case class Lambda(bound: Id, body: Expr) extends Expr

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

sealed trait Type

case class Schema(name: String, args: List[TypeParam]) {
  def con = Con(name, args.length)
}

case class TypeVar(index: Int) extends Type
case class TypeParam(name: String) extends Type
case class TypeApp(name: String, args: List[Type]) extends Type
case class TypeInst(orig: Type, gen: Type) extends Type