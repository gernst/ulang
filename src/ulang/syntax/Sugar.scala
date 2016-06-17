package ulang.syntax

import scala.annotation.tailrec

object Function {
  def apply(t1: Type, t2: Type) = TypeApp("→", List(t1, t2))

  def unapply(t: Type): Option[(Type, Type)] = t match {
    case TypeApp("→", List(t1, t2)) =>
      Some((t1, t2))
    case _ =>
      None
  }
}

class Unary(val op: Op) extends (Expr => Expr) {
  def unapply(e: Expr) = e match {
    case App(`op`, arg) =>
      Some(arg)
    case _ =>
      None
  }

  def apply(arg: Expr) = {
    App(op, arg)
  }
}

class Binary(val op: Op) extends ((Expr, Expr) => Expr) {
  def unapply(e: Expr) = e match {
    case App(App(`op`, arg1), arg2) =>
      Some((arg1, arg2))
    case _ =>
      None
  }

  def apply(arg1: Expr, arg2: Expr) = {
    App(App(op, arg1), arg2)
  }
}

class Ternary(val op: Op) extends ((Expr, Expr, Expr) => Expr) {
  def unapply(e: Expr) = e match {
    case App(App(App(`op`, arg1), arg2), arg3) =>
      Some((arg1, arg2, arg3))
    case _ =>
      None
  }

  def apply(arg1: Expr, arg2: Expr, arg3: Expr) = {
    App(App(App(op, arg1), arg2), arg3)
  }
}

class Nary(val op: Op, val neutral: Op) {
  def flatArgs(e: Expr): List[Expr] = e match {
    case App(App(`op`, arg1), arg2) =>
      flatArgs(arg1) ++ flatArgs(arg2)
    case _ =>
      List(e)
  }

  def unapply(e: Expr) = flatArgs(e) match {
    case List(_) => None
    case args    => Some(args)
  }

  def apply(args: List[Expr]): Expr = {
    if (args.isEmpty) neutral
    else args.reduce((arg1, arg2) => App(App(op, arg1), arg2))
  }
}

class Binder(val op: Op) extends (FreeVar => Expr) {
  def apply(bound: FreeVar, body: Expr): Expr = {
    Bind(op, bound, body)
  }

  def unapply(expr: Expr): Option[(FreeVar, Expr)] = expr match {
    case Bind(`op`, bound, body) =>
      Some((bound, body))
    case _ =>
      None
  }
}

object Bind extends ((Op, FreeVar, Expr) => Expr) {
  def apply(op: Op, bound: FreeVar, body: Expr): Expr = {
    App(op, Lambda(bound, body))
  }

  def unapply(expr: Expr): Option[(Op, FreeVar, Expr)] = expr match {
    case App(op: Op, Lambda(bound: FreeVar, body)) =>
      Some((op, bound, body))
    case _ =>
      None
  }
}

object Apps extends ((Expr, List[Expr]) => Expr) {
  def apply(fun: Expr, args: List[Expr]): Expr = {
    args.foldLeft(fun)(App)
  }

  def unapply(expr: Expr): Option[(Expr, List[Expr])] = {
    Some(flatten(expr, Nil))
  }

  def flatten(expr: Expr, args: List[Expr]): (Expr, List[Expr]) = expr match {
    case App(fun, arg) =>
      flatten(fun, arg :: args)
    case _ =>
      (expr, args)
  }
}

object Lambdas extends ((List[FreeVar], Expr) => Expr) {
  def apply(bounds: List[FreeVar], body: Expr): Expr = {
    bounds.foldRight(body)(Lambda)
  }

  def unapply(expr: Expr): Option[(List[Expr], Expr)] = {
    Some(flatten(expr))
  }

  def flatten(expr: Expr): (List[Expr], Expr) = expr match {
    case Lambda(bound, body) =>
      val (bounds, inner) = flatten(body)
      (bound :: bounds, inner)
    case _ =>
      (Nil, expr)
  }
}

object Binds extends ((Op, List[FreeVar], Expr) => Expr) {
  def apply(op: Op, bounds: List[FreeVar], body: Expr): Expr = {
    bounds.foldRight(body)(Bind(op, _, _))
  }

  def unapply(expr: Expr): Option[(Op, List[Expr], Expr)] = expr match {
    case Bind(op, bound, body) =>
      val (bounds, inner) = flatten(op, body)
      Some((op, bound :: bounds, inner))
    case _ =>
      None
  }

  def flatten(op: Op, expr: Expr): (List[Expr], Expr) = expr match {
    case Bind(`op`, bound, body) =>
      val (bounds, inner) = flatten(op, body)
      (bound :: bounds, inner)
    case _ =>
      (Nil, expr)
  }
}