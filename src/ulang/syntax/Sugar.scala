package ulang.syntax

import scala.annotation.tailrec

class Unary(op: Op) extends (Expr => Expr) {
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

class Binary(op: Op) extends ((Expr, Expr) => Expr) {
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

class Ternary(op: Op) extends ((Expr, Expr, Expr) => Expr) {
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

class Nary(op: Op, neutral: Op) {
  def flatArgs(e: Expr): List[Expr] = e match {
    case App(App(`op`, arg1), arg2) =>
      flatArgs(arg1) ++ flatArgs(arg2)
    case _ =>
      List(e)
  }

  def unapply(e: Expr) = flatArgs(e) match {
    case List(_) => None
    case args => Some(args)
  }

  def apply(args: List[Expr]): Expr = {
    if (args.isEmpty) neutral
    else args.reduce((arg1, arg2) => App(App(op, arg1), arg2))
  }
}

class Binder(op: Op) extends ((FreeVar, Expr) => Expr) {
  def unapply(e: Expr) = e match {
    case App(`op`, Lambda(bound, body)) =>
      Some((bound, body))
    case _ =>
      None
  }

  def apply(bounds: List[FreeVar], body: Expr): Expr = {
    App(op, Lambda(bounds, body))
  }

  def apply(bound: FreeVar, body: Expr): Expr = {
    App(op, Lambda(bound, body))
  }
}

object FlatApp {
  @tailrec
  def flatten(e: Expr, args: List[Expr]): Option[(Op, List[Expr])] = e match {
    case op: Op =>
      Some((op, args))
    case App(fun, arg) =>
      flatten(fun, arg :: args)
    case _ =>
      None
  }

  def unapply(e: Expr): Option[(Op, List[Expr])] = {
    flatten(e, Nil)
  }
}