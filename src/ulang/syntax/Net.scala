package ulang.syntax

import arse.control._
import scala.annotation.tailrec
import ulang.syntax._

case class Net(ops: Map[Op, Net], app: Option[Net], bound: Option[Net], rhs: List[Expr]) {
  def insert(args: List[Expr], stack: List[FreeVar], expr: Expr): Net = args match {
    case Nil =>
      copy(rhs = rhs :+ (expr bind stack))

    case (op: Op) :: rest =>
      val sub = ops.getOrElse(op, Net.empty)
      copy(ops = ops + (op -> sub.insert(rest, stack, expr)))

    case App(fun, arg) :: rest =>
      val sub = app.getOrElse(Net.empty)
      copy(app = Some(sub.insert(fun :: arg :: rest, stack, expr)))

    case (fv: FreeVar) :: rest =>
      val sub = bound.getOrElse(Net.empty)
      copy(bound = Some(sub.insert(rest, fv :: stack, expr)))

    case arg :: rest =>
      error("in net: " + arg + " is not a pattern")
  }

  def +(args_expr: (List[Expr], Expr)) = args_expr match {
    case (args, expr) => this.insert(args, Nil, expr)
  }

  def format(ident: Int): String = {
    val sp = "  " * ident
    var res = ""
    for ((op, net) <- ops) {
      res += sp + "op " + op + "\n"
      res += net.format(ident + 1)
    }
    for (net <- app) {
      res += sp + "app\n"
      res += net.format(ident + 1)
    }
    for (net <- bound) {
      res += sp + "bind\n"
      res += net.format(ident + 1)
    }
    for (expr <- rhs) {
      res += sp + "rhs\n"
      res += sp + "  " + expr + "\n"
    }
    res
  }

  override def toString = format(0)
}

object Net {
  def apply(cases: List[(List[Expr], Expr)]): Net = {
    cases.foldLeft(Net.empty)(_ + _)
  }

  val empty = Net(Map.empty, None, None, Nil)
}