package ulang.transform

import arse._
import arse.Combinators._
import arse.control._
import ulang.syntax.Syntax
import ulang.source._

class Reorder(syntax: Syntax) extends Combinators with Primitives with Mixfix {
  type T = Tree

  type Op = Id
  type Expr = Tree

  val closed = __ collect {
    case id: Id if !(syntax contains id) =>
      id
    case Node(List(id: Id)) if (syntax contains id) =>
      id
    case Node(args) =>
      top(args)
  }

  def unary(op: Op, arg: Expr): Expr = Node(List(op, arg))
  def binary(op: Op, arg1: Expr, arg2: Expr) = Node(List(op, arg1, arg2))

  val normal_app = parse(Node)(closed +)
  val inner_expr: Parser[Tree, Expr] = normal_app

  def mixfix_op[A](m: Map[String, A], tree: Tree) = tree match {
    case id @ Id(name) if m contains name =>
      (id, m(name))
    case _ =>
      fail
  }

  def mixfix_op(s: Set[String], tree: Tree) = tree match {
    case id @ Id(name) if s contains name =>
      id
    case _ =>
      fail
  }

  val prefix_op = next { mixfix_op(syntax.prefix_ops, _) }
  val postfix_op = next { mixfix_op(syntax.postfix_ops, _) }
  val infix_op = next { mixfix_op(syntax.infix_ops, _) }
  val bindfix_op = next { mixfix_op(syntax.bindfix_ops, _) }

  val top = mixfix_expr $

  def reorder(tree: Tree): Tree = tree match {
    case id: Id =>
      id
    case Node(args) =>
      top(args)
  }
}

object Reorder {
  implicit def toReorder(syntax: Syntax) = new Reorder(syntax)
}