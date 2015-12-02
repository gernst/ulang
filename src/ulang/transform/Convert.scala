package ulang.transform

import ulang.syntax._
import ulang.source._

class Convert(sig: Sig) {
  def toType(tree: Tree): Type = tree match {
    case Id(name) =>
      if (sig.cons contains name) TypeApp(name, Nil)
      else TypeParam(name)

    case Node(tree :: Nil) =>
      toType(tree)

    case Node(Id(name) :: ts) =>
      val con = Con(name, ts.length)
      if (!(sig contains con))
        error("in convert: " + con + " is not in the signature")

      TypeApp(name, ts.map(toType))

    case _ =>
      error("in convert: unexpected type " + tree)
  }

  def toSchema(tree: Tree): Schema = tree match {
    case Id(name) if (sig.cons contains name) =>
      Schema(name, Nil)
    case Node(Id(name) :: args) if // (sig.cons contains name) && // NO! used to define
    args.forall(_.isInstanceOf[Id]) =>
      Schema(name, args.map { case Id(name) => TypeParam(name) })
    case _ =>
      error("in convert: not a type schema " + tree)
  }

  def toExpr(tree: Tree): Expr = {
    var scope: Map[String, FreeVar] = Map.empty

    def toExpr(tree: Tree, stack: List[Id]): Expr = tree match {
      case id @ Id(name) =>
        val index = stack.indexOf(id)
        if (index >= 0) {
          BoundVar(index)
        } else if (sig.ops contains name) {
          Op(name)
        } else if (scope contains name) {
          scope(name)
        } else {
          val x = FreeVar(name)
          scope += (name -> x)
          x
        }

      case Node(List(Id("λ"), Node(List(Id("."), Node(bound), body)))) if bound.forall(_.isInstanceOf[Id]) =>
        val params = bound map { case Id(name) => FreeVar(name) }
        Lambda(params, toExpr(body, (bound.asInstanceOf[List[Id]]) ++ stack))

      case Node(op :: args) =>
        App(toExpr(op, stack), args.map(toExpr(_, stack)))
    }

    toExpr(tree, Nil)
  }
}

object Convert {
  implicit def toConvert(sig: Sig) = new Convert(sig)
}