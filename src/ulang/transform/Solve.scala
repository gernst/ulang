package ulang.transform

import arse.control._
import scala.annotation.tailrec
import ulang.syntax
import ulang.source._

class Solve(sig: syntax.Sig) {
  type Constraint = (TypeVar, List[Type])
  import Unify._

  val unify = Unify.default

  def solve(cs: List[Constraint]) {
    for ((t, ts) <- cs)
      unify(t, ts)
  }

  def constraints(expr: Expr, scope: Map[String, TypeVar]): (TypeVar, List[Constraint]) = expr match {
    case Id(name) if scope contains name =>
      (scope(name), Nil)

    case Id(name) if sig.ops contains name =>
      ???
    /*
      (typ, Nil)

    case Op(name, typ: TypeVar) =>
      if (!(sig.ops contains name))
        fatal("in infer: " + expr + " is not in the signature")

      val ts = sig.ops(name) map (_.generic)
      (typ, List((typ, ts.toList)))

    case op @ Op(name, typ) =>
      if (!(sig contains op))
        fatal("in infer: " + expr + " is not in the signature")

      val to = TypeVar.fresh
      (to, List((to → List(typ.generic))))*/

    case App(fun, arg) =>
      val (tf, csf) = constraints(fun, scope)
      val (ta, csa) = constraints(arg, scope)
      val tr = TypeVar.fresh
      (tr, (tf, List(ta → tr)) :: csf ::: csa)

    case Lambda(bound, body) =>
      val (ta, _) = constraints(bound, scope)
      val (tb, cs) = constraints(body, scope + (bound.name -> ta))
      val tl = TypeVar.fresh
      (tl, (tl, List(ta → tb)) :: cs)

    case _ =>
      fatal("in infer: non-generic type for " + expr)
  }
}