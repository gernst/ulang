package ulang.transform

import arse.control._
import ulang.syntax._
import scala.annotation.tailrec

class Solve(sig: Sig) {
  type Constraint = (TypeVar, List[Type])
  import Unify._

  val unify = new Unify(Nil)

  def solve(cs: List[Constraint]) {
    for ((t, ts) <- cs)
      unify(t, ts)
  }

  def constraints(expr: Expr, stack: List[FreeVar]): (TypeVar, List[Constraint]) = expr match {
    case FreeVar(name, typ: TypeVar) =>
      (typ, Nil)

    case BoundVar(index) =>
      constraints(stack(index), stack)

    case Op(name, typ: TypeVar) =>
      if (!(sig.ops contains name))
        fatal("in infer: " + expr + " is not in the signature")

      val ts = sig.ops(name) map (_.generic)
      (typ, List((typ, ts.toList)))

    case op @ Op(name, typ) =>
      if (!(sig contains op))
        fatal("in infer: " + expr + " is not in the signature")

      val to = TypeVar.fresh
      (to, List((to → List(typ.generic))))

    case App(fun, arg) =>
      val (tf, csf) = constraints(fun, stack)
      val (ta, csa) = constraints(arg, stack)
      val tr = TypeVar.fresh
      (tr, (tf, List(ta → tr)) :: csf ::: csa)

    case Lambda(bound, body) =>
      val (ta, _) = constraints(bound, stack)
      val (tb, cs) = constraints(body, bound :: stack)
      val tl = TypeVar.fresh
      (tl, (tl, List(ta → tb)) :: cs)

    case _ =>
      fatal("in infer: non-generic type for " + expr)
  }
}