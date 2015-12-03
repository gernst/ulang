package ulang.transform

import de.gidonernst.util.DisjointSets
import ulang.source._
import ulang.syntax._
import arse.control._

class Infer(sig: Sig, df: Defs) {
  import Unify._

  def infer(preexpr: Expr): (Expr, Type) = {
    val (ityp, thetas) = infer(preexpr, Nil, List(DisjointSets.empty))

    val (expr, rtyp) = thetas match {
      case Nil =>
        error("in infer: no type for " + preexpr)
      case List(theta) =>
        // println("in infer: subst = " + theta)
        (subst(preexpr, theta), subst(ityp, theta))
      case _ =>
        error("in infer: unresolved overloading for: " + preexpr)
    }

    if (rtyp.isInstanceOf[TypeVar])
      error("in infer: unspecified type of " + preexpr + ": " + rtyp)

    (expr, rtyp)
  }

  def infer(expr: Expr, stack: List[FreeVar], thetas: List[Subst]): (Type, List[Subst]) = {
    val (typ, new_thetas) = _infer(expr, stack, thetas)
    if (new_thetas.isEmpty)
      error("in infer: no type for " + expr)

    // val ts = new_thetas.map(_.find(typ))
    // println("in infer: " + expr + " in " + ts.mkString("{ ", ", ", " }"))

    (typ, new_thetas)
  }

  def unify(t1: Type, t2: Type, thetas: List[Subst]) = {
    sig unify (t1, t2, thetas)
  }

  def _infer(expr: Expr, stack: List[FreeVar], thetas: List[Subst]): (Type, List[Subst]) = expr match {
    // case FreeVar(_, typ: TypeVar) =>
    //   fatal("in infer: unbound variable " + expr)

    case FreeVar(_, typ) =>
      (typ, thetas)

    case BoundVar(index) =>
      val FreeVar(_, typ) = stack(index)
      (typ, thetas)

    case Op(name, typ: TypeVar) =>
      if (!(sig.ops contains name))
        fatal("in infer: " + expr + " is not in the signature")

      val ts0 = sig.ops(name).toList
      val ts1 = ts0 map (df synonym _.generic)
      (typ, ts1.flatMap(unify(typ, _, thetas)))

    case App(fun, arg) =>
      val (ft, thetas1) = infer(fun, stack, thetas)
      val (at, thetas2) = infer(arg, stack, thetas1)
      val rt = TypeVar.fresh
      val thetas3 = unify(ft, at → rt, thetas2)

      if (thetas3.isEmpty) {
        val fts = thetas2.map(_.find(ft))
        println("in infer: " + fun + " in " + fts.mkString("{ ", ", ", " }"))
        val ats = thetas2.map(_.find(at))
        println("in infer: " + arg + " in " + ats.mkString("{ ", ", ", " }"))
        error("in infer: cannot apply " + fun + " to " + arg)
      }
      (rt, thetas3)

    case Lambda(bound, body) =>
      val (at, thetas1) = infer(bound, stack, thetas)
      val (bt, thetas2) = infer(body, bound :: stack, thetas1)
      (at → bt, thetas2)

    case _ =>
      error("in infer: cannot type " + expr)
  }

  def subst(typ: Type, theta: Subst): Type = typ match {
    case v: TypeVar =>
      val t = theta.find(v)
      if (v == t) v
      else subst(t, theta)
    case TypeInst(orig, _) => orig
    case _: TypeParam => typ
    case TypeApp(name, args) => TypeApp(name, args.map(subst(_, theta)))
  }

  def subst(expr: Expr, theta: Subst): Expr = expr match {
    case FreeVar(name, typ) => FreeVar(name, subst(typ, theta))
    case Op(name, typ) => Op(name, subst(typ, theta))
    case App(fun, arg) => App(subst(fun, theta), subst(arg, theta))
    case Lambda(bound, body) => Lambda(subst(bound, theta).asInstanceOf[FreeVar], subst(body, theta))
    case _ => expr
  }

  type Constraint = (TypeVar, Set[Type])

  def constraints(expr: Expr, stack: List[FreeVar]): (TypeVar, List[Constraint]) = expr match {
    case FreeVar(name, typ: TypeVar) =>
      (typ, Nil)

    case BoundVar(index) =>
      constraints(stack(index), stack)

    case Op(name, typ: TypeVar) =>
      if (!(sig.ops contains name))
        fatal("in infer: " + expr + " is not in the signature")

      val ts = sig.ops(name) map (_.generic)
      (typ, List((typ, ts)))

    case op @ Op(name, typ) =>
      if (!(sig contains op))
        fatal("in infer: " + expr + " is not in the signature")

      val to = TypeVar.fresh
      (to, List((to → Set(typ.generic))))

    case App(fun, arg) =>
      val (tf, csf) = constraints(fun, stack)
      val (ta, csa) = constraints(arg, stack)
      val tr = TypeVar.fresh
      (tr, (tf, Set(ta → tr)) :: csf ::: csa)

    case Lambda(bound, body) =>
      val (ta, _) = constraints(bound, stack)
      val (tb, cs) = constraints(body, bound :: stack)
      val tl = TypeVar.fresh
      (tl, (tl, Set(ta → tb)) :: cs)

    case _ =>
      fatal("in infer: non-generic type for " + expr)
  }
}

object Infer {
  implicit def toInfer(sig: Sig, df: Defs) = new Infer(sig, df)
}