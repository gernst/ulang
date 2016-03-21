package ulang.transform

import ulang.DisjointSets
import ulang.source._
import ulang.syntax._
import arse.control._
import ulang.syntax.predefined.pred.Eq

class Infer(sig: Sig, df: Defs) {
  import Unify._

  val unify = new Unify(Nil)

  def check_overloading(expr: Expr, pairs: List[(Expr, Type)]): (Expr, Type) = pairs match {
    case Nil =>
      error("in infer: no type for " + expr)

    case List((expr, typ)) =>
      // println("in infer: subst = " + theta)

      if (!typ.isConcrete)
        error("in infer: unspecified type of " + expr + ": " + typ)

      (expr, typ)

    case _ =>
      error("in infer: unresolved overloading for: " + expr)
  }

  def infer(expr: Expr): (Expr, Type) = {
    val (typ) = infer(expr, Nil)
    check_overloading(expr, subst(expr, typ))
  }

  def infer_def(op: Op, args: List[Expr], _rhs: Expr): (Expr, Type) = {
    val (lhs, rhs, typ) = infer_eq(App(op, args), _rhs)
    (Eq(lhs, rhs), Type.bool)
  }

  def infer_defs(name: String, args: List[Expr], rhs: Expr): (Expr, Type) = {
    val ts = sig.ops(name)
    val ops = ts map (Op(name, _))
    val preexpr = Eq(App(Op(name), args), rhs)

    val solve = new Solve(sig)
    val (ct, cs) = solve.constraints(preexpr, Nil)
    println("constraints for " + preexpr + ": " + ct)
    for ((t, ts) <- cs) {
      println("  " + t + " in " + ts.mkString("{ ", ", ", " }"))
    }
    println()

    check_overloading(preexpr, ops map (infer_def(_, args, rhs)))
  }

  def infer_eq(_lhs: Expr, _rhs: Expr): (Expr, Expr, Type) = {
    val _ltyp = infer(_lhs, Nil)
    val _rtyp = infer(_rhs, Nil)
    unify(_rtyp, _ltyp)
    val (lhs, ltyp) = check_overloading(_lhs, subst(_lhs, _ltyp))
    val (rhs, rtyp) = check_overloading(_rhs, subst(_rhs, _rtyp))
    assert(ltyp == rtyp)
    (lhs, rhs, ltyp)
  }

  def infer(expr: Expr, stack: List[FreeVar]): Type = {
    val typ = _infer(expr, stack)
    if (unify.thetas.isEmpty)
      error("in infer: no type for " + expr)

    // val ts = new_thetas.map(_.find(typ))
    // println("in infer: " + expr + " in " + ts.mkString("{ ", ", ", " }"))

    typ
  }

  def _infer(expr: Expr, stack: List[FreeVar]): Type = expr match {
    // case FreeVar(_, typ: TypeVar) =>
    //   fatal("in infer: unbound variable " + expr)

    case FreeVar(_, typ) =>
      (typ)

    case BoundVar(index) =>
      val FreeVar(_, typ) = stack(index)
      (typ)

    case Op(name, typ: TypeVar) =>
      if (!(sig.ops contains name))
        fatal("in infer: " + expr + " is not in the signature")

      val ts0 = sig.ops(name)
      val ts1 = ts0 map (df synonym _.generic)
      ts1 foreach (unify(typ, _))
      typ

    case op @ Op(name, typ) =>
      if (!(sig contains op))
        fatal("in infer: " + expr + " is not in the signature")
      (typ)

    case App(fun, arg) =>
      val ft = infer(fun, stack)
      val at = infer(arg, stack)
      val rt = TypeVar.fresh
      unify(ft, at → rt)

      if (unify.thetas.isEmpty) {
        val fts = unify.thetas.map(_.find(ft))
        println("in infer: " + fun + " in " + fts.mkString("{ ", ", ", " }"))
        val ats = unify.thetas.map(_.find(at))
        println("in infer: " + arg + " in " + ats.mkString("{ ", ", ", " }"))
        error("in infer: cannot apply " + fun + " to " + arg)
      }

      rt

    case Lambda(bound, body) =>
      val (at) = infer(bound, stack)
      val (bt) = infer(body, bound :: stack)
      (at → bt)

    case _ =>
      error("in infer: cannot type " + expr)
  }

  def subst(expr: Expr, typ: Type): List[(Expr, Type)] = {
    unify.thetas map { theta => (subst(expr, theta), subst(typ, theta)) }
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
}