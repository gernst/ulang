package ulang.transform

import ulang.source._
import arse._
import ulang.DisjointSets
import scala.language.implicitConversions

object Unify {
  type Subst = DisjointSets[Type]
  def default = List(DisjointSets.empty: Subst)

  def unify(t1: Type, t2: Type, thetas: List[Subst]): List[Subst] = {
    val res = thetas.map { (theta: Subst) => some(unify(t1, t2, theta)) or none }
    res.collect { case Some(theta) => theta }
  }

  def unify(ts1: List[Type], ts2: List[Type], theta: Subst): Subst = {
    (ts1 zip ts2).foldLeft(theta) {
      case (theta, (t1, t2)) => unify(t1, t2, theta)
    }
  }

  def unify(t1: Type, t2: Type, theta: Subst): Subst = {
    val r1 = theta find t1
    val r2 = theta find t2
    // println("in unify: " + r1 + "  =  " + r2 + " in " + theta)

    (r1, r2) match {
      case (t1, t2) if t1 == t2 =>
        theta
      case (a1: TypeVar, t2) =>
        // println("in unify: occurs check")
        if (contains(t2, a1, theta)) fail
        theta.union(a1, t2)
      case (t1, a2: TypeVar) =>
        unify(a2, t1, theta)
      /*case (TypeInst(_, t1), t2) =>
        unify(t1, t2, theta)
      case (t1, TypeInst(_, t2)) =>
        unify(t1, t2, theta)*/
      case (TypeApp(name1, args1), TypeApp(name2, args2)) =>
        if (name1 != name2 || args1.length != args2.length)
          fail
        unify(args1, args2, theta)
      case _ =>
        fail
    }
  }

  def contains(typ: Type, vr: TypeVar, theta: Subst): Boolean = (theta find typ) match {
    case `vr` => true
    case TypeApp(_, args) => args.exists(contains(_, vr, theta))
    // case TypeInst(_, typ) => contains(typ, vr, theta)
    case _ => false
  }
}
