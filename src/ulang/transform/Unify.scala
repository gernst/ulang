package ulang.transform

import ulang.syntax._
import arse.control._
import ulang.DisjointSets
import scala.language.implicitConversions

class Unify(var thetas : List[Unify.Subst]) {
  import Unify._

  def apply(t1: Type, t2: Type) {
    val res = thetas.map { (theta: Subst) => some(apply(t1, t2, theta)) or none }
    thetas = res.collect { case Some(theta) => theta }
  }

  def apply(t1: Type, ts: List[Type]) {
    for(t2 <- ts) apply(t1, t2)
  }

  def apply(ts1: List[Type], ts2: List[Type], theta: Subst): Subst = {
    (ts1 zip ts2).foldLeft(theta) {
      case (theta, (t1, t2)) => apply(t1, t2, theta)
    }
  }

  def apply(t1: Type, t2: Type, theta: Subst): Subst = {
    val r1 = theta find t1
    val r2 = theta find t2
    // println("in apply: " + r1 + "  =  " + r2 + " in " + theta)

    (r1, r2) match {
      case (t1, t2) if t1 == t2 =>
        theta
      case (a1: TypeVar, t2) =>
        // println("in apply: occurs check")
        if (contains(t2, a1, theta)) fail
        theta.union(a1, t2)
      case (t1, a2: TypeVar) =>
        apply(a2, t1, theta)
      case (TypeInst(_, t1), t2) =>
        apply(t1, t2, theta)
      case (t1, TypeInst(_, t2)) =>
        apply(t1, t2, theta)
      case (TypeApp(name1, args1), TypeApp(name2, args2)) =>
        if (name1 != name2 || args1.length != args2.length)
          fail
        apply(args1, args2, theta)
    }
  }

  def contains(typ: Type, vr: TypeVar, theta: Subst): Boolean = (theta find typ) match {
    case `vr` => true
    case TypeApp(_, args) => args.exists(contains(_, vr, theta))
    case TypeInst(_, typ) => contains(typ, vr, theta)
    case _ => false
  }
}

object Unify {
  type Subst = DisjointSets[Type]
}