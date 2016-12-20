package ulang.transform

import arse._
import ulang.DisjointSets
import ulang._
import ulang.syntax._
import ulang.expr._
import ulang.typ._

sealed trait Constraint {
  def size: Int
}

case class Eq(lhs: Alpha, rhs: Type) extends Constraint {
  def size = 1
}

case class Any(cs: List[Constraint]) extends Constraint {
  val size = cs.foldLeft(0)(_ + cs.size * _.size)
}

case class All(cs: List[Constraint]) extends Constraint {
  val size = cs.foldLeft(0)(_ + _.size)
}

case class Infer(sig: Sig, df: Defs) {
  import Unify._

  def infer(e: Expr, scope: Map[String, Type]): List[Subst] = {
    solve(constraints(e, scope))
  }

  def infer_def(e: Expr): List[Subst] = {
    solve(constraints_def(e))
  }

  def solve(cs: List[Constraint]): List[Subst] = {
    // println(cs)
    solve(Unify.default, All(cs))
  }

  def solve(thetas: List[Subst], c: Constraint): List[Subst] = c match {
    case Eq(lhs, rhs) => unify(lhs, rhs, thetas)
    case All(cs)      => cs.foldLeft(thetas)(solve)
    case Any(cs)      => cs.flatMap(solve(thetas, _))
  }

  def constraints_def(expr: Expr): List[Constraint] = {
    val ce = constraints_closed(expr)
    val op: Op = ??? // expr.defop
    val tso = sig.types(op.name)
    val tss = ??? // tso map df.synonym
    val tsn = tss // map nongeneric
    val co = ??? // Any(tsn map (op.typ ~> _))
    List(All(ce.sortBy(_.size)), co)
  }

  def constraints_closed(expr: Expr): List[Constraint] = {
    ???
    /*
    val fvs = expr.free filter {
      case FreeVar(name, _) => !sig.contains_op(name)
    }

    constraints(expr, fvs.map(id => (id.name, id.typ)).toMap)
    */
  }

  def constraints(expr: Expr, scope: Map[String, Type]): List[Constraint] = ??? /*expr match {
    case Id(name) if scope contains name =>
      val c = expr.typ ~> scope(name)
      c :: Nil

    case expr @ Id(name) if sig contains_op name =>
      val tso = sig.types(name)
      val tss = tso map df.synonym
      val tsg = tss map generic
      val cs = (tso, tsg).zipped map {
        case (to, tg) =>
          All(List(expr.orig ~> to, expr.typ ~> tg))
      }
      Any(cs) :: Nil

    case Id(name) =>
      fatal("in infer: unbound identifier " + expr)

    case Typed(expr, typ) =>
      val c = expr.typ ~> typ
      c :: Nil

    case App(fun, arg) =>
      val cf = constraints(fun, scope)
      val ca = constraints(arg, scope)
      val cr = fun.typ ~> (arg.typ → expr.typ)
      cr :: cf ::: ca

    case Lambda(bound, body) =>
      val cb = constraints(body, scope + (bound.name -> bound.typ))
      val cl = expr.typ ~> (bound.typ → body.typ)
      cl :: cb
  }*/

  def generic(typ: Type): Type = {
    var ren: Map[String, Alpha] = Map.empty

    def gen_rec(typ: Type): Type = typ match {
      case Param(name) if ren contains name =>
        ren(name)
      case Param(name) =>
        val alpha = Alpha.fresh()
        ren += name -> alpha
        alpha
      case TypeApp(name, args) =>
        TypeApp(name, args map gen_rec)
    }

    gen_rec(typ)
  }
}
