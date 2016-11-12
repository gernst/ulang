package ulang.expr

import ulang.syntax.Sig
import ulang.typ._

/*
object Infer {
  val sig: Sig = ???

  /* assumptions:
   * - variables are bound properly
   * - free variables with the same name are typed alike
   * - ops either have a variable type (in which new generic instances are needed)
   *   or they have a fixed, non-generic type (in which case this is the right one)
   */
  def infer(expr: Expr, scope: List[Type]): (Type, Typing) = expr match {
    case Bound(index) =>
      (scope(index), Typing.empty)

    case Free(name, typ) =>
      (typ, Typing.empty)

    case Op(name, a: Alpha) =>
      val ts = sig.types(name)
      val gts = ???
      val ty = Typing.choose(a, gts)
      (a, ty)

    case Op(name, typ) =>
      (typ, Typing.empty)

    case App(fun, arg) =>
      val rt = Alpha.fresh()
      val (ft, fty) = infer(fun, scope)
      val (at, aty) = infer(arg, scope)
      val ty = fty compose aty
      if(ty.hasFailed) sys.error("cannot apply " + fun + " to " + arg)
      val rty = ty unify (ft, Function(at, rt))
      if(rty.hasFailed) sys.error("not a function " + fun)
      (rt, rty)

    case Lambda(bound, body) =>
      val at = bound.typ
      val (bt, ty) = infer(body, at :: scope)
      (Function(at, bt), ty)
  }
}
*/