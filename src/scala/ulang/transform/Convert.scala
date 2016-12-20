package ulang.transform

import ulang.source
import ulang.syntax
import arse._

object Convert {
  import Unify._
/*
  def typ(sig: syntax.Sig, t: source.Type): syntax.Type = t match {
    case source.Id(name) =>
      if (sig contains_con (name, 0))
        syntax.TypeApp(name)
      else
        syntax.TypeParam(name)

    case source.TypeApp(name, args) if sig contains_con (name, args.length) =>
      syntax.TypeApp(name, args map (typ(sig, _)))

    /* case source.TypeVar(index) =>
      syntax.TypeParam("?" + index) */

    case source.TypeInst(orig) =>
      orig

    case _ =>
      error("in convert: not a proper type " + t)
  }

  def subst(sig: syntax.Sig, t: source.Type, theta: Subst): syntax.Type = t match {
    case _: source.TypeVar =>
      val r = theta.find(t)
      if (r == t) typ(sig, t)
      else subst(sig, r, theta)

    case source.TypeApp(name, args) =>
      syntax.TypeApp(name, args map (subst(sig, _, theta)))

    case _ =>
      typ(sig, t)
  }

  def expr_infer(infer: Infer, e: source.Expr, scope: Option[Map[String, source.Type]]): (syntax.Expr, syntax.Type) = {
    val thetas = if (scope.isEmpty)
      infer.infer_def(e)
    else
      infer.infer(e, scope.get.toMap)

    val exprs = thetas map {
      theta =>
        (expr(infer.sig, e, Nil, theta), subst(infer.sig, e.typ, theta))
    }

    exprs.distinct match {
      case Nil =>
        error("in infer: no type for " + e)
      case List(et) =>
        et
      case _ =>
        val ts = exprs map (_._2 )
        error("in infer: ambiguous type for " + e + " in " + ts.mkString(", "))
    }
  }

  def expr_infer_top(sig: syntax.Sig, df: syntax.Defs, e: source.Expr, free: List[syntax.FreeVar]) = {
    val infer = Infer(sig, df)
    val scope = free map {
      case syntax.FreeVar(name, typ) =>
        (name, infer.nongeneric(typ))
    }
    expr_infer(infer, e, Some(scope.toMap))
  }

  def expr_infer_def(sig: syntax.Sig, df: syntax.Defs, e: source.Expr) = {
    val infer = Infer(sig, df)
    expr_infer(infer, e, None)
  }

  def expr(sig: syntax.Sig, e: source.Expr, stack: List[syntax.FreeVar], theta: Subst): syntax.Expr = e match {
    case e @ source.Id(name) =>
      val pos = stack.indexWhere(_.name == name)
      if (pos >= 0) {
        syntax.BoundVar(pos)
      } else if (sig contains_op name) {
        val typ = subst(sig, e.orig, theta)
        syntax.Op(name, typ)
      } else {
        val typ = subst(sig, e.typ, theta)
        syntax.FreeVar(name, typ)
      }
      
    case source.Typed(arg, _) =>
      expr(sig, arg, stack, theta)

    case source.App(fun, arg) =>
      syntax.App(expr(sig, fun, stack, theta), expr(sig, arg, stack, theta))

    case source.Lambda(bound, body) =>
      val typ = subst(sig, bound.typ, theta)
      val fv = syntax.FreeVar(bound.name, typ)
      syntax.Lambda(fv, expr(sig, body, fv :: stack, theta))
  }

  def con(t: source.Type): syntax.Con = t match {
    case source.Id(name) =>
      syntax.Con(name, 0)

    case source.TypeApp(name, args) =>
      syntax.Con(name, args.length)

    case _ =>
      error("in convert: not a type constructor " + t)
  }

  def schema(sig: syntax.Sig, t: source.Type): syntax.Schema = typ(sig, t) match {
    case syntax.TypeApp(name, args) if args forall (_.isInstanceOf[syntax.TypeParam]) =>
      syntax.Schema(name, args.asInstanceOf[List[syntax.TypeParam]])

    case s =>
      error("in convert: not a type schema " + s)
  }

  def decl(thy: syntax.Thy, decl: source.Decl): syntax.Thy = {
    decls(thy, List(decl))
  }

  type Pass = (syntax.Thy, syntax.Sig, source.Decl) => syntax.Thy
  val passes: List[Pass] = List(imports, fixities, type_sig, op_sig, type_def, op_def)

  def decls(thy: syntax.Thy, decls: List[source.Decl]): syntax.Thy = {
    passes.foldLeft(thy) {
      case (thy, pass) =>
        val sig = thy.sig
        decls.foldLeft(thy) {
          case (thy, decl) =>
            try {
              pass(thy, sig, decl)
            } catch {
              case e: Exception =>
                error("in convert: invalid declaration " + decl + "\nreason: " + e)
                throw e
            }
        }
    }
  }

  def imports(thy: syntax.Thy, sig: syntax.Sig, decl: source.Decl): syntax.Thy = decl match {
    case source.Import(in) =>
      thy + in
    case _ =>
      thy
  }

  def fixities(thy: syntax.Thy, sig: syntax.Sig, decl: source.Decl): syntax.Thy = decl match {
    case source.FixDecl(fixity, name) =>
      thy + (name, fixity)

    case _ =>
      thy
  }

  def type_sig(thy: syntax.Thy, sig: syntax.Sig, decl: source.Decl): syntax.Thy = decl match {
    case source.TypeDecl(lhs) =>
      thy + con(lhs)

    case _ =>
      thy
  }

  def op_sig(thy: syntax.Thy, sig: syntax.Sig, decl: source.Decl): syntax.Thy = decl match {
    case source.OpDecl(name, rhs) =>
      thy + syntax.Op(name, typ(thy.sig, rhs))

    case source.DataDef(lhs, constrs) =>
      constrs.foldLeft(thy) {
        case (thy, decl) =>
          op_sig(thy, sig, decl)
      }

    case _ =>
      thy
  }

  def type_def(thy: syntax.Thy, sig: syntax.Sig, decl: source.Decl): syntax.Thy = decl match {
    case source.TypeDef(lhs, rhs) =>
      val s = schema(thy.sig, lhs)
      thy + (s.con, s.args, typ(thy.sig, rhs))

    case _ =>
      thy
  }

  def op_def(thy: syntax.Thy, sig: syntax.Sig, decl: source.Decl): syntax.Thy = decl match {
    case source.OpDef(axiom) =>
      val (expr, typ) = expr_infer_def(thy.sig, thy.df, axiom)
      if (!typ.isBool)
        error("in convert: axioms " + expr + " has type " + typ)
      thy + expr

    case _ =>
      thy
  }*/
}