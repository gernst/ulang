package ulang.calculus

import arse._
import arse.Combinators._
import ulang.Context
import ulang.syntax._

object Parsers extends ulang.source.Parsers {
  import Basic._
  val basic_rules = rules.map(r => (r.name, r)).toMap

  def expr(ctx: Context): Parser[String, (Expr, Type)] = ??? // ulang.source.Parsers.expr.strict_tree map (ctx toExpr _)

  def formula(ctx: Context) = expr(ctx) map {
    case (phi, typ) =>
      if (typ != Type.bool)
        error("in prove: not a formula (type " + typ + ")")

      phi
  }

  val basic_rule = __ collect {
    case name if basic_rules contains name =>
      basic_rules(name)
  }

  val rotate = lit("rotate") ~> parse(Rotate)(int)
  def cut(ctx: Context) = lit("cut") ~> parse(Cut)(??? /*formula(ctx)*/)
  val simplify = lit("simplify") ~> ret(Simplify)
  def structural(ctx: Context): Parser[String, Rule] = ??? /*lit("structural induction") ~> expr(ctx) map {
    case (x: FreeVar, typ) =>
      Structural(ctx, x)
  }*/

  def rule(ctx: Context) = basic_rule | rotate | simplify | cut(ctx) | structural(ctx)
}