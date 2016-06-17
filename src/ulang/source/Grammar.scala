package ulang.source

import scala.util.DynamicVariable

import arse._
import ulang._
import ulang.syntax._

object Grammar {
  import Parser._
  import Recognizer._
  import Mixfix._

  import Operators._

  def expect(s: String) = s ! "expected '" + s + "'"
  def parens[A](p: Parser[String, A]) = "(" ~ p ~ expect(")")

  val name = __ filterNot Parsers.keywords
  val nonmixfix = name filterNot Operators.contains

  val expr: Parser[String, Expr] = mixfix(expr_app, FreeVar, Apps, Operators)

  val fv = FreeVar.from(nonmixfix)
  val fvs = fv.+
  val keywords = Parsers.keywords

  val binding = Lambdas.from(fvs ~ expect("."), expr)
  val lambda = "λ" ~ binding
  val bindfix_app = App.from(bindfix_op, binding)

  val anyfv = FreeVar.from(name)
  val expr_closed = parens(expr | anyfv) | lambda | bindfix_app | fv

  val expr_app = Apps.from(expr_closed, expr_closed.*)

  val typ: Parser[String, Type] = mixfix(type_app, id _, TypeApp, Operators)

  val tv = TypeVar.from(nonmixfix)
  val tvs = tv.+

  val type_closed = parens(typ) | tv
  val type_app = TypeApp.from(nonmixfix, type_closed.*) | type_closed

  val strict_typ = typ ! "expected type"
  val strict_expr = expr ! "expected expression"

  val colon_typ = ":" ~ strict_typ
  val eq_typ = "=" ~ strict_typ
  val eq_expr = "=" ~ strict_expr

  val thy = string map Parsers.thy
  val strict_thy = thy ! "expected module name"
  val imprt = "import" ~ Import.from(strict_thy)

  val opdecl = OpDecl.from(name, colon_typ)
  val strict_opdecl = opdecl ! "expected operator declaration"

  val constrdecls = strict_opdecl sep "|"
  val eq_constrdecls = expect("=") ~ constrdecls

  // TODO: fails silently when no eq is given 
  val datadef = "data" ~ DataDef.from(strict_typ, eq_constrdecls)
  val typedef = "type" ~ TypeDef.from(strict_typ, eq_typ)
  val opdef = OpDef.from(expr)

  val decl = imprt | datadef | typedef | opdecl | opdef
}