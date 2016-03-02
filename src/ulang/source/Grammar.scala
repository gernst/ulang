package ulang.source

import arse._
import arse.control._
import arse.Combinators.parse
import ulang.syntax._

object FixityParsers extends Parsers {
  val left = lit("left", Left)
  val right = lit("right", Right)
  val non = ret(Non)

  val assoc = left | right | non

  val prefix = lit("prefix") ~> parse(Prefix)
  val postfix = lit("postfix") ~> parse(Postfix)
  val infix = lit("infix") ~> parse(Infix)(assoc, int)
  val bindfix = lit("binder", Bindfix)

  val fixity = prefix | postfix | infix | bindfix
}

case class ExprParsers(syntax: Syntax, sig: Sig) extends Parsers with SyntaxParsers with Mixfix {
  type Op = ulang.syntax.Expr
  type Expr = ulang.syntax.Expr

  def op(name: String) = FreeVar(name)
  def app(op: Op, args: List[Expr]) = App(op, args)
  def unary(op: Op, arg: Expr) = app(op, List(arg))
  def binary(op: Op, arg1: Expr, arg2: Expr) = app(op, List(arg1, arg2))
  def abs(bound: List[FreeVar], body: Expr) = Lambda(bound, body bind bound)

  val keywords = Parsers.keywords

  val free_var = name collect {
    case name if (!(syntax contains name)) =>
      FreeVar(name)
  }

  val free_vars = free_var.+

  val binding = parse(abs _)(free_vars <~ lit("."), mixfix_expr)

  val lambda = lit("λ") ~> binding
  val bindfix_app = parse(unary _)(bindfix_op, binding)

  val arg = parens(mixfix_expr) | free_var
  val closed = parens(mixfix_expr) | lambda | bindfix_app | free_var

  val normal_app = parse(app _)(closed, closed.*)
  val inner_expr: Parser[String, Expr] = normal_app

  def lift_ops(expr: Expr): Expr = expr mapFree {
    case fv @ FreeVar(name, _) =>
      if (sig.ops contains name) Op(name)
      else fv
  }

  val parser = mixfix_expr map lift_ops
}

case class SchemaParsers(syntax: Syntax) extends Parsers with SyntaxParsers {
  type Op = String

  def op(name: String) = name
  def unary(op: String, arg: TypeParam) = app(op, List(arg))
  def binary(op: String, arg1: TypeParam, arg2: TypeParam) = app(op, List(arg1, arg2))
  def app(op: Op, args: List[TypeParam]) = Schema(op, args)
  val keywords = Parsers.keywords ++ Set("|", "=")

  val param = parse(TypeParam)(name)
  val prefix = (prefix_op ~ param) map { case ((op, _), arg) => unary(op, arg) }
  val postfix = (param ~ postfix_op) map { case (arg, (op, _)) => unary(op, arg) }
  val infix = (param ~ infix_op ~ param) map { case ((arg1, (op, _)), arg2) => binary(op, arg1, arg2) }
  val normal = parse(app _)(name, param.*)

  val parser = prefix | postfix | infix | normal
}

case class TypeParsers(syntax: Syntax, sig: Sig) extends Parsers with SyntaxParsers with Mixfix {
  type Op = String
  type Expr = Type

  def op(name: String) = name
  def unary(op: String, arg: Type) = app(op, List(arg))
  def binary(op: String, arg1: Type, arg2: Type) = app(op, List(arg1, arg2))
  def app(op: Op, args: List[Type]) = TypeApp(op, args)

  val keywords = Parsers.keywords ++ Set("|", "=")

  val con = name filter sig.cons.contains
  val param = name filterNot sig.cons.contains map TypeParam

  val closed = parens(mixfix_expr) | param
  val type_app = parse(app _)(con, closed.*)

  val inner_expr: Parser[String, Type] = type_app | closed

  val parser = mixfix_expr
}

case class DeclParsers(syntax: Syntax, sig: Sig) extends Parsers {
  import Parsers._
  import FixityParsers.fixity

  val schema = SchemaParsers(syntax)
  val typ = TypeParsers(syntax, sig)
  val expr = ExprParsers(syntax, sig)

  val strict_schema = schema.parser ! "expected schema"
  val strict_typ = typ.parser ! "expected type"
  val strict_expr = expr.parser ! "expected expression"

  val colon_typ = lit(":") ~> strict_typ
  val eq_typ = lit("=") ~> strict_typ
  val eq_expr = lit("=") ~> strict_expr

  val con = strict_schema map (_.con)

  val module = string map Parsers.module
  val strict_module = module ! "expected module name"
  val imprt = lit("import") ~> parse(Import)(strict_module)

  val fixdecl = parse(FixDecl)(fixity, nonkw)

  val opdecl = parse(OpDecl)(expr.name, colon_typ)
  val strict_opdecl = opdecl ! "expected operator declaration"

  val constrdecls = repsep(strict_opdecl, lit("|"))
  val eq_constrdecls = lit("=") ~> constrdecls

  val typedecl = (lit("data") | lit("type")) ~> parse(TypeDecl)(con)

  val datadef = lit("data") ~> parse(DataDef)(strict_schema, eq_constrdecls)
  val typedef = lit("type") ~> parse(TypeDef)(strict_schema, eq_typ)

  def _opdef(name: String, args: List[Expr], rhs: Expr): OpDef = {
    val typ: Type = TypeVar.fresh // ??? // TODO: infer and check type
    OpDef(Op(name, typ), args, rhs)
  }

  val opdef = parse(_opdef _)(expr.name, expr.arg.*, eq_expr)

  val parser = imprt | fixdecl | datadef | typedef | opdecl | opdef
}