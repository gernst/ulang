package ulang.source

import arse._
import arse.control._
import arse.Combinators.parse
import ulang.syntax.Thy
import ulang.syntax.Bindfix
import ulang.transform.Infer
import scala.util.DynamicVariable

case class Grammar(thy: Thy) {
  def syntax = thy.syntax
  def sig = thy.sig
  def df = thy.df

  object fixities extends Parsers {
    val left = lit("left", Left)
    val right = lit("right", Right)
    val non = ret(Non)

    val assoc = left | right | non

    val prefix = lit("prefix") ~> parse(Prefix)
    val postfix = lit("postfix") ~> parse(Postfix)
    val infix = lit("infix") ~> parse(Infix)(assoc, int)
    val bindfix = lit("binder", Bindfix)

    val parser = prefix | postfix | infix | bindfix
  }

  object exprs extends Parsers with SyntaxParsers with Mixfix {
    type Op = ulang.source.Expr
    type Expr = ulang.source.Expr

    def syntax = thy.syntax

    def app(op: Op, args: List[Expr]) = App(op, args)

    def op(name: String) = Id(name)
    def unary(op: Op, arg: Expr) = app(op, List(arg))
    def binary(op: Op, arg1: Expr, arg2: Expr) = app(op, List(arg1, arg2))
    def abs(bound: List[Id], body: Expr) = Lambda(bound, body)

    val keywords = Parsers.keywords

    val free_var = parse(Id)(name filterNot (syntax.contains))
    val free_vars = free_var.+

    val binding = parse(abs _)(free_vars <~ lit("."), mixfix_expr)

    val lambda = lit("λ") ~> binding
    val bindfix_app = parse(unary _)(bindfix_op, binding)

    val arg = parens(mixfix_expr) | free_var
    val args = arg.*
    val closed = parens(mixfix_expr) | lambda | bindfix_app | free_var

    val normal_app = parse(app _)(closed, closed.*)
    val inner_expr: Parser[String, Expr] = normal_app

    def parser = mixfix_expr
  }

  object schemas extends Parsers with SyntaxParsers {
    type Op = String

    def syntax = thy.syntax

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

  object types extends Parsers with SyntaxParsers with Mixfix {
    type Op = String
    type Expr = Type

    def syntax = thy.syntax

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

  object decls extends Parsers {
    import Parsers._

    val infer = new Infer(sig, df)

    val strict_schema = schemas.parser ! "expected schema"
    val strict_typ = types.parser ! "expected type"
    val strict_expr = exprs.parser ! "expected expression"

    val colon_typ = lit(":") ~> strict_typ
    val eq_typ = lit("=") ~> strict_typ
    val eq_expr = lit("=") ~> strict_expr

    val con = strict_schema map (_.con)

    val module = string map Parsers.module
    val strict_module = module ! "expected module name"
    val imprt = lit("import") ~> parse(Import)(strict_module)

    val fixdecl = parse(FixDecl)(fixities.parser, nonkw)

    val opdecl = parse(OpDecl)(exprs.name, ??? /*colon_typ*/)
    val strict_opdecl = opdecl ! "expected operator declaration"

    val constrdecls = repsep(strict_opdecl, lit("|"))
    val eq_constrdecls = lit("=") ~> constrdecls

    val typedecl = (lit("data") | lit("type")) ~> parse(TypeDecl)(con)

    val datadef = lit("data") ~> parse(DataDef)(strict_schema, eq_constrdecls)
    val typedef = lit("type") ~> parse(TypeDef)(strict_schema, eq_typ)

    /*def _opdef(name: String, _args: List[Expr], _rhs: Expr): OpDef = {
      import ulang.syntax.predefined.pred.Eq
      val res = infer.infer_defs(name, _args, _rhs)
      val (Eq(FlatApp(op, args), rhs), _) = res
      OpDef(op, args, rhs)
    }*/

    val opdef = parse(OpDef)(exprs.name, exprs.args, eq_expr)

    val parser = imprt | fixdecl | datadef | typedef | opdecl | opdef
  }
}