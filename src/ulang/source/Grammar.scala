package ulang.source

import arse._
import arse.control._
import arse.Combinators.parse
import scala.util.DynamicVariable

class Grammar(var syntax: Syntax) {
  def grammar = this

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

    def syntax = grammar.syntax

    def app(op: Op, args: List[Expr]) = App(op, args)

    def op(name: String) = Id(name)
    def unary(op: Op, arg: Expr) = app(op, List(arg))
    def binary(op: Op, arg1: Expr, arg2: Expr) = app(op, List(arg1, arg2))
    def abs(ids: List[Id], body: Expr) = Lambda(ids, body)

    val keywords = Parsers.keywords

    val free_var = name filterNot (syntax.contains) map Id
    val free_vars = free_var.+

    val binding = parse(abs _)(free_vars <~ lit("."), mixfix_expr)

    val lambda = lit("λ") ~> binding
    val bindfix_app = parse(unary _)(bindfix_op, binding)

    val arg = parens(mixfix_expr) | free_var
    val args = arg.*
    val closed = parens(mixfix_expr) | lambda | bindfix_app | free_var

    val normal_app = parse(app _)(closed, closed.*)
    val inner_expr: Parser[String, Expr] = normal_app

    def infer(expr: ulang.source.Expr): ulang.syntax.Expr = ???

    val parser = mixfix_expr map infer
  }

  object types extends Parsers with SyntaxParsers with Mixfix {
    type Op = String
    type Expr = Type

    def syntax = grammar.syntax

    def op(name: String) = name
    def unary(op: String, arg: Type) = app(op, List(arg))
    def binary(op: String, arg1: Type, arg2: Type) = app(op, List(arg1, arg2))
    def app(op: Op, args: List[Type]) = TypeApp(op, args)

    val keywords = Parsers.keywords ++ Set("|", "=")

    val param = parse(Id)(name)

    val closed = parens(mixfix_expr) | param
    val type_app = parse(app _)(name, closed.*)

    val inner_expr: Parser[String, Type] = type_app | closed

    val parser = mixfix_expr
  }

  object decls extends Parsers {
    import Parsers._

    val strict_typ = types.parser ! "expected type"
    val strict_expr = exprs.parser ! "expected expression"

    val colon_typ = lit(":") ~> strict_typ
    val eq_typ = lit("=") ~> strict_typ
    val eq_expr = lit("=") ~> strict_expr

    val mod = string map Parsers.mod
    val strict_mod = mod ! "expected module name"
    val imprt = lit("import") ~> parse(Import)(strict_mod)

    def fix(fixity: Fixity, name: String) = {
      syntax += (name, fixity)
      FixDecl(fixity, name)
    }

    val fixdecl = parse(fix _)(fixities.parser, nonkw)

    val opdecl = parse(OpDecl)(exprs.name, colon_typ)
    val strict_opdecl = opdecl ! "expected operator declaration"

    val constrdecls = repsep(strict_opdecl, lit("|"))
    val eq_constrdecls = lit("=") ~> constrdecls

    val datadef = lit("data") ~> parse(DataDef)(strict_typ, eq_constrdecls)
    val typedef = lit("type") ~> parse(TypeDef)(strict_typ, eq_typ)
    val opdef = parse(OpDef)(exprs.mixfix_expr)

    val parser = imprt | fixdecl | datadef | typedef | opdecl | opdef
  }
}