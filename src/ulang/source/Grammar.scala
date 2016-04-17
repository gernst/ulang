package ulang.source

import arse._
import arse.control._
import arse.Combinators.parse
import scala.util.DynamicVariable

object Grammar {
  object context extends DynamicVariable(Syntax.default) {
    def syntax = value
    def syntax_=(other: Syntax) { value = other }
  }

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

    def syntax = context.syntax

    def app(op: Op, args: List[Expr]) = App(op, args)

    def op(name: String) = Id(name)
    def unary(op: Op, arg: Expr) = app(op, List(arg))
    def binary(op: Op, arg1: Expr, arg2: Expr) = app(op, List(arg1, arg2))
    def abs(ids: List[Id], body: Expr) = Lambda(ids, body)

    val keywords = Parsers.keywords

    val binding = parse(abs _)(ids <~ lit("."), mixfix_expr)
    val lambda = lit("λ") ~> binding
    val bindfix_app = parse(unary _)(bindfix_op, binding)

    val anyop = parse(Id)(name)
    val closed = parens(mixfix_expr | anyop) | lambda | bindfix_app | id

    val normal_app = parse(app _)(closed, closed.*)
    val inner_expr: Parser[String, Expr] = normal_app

    val parser = mixfix_expr
  }

  object types extends Parsers with SyntaxParsers with Mixfix {
    type Op = String
    type Expr = Type

    def syntax = context.syntax

    def op(name: String) = name
    def unary(op: String, arg: Type) = app(op, List(arg))
    def binary(op: String, arg1: Type, arg2: Type) = app(op, List(arg1, arg2))
    def app(op: Op, args: List[Type]) = {
      if (args.isEmpty) Id(op)
      else TypeApp(op, args)
    }

    val keywords = Parsers.keywords ++ Set("|", "=")

    val closed = parens(mixfix_expr) | id
    val type_app = parse(app _)(nonmixfix, closed.*)

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

    def in(thy: ulang.syntax.Thy) = {
      context.syntax ++= thy.exported.syntax
      Import(thy)
    }

    val thy = string map Parsers.thy
    val strict_thy = thy ! "expected module name"
    val imprt = lit("import") ~> parse(in _)(strict_thy)

    def fix(fixity: Fixity, name: String) = {
      context.syntax += (name, fixity)
      FixDecl(fixity, name)
    }

    val fixdecl = parse(fix _)(fixities.parser, nonkw)

    val opdecl = parse(OpDecl)(exprs.name, colon_typ)
    val strict_opdecl = opdecl ! "expected operator declaration"

    val constrdecls = repsep(strict_opdecl, lit("|"))
    val eq_constrdecls = lit("=") ~> constrdecls

    // TODO: fails silently when no eq is given 
    val datadef = lit("data") ~> parse(DataDef)(strict_typ, eq_constrdecls)
    val typedef = lit("type") ~> parse(TypeDef)(strict_typ, eq_typ)
    val opdef = parse(OpDef)(exprs.mixfix_expr)

    val parser = imprt | fixdecl | datadef | typedef | opdecl | opdef
  }
}