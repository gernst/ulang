package ulang.source

import arse.Combinators.parse
import arse.Infix
import arse.Left
import arse.Non
import arse.Parser
import arse.Postfix
import arse.Prefix
import arse.Right
import ulang.syntax.Bindfix

object Parsers extends ulang.Parsers {
  val keywords = Set(
    "import", "data", "type",
    "prefix", "infix", "postfix", "binder",
    "(", ")", ";")

  val typ_keywords = keywords ++ Set("|", "=", "\\", ".")
  val expr_keywords = keywords
  
  def name(kw: Set[String]) = string filterNot kw map Symbols

  val left = lit("left", Left)
  val right = lit("right", Right)
  val non = ret(Non)

  val assoc = left | right | non

  val prefix = lit("prefix") ~> parse(Prefix)
  val postfix = lit("postfix") ~> parse(Postfix)
  val infix = lit("infix") ~> parse(Infix)(assoc, int)
  val bindfix = lit("binder", Bindfix)

  val fixity = prefix | postfix | infix | bindfix

  abstract class tree(kw: Set[String], what: String) {
    val name = Parsers.name(kw)
    val id = parse(Id)(name)
    val closed = parens(rec(strict_tree)) | id
    val tree: Parser[String, Tree] = parse(Node)(closed +)
    val strict_tree = tree ! "expected " + what
  }

  object typ extends tree(typ_keywords, "type")
  object expr extends tree(expr_keywords, "expr")

  val imprt = lit("import") ~> parse(Import)

  val colon_typ = lit(":") ~> typ.strict_tree
  val eq_typ = lit("=") ~> typ.strict_tree

  val opdecl = parse(OpDecl)(string, colon_typ)
  val strict_opdecl = opdecl ! "expected operator declaration"

  // val constrdecl = lit("|") ~> strict_opdecl
  val eq_constrdecls = lit("=") ~> repsep(strict_opdecl, lit("|"))

  val datadef = lit("data") ~> parse(DataDef)(typ.strict_tree, eq_constrdecls)
  val typedef = lit("type") ~> parse(TypeDef)(typ.strict_tree, eq_typ ?)

  val fixdecl = parse(FixDecl)(fixity, expr.name)

  val opdef = parse(OpDef)(expr.tree)

  val decl = imprt | datadef | typedef | fixdecl | opdecl | opdef
  
  val strict_semi = expect(";") <~ lit(";").*
  val eod = eof | strict_semi
  val decl_ = decl <~ eod

  val decls_ = decl_ *
}
