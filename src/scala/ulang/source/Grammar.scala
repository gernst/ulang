package ulang.source

import scala.language.implicitConversions

import arse._
import ulang.syntax.Sig

object Grammar {
  import Parser._
  import Recognizer._
  import Mixfix._

  def expect(s: String) = s ! "expected '" + s + "'"
  def parens[A](p: Parser[List[String], A]) = "(" ~ p ~ expect(")")
  def list[A](p: Parser[List[String], A]) = p map (List(_))

  val sections = Set("data", "type", "definition", "axiomatization", "lemma")

  val expr: Parser[List[String], Expr] = {
    import Ops._

    val id = Id.from(nonmixfix)
    val ids = id +
    val anyid = Id.from(name)

    val top = Parser.rec(expr)
    val binding = Lambda.from(ids ~ ".", top)
    val lambda = "λ" ~ binding
    val bindfix_app = ExprApp.from(bindfix_op, list(binding))
    val closed = parens(top | anyid) | lambda | bindfix_app | id
    val app = ExprApp.from(closed, closed.*)

    mixfix(app, Id, ExprApp, Ops)
  }

  val typ: Parser[List[String], Type] = {
    import Cons._

    val id = Id.from(nonmixfix)
    val top = Parser.rec(typ)
    val closed = parens(top) | id
    val app = TypeApp.from(nonmixfix, closed *) | closed

    mixfix(app, (s: String) => s, TypeApp, Cons)
  }

  val opttypes = ":" ~ (typ +) | ret(Nil)

  val file = string filterNot Set(";")
  val imports = "import" ~ Imports.from(file +) ~ ";"

  val constr = Constr.from(Ops.name, opttypes)
  val constrs = constr rep (sep = "|")

  val data = typ ~ "=" ~ constrs ~ ";"
  val typd = typ ~ "=" ~ typ ~ ";"

  val datas = "data" ~ Datas.from(data *)
  val types = "type" ~ Types.from(typd *)

  val op = Op.from(Ops.name, ":" ~ typ) ~ ";"
  val ax = expr ~ ";"
  val axs = ax *

  val opdefs = "definition" ~ Defs.from(op *, "where" ~ axs)
  val axdefs = "axiomatization" ~ Axioms.from(op *, "where" ~ axs)

  val decl = imports | types | datas | opdefs | axdefs
  val module = Module.from(decl *)
}

object Ops extends Syntax[String] {
  import Recognizer._
  import Parser._

  val keywords = Grammar.sections ++ Set("(", ")", ";", "λ", ".")
  val name = __ filterNot keywords
  val nonmixfix = name filterNot contains

  override def contains(s: String) =
    (prefix_ops contains s) ||
      (postfix_ops contains s) ||
      (infix_ops contains s) ||
      (bindfix_ops contains s)

  val prefix_ops: Map[String, Int] = Map(
    "#" -> 11,
    "-" -> 10,
    "¬" -> 5)

  val postfix_ops: Map[String, Int] = Map(
    "+1" -> 11,
    "-1" -> 11,
    "*" -> 12)

  val infix_ops: Map[String, (Assoc, Int)] = Map(
    "·" -> (Right, 20),
    "*" -> (Left, 9),
    "/" -> (Left, 9),
    "+" -> (Left, 8),
    "-" -> (Left, 8),
    "::" -> (Right, 8),
    "++" -> (Right, 7),
    "≠" -> (Non, 6),
    "=" -> (Non, 6),
    "≤" -> (Non, 6),
    "≥" -> (Non, 6),
    "<" -> (Non, 6),
    ">" -> (Non, 6),
    "∈" -> (Non, 5),
    "∧" -> (Left, 4),
    "∨" -> (Left, 3),
    "→" -> (Right, 2),
    "↔" -> (Non, 1),
    "," -> (Right, 0))

  val bindfix_ops: Set[String] = Set(
    "∃",
    "∀",
    "ε")

  val bindfix_op = Mixfix.mixfix_op(bindfix_ops, Id)
}

object Cons extends Syntax[String] {
  import Recognizer._
  import Parser._

  val keywords = Grammar.sections ++ Set("(", ")", ";", "=", "|")
  val name = __ filterNot keywords
  val nonmixfix = name filterNot contains

  val prefix_ops: Map[String, Int] = Map()
  val postfix_ops: Map[String, Int] = Map()

  val infix_ops: Map[String, (Assoc, Int)] = Map(
    "×" -> (Right, 3),
    "→" -> (Right, 2))
}