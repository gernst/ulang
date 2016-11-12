package ulang

import arse._
import ulang.syntax._

object Operators extends Syntax[String] {
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
    "×" -> (Right, 5),
    "∧" -> (Left, 4),
    "∨" -> (Left, 3),
    "→" -> (Right, 2),
    "↔" -> (Non, 1),
    "," -> (Right, 0))

  val bindfix_ops: Set[String] = Set(
    "∃",
    "∀",
    "ε")
}