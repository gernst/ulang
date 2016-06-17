package ulang.source

object Symbols extends (String => String) {
  val map = Map(
    "not" -> "¬",
    "and" -> "∧",
    "or" -> "∨",
    // "implies" -> "→",
    "->" -> "→",
    "<->" -> "↔",
    "forall" -> "∀",
    "exists" -> "∃",
    "lambda" -> "λ",
    "|-" -> "⊦")

  // called from the parser
  def apply(s: String) = map.getOrElse(s, s)
}