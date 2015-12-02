package ulang.syntax.predefined

import ulang.syntax._

object prop {
  import Type.bool
  
  val True = Op("true", bool)
  val False = Op("false", bool)

  val not = Op("¬", bool → bool)
  val and = Op("∧", bool → (bool → bool))
  val or = Op("∨", bool → (bool → bool))
  val imp = Op("→", bool → (bool → bool))
  val eqv = Op("↔", bool → (bool → bool))
  /*
  val prop_syntax = Syntax(
    Set("true", "false"),
    Map("¬" → 5),
    Map(),
    Map("∧" → (4, Left), "∨" → (3, Left), "→" → (2, Right), "↔" → (1, Non)),
    Set())
    */

  object Not extends Unary(not)
  object And extends Binary(and)
  object Or extends Binary(or)
  object Imp extends Binary(imp)
  object Eqv extends Binary(eqv)

  object Con extends Nary(and, True)
  object Dis extends Nary(or, False)
}