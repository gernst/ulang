package ulang

import ulang.typ._

package object expr {
  type Value = Any
  type Function = (Value => Value)
  
  type Expr = Term[(String, Type)]
  type Op = Free[(String, Type)]
  type FreeVar = Free[(String, Type)]
  
  def Op(name: String, typ: Type) = {
    Free((name, typ))
  }

  val equals = Op("=", alpha → (alpha → bool))

  val True = Op("true", bool)
  val False = Op("false", bool)

  val not = Op("¬", bool → bool)
  val and = Op("∧", bool → (bool → bool))
  val or = Op("∨", bool → (bool → bool))
  val imp = Op("→", bool → (bool → bool))
  val eqv = Op("↔", bool → (bool → bool))

  val all = Op("∀", (alpha → bool) → bool)
  val ex = Op("∃", (alpha → bool) → bool)

  val if_then_else = Op("if_then_else", bool → (alpha → (alpha → alpha)))

  object Not extends Unary(not)
  object And extends Binary(and)
  object Or extends Binary(or)
  object Imp extends Binary(imp)
  object Eqv extends Binary(eqv)

  object Ands extends Nary(and, True)
  object Ors extends Nary(or, False)

  object Eq extends Binary(equals)
  object IfThenElse extends Ternary(if_then_else)

  object All extends Binder(all)
  object Ex extends Binder(ex)

  implicit class SubstOps[A, B](s: A => Term[A]) {
    def o(t: A => Term[A]) = (a: A) => s(a) subst t
  }
}