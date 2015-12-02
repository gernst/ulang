package ulang.syntax.predefined

import ulang.syntax._

object pred {
  import Type._

  // val lambda = Op("λ", beta → (alpha → beta)) // fake op to facilitate parsing

  val all = Op("∀", (alpha → bool) → bool)
  val ex = Op("∃", (alpha → bool) → bool)

  object Eq extends Binary(Op.equals)
  object IfThenElse extends Ternary(Op.if_then_else)

  object All extends Binder(all)
  object Ex extends Binder(ex)

  object Function {
    def apply(t1: Type, t2: Type) = TypeApp("→", List(t1, t2))

    def unapply(t: Type): Option[(Type, Type)] = t match {
      case TypeApp("→", List(t1, t2)) =>
        Some((t1, t2))
      case _ =>
        None
    }
  }

  /*
  val pred_syntax = Syntax(
    Set("id"),
    Map(),
    Map(),
    Map("=" → ((6, Non))),
    Set("∀", "∃"))
    */

}