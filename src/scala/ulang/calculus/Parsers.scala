package ulang.calculus

import arse._
import ulang._
import ulang.syntax._
import ulang.expr.FreeVar

class Parsers(thy: Thy) extends ulang.syntax.Parsers(thy) {
  val cut = "cut" ~ Cut.from(formula)
  
  val simplify = "simplify" ~ ret[List[String], Rule](Simplify)
  
  val structural = "induction" ~ expr map {
    case x: FreeVar =>
      Structural(thy, x)
  }

  def rule = simplify | cut | structural
}